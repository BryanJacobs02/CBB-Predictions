"""Build the Lockbot 2.0 modeling table: one row per D1 game, point-in-time features only.

Leakage rules (the things Lockbot 1.0 got wrong or risked):
  * ratings come from the latest KenPom snapshot strictly BEFORE game day, same season
  * early-season games (before the first snapshot) fall back to LAST season's final ratings
  * never used: Seed, Event, *Final columns, or any end-of-season table for the same season

Usage:
  python3 -I lockbot2/build_dataset.py --export-dir <lockbot_export> \
      --odds <odds_2026.csv> --out lockbot2/out/games_model.csv
"""
import argparse
import sys
from pathlib import Path

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).parent))
import team_match as tm  # noqa: E402

RATING_COLS = ["AdjEM", "AdjOE", "AdjDE", "AdjTempo"]


def american_to_prob(x):
    x = pd.to_numeric(x, errors="coerce")
    return np.where(x < 0, -x / (-x + 100), 100 / (x + 100))


def load_games(E, r_file):
    s = pd.read_csv(E / "schedule_extra.csv", dtype={"game_id": str, "home_id": str, "away_id": str})
    s = s[(s.status_type_completed == True) & s.home_conference_id.notna()  # noqa: E712
          & s.away_conference_id.notna() & s.home_score.notna() & s.away_score.notna()].copy()
    s["game_date"] = pd.to_datetime(s.game_date)
    kp_teams = pd.read_csv(E / "kp_teams.csv")
    maps = {}
    for yr, sd in s.groupby("season"):
        mp, d = tm.build_team_map(sd, kp_teams[kp_teams.Season == yr].TeamName.unique(), r_file)
        assert not d["conflicts"] and len(d["unmatched"]) == 0, f"team matching incomplete for {yr}"
        maps[yr] = mp
    s["kp_home"] = [maps[y][i] for y, i in zip(s.season, s.home_id)]
    s["kp_away"] = [maps[y][i] for y, i in zip(s.season, s.away_id)]
    s["margin"] = s.home_score - s.away_score
    s["total"] = s.home_score + s.away_score
    s["home_win"] = (s.margin > 0).astype(int)
    s["neutral"] = s.neutral_site.fillna(False).astype(int)
    s["conf_game"] = s.conference_competition.fillna(False).astype(int)
    s["postseason"] = (s.season_type == 3).astype(int)
    keep = ["game_id", "game_date", "season", "kp_home", "kp_away", "home_score", "away_score",
            "margin", "total", "home_win", "neutral", "conf_game", "postseason"]
    return s[keep].sort_values(["game_date", "game_id"]).reset_index(drop=True)


def add_schedule_context(g):
    """Games played so far, rest days, and recent margin for each team (from D1 results only)."""
    def side(team_col, pf, pa, tag):
        return g[["game_id", "game_date", "season", team_col, pf, pa]].rename(
            columns={team_col: "team", pf: "pf", pa: "pa"}).assign(side=tag)
    long = pd.concat([side("kp_home", "home_score", "away_score", "h"),
                      side("kp_away", "away_score", "home_score", "a")])
    long = long.sort_values(["team", "season", "game_date", "game_id"])
    grp = long.groupby(["team", "season"])
    long["gp"] = grp.cumcount()
    long["rest"] = grp.game_date.diff().dt.days.clip(upper=14)
    long["m"] = long.pf - long.pa
    long["last5"] = grp.m.transform(lambda x: x.shift().rolling(5, min_periods=1).mean())
    for tag in ("h", "a"):
        part = long[long.side == tag][["game_id", "gp", "rest", "last5"]]
        g = g.merge(part.rename(columns={c: f"{c}_{tag}" for c in ("gp", "rest", "last5")}),
                    on="game_id", how="left")
    return g


def add_league_context(g):
    """League-wide average total over the 30 days BEFORE each game (scoring environment).
    Falls back to last season's average until enough games have been played."""
    daily = g.groupby("game_date").total.agg(["sum", "count"])
    idx = pd.date_range(daily.index.min() - pd.Timedelta(days=1), daily.index.max())
    daily = daily.reindex(idx, fill_value=0)
    rs = daily["sum"].rolling(30, min_periods=1).sum().shift(1)
    rc = daily["count"].rolling(30, min_periods=1).sum().shift(1)
    lg = (rs / rc.where(rc >= 150)).rename("lg_total_30d")
    g = g.merge(lg, left_on="game_date", right_index=True, how="left")
    season_mean = g.groupby("season").total.mean()
    prior_mean = g.season.map(lambda y: season_mean.get(y - 1, np.nan))
    g["lg_total_30d"] = g.lg_total_30d.fillna(prior_mean)
    return g


def add_ratings(g, E):
    snaps = pd.read_csv(E / "kp_archive_weekly.csv", usecols=["TeamName", "season", "snap_date"] + RATING_COLS)
    snaps["snap_date"] = pd.to_datetime(snaps.snap_date)
    snaps = snaps.sort_values("snap_date")
    final = pd.read_csv(E / "kp_ratings.csv", usecols=["TeamName", "season"] + RATING_COLS)
    prior = final.rename(columns={c: f"prior_{c}" for c in RATING_COLS})
    prior["season"] = prior.season + 1          # last season's FINAL ratings, usable this season

    for tag, col in (("h", "kp_home"), ("a", "kp_away")):
        base = g[["game_id", "game_date", "season", col]].rename(columns={col: "TeamName"}).sort_values("game_date")
        now = pd.merge_asof(base, snaps, left_on="game_date", right_on="snap_date",
                            by=["TeamName", "season"], direction="backward", allow_exact_matches=False)
        now["snap_age"] = (now.game_date - now.snap_date).dt.days
        old_q = base.assign(game_date=base.game_date - pd.Timedelta(days=28))
        old = pd.merge_asof(old_q, snaps[["TeamName", "season", "snap_date", "AdjEM"]],
                            left_on="game_date", right_on="snap_date", by=["TeamName", "season"],
                            direction="backward", allow_exact_matches=False)
        now["mom"] = now.AdjEM.values - old.AdjEM.values
        now = now.drop(columns=["game_date", "season", "TeamName", "snap_date"])
        now = now.merge(base[["game_id", "season", "TeamName"]], on="game_id")
        now = now.merge(prior, on=["TeamName", "season"], how="left").drop(columns=["TeamName", "season"])
        now = now.rename(columns={c: f"{c}_{tag}" for c in now.columns if c != "game_id"})
        g = g.merge(now, on="game_id", how="left")

    # "effective" ratings: snapshot if we have one, else last season's final
    for c in RATING_COLS:
        for tag in ("h", "a"):
            g[f"{c}_{tag}_eff"] = g[f"{c}_{tag}"].fillna(g[f"prior_{c}_{tag}"])
    g["from_prior_h"] = g.AdjEM_h.isna().astype(int)
    g["from_prior_a"] = g.AdjEM_a.isna().astype(int)
    return g


def add_market(g, odds_path):
    o = pd.read_csv(odds_path, dtype=str)
    o = o[o.has_odds == "TRUE"].copy()

    def num(c):
        return pd.to_numeric(o[c].astype(str).str.replace(r"^[ou]", "", regex=True), errors="coerce")
    m = pd.DataFrame({
        "game_id": o.game_id,
        "mkt_spread": num("point_spread_home_close_line"),     # home line: negative = home favored
        "mkt_spread_open": num("point_spread_home_open_line"),
        "mkt_total": num("total_over_close_line"),
        "mkt_total_open": num("total_over_open_line"),
        "ml_home": pd.to_numeric(o.moneyline_home_close_odds.str.replace("+", "", regex=False), errors="coerce"),
        "ml_away": pd.to_numeric(o.moneyline_away_close_odds.str.replace("+", "", regex=False), errors="coerce"),
    })
    ph, pa = american_to_prob(m.ml_home), american_to_prob(m.ml_away)
    m["mkt_wp_home"] = ph / (ph + pa)                           # de-vigged
    return g.merge(m, on="game_id", how="left")


def add_kenpom_fanmatch(g, E):
    fm = pd.read_csv(E / "kp_fanmatch.csv")
    if fm.HomeWP.max() > 1.5:                   # KenPom stores win probability as a percent
        fm["HomeWP"] = fm.HomeWP / 100
    a = fm.rename(columns={"DateOfGame": "d", "Home": "kp_home", "Visitor": "kp_away",
                           "HomePred": "kp_pred_h", "VisitorPred": "kp_pred_a"})
    a["kp_wp_home"] = a.HomeWP
    b = fm.rename(columns={"DateOfGame": "d", "Visitor": "kp_home", "Home": "kp_away",
                           "VisitorPred": "kp_pred_h", "HomePred": "kp_pred_a"})
    b["kp_wp_home"] = 1 - b.HomeWP
    cols = ["d", "kp_home", "kp_away", "kp_pred_h", "kp_pred_a", "kp_wp_home"]
    both = pd.concat([a[cols], b[cols]]).drop_duplicates(["d", "kp_home", "kp_away"], keep="first")
    g["d"] = g.game_date.dt.strftime("%Y-%m-%d")
    g = g.merge(both, on=["d", "kp_home", "kp_away"], how="left").drop(columns="d")
    g["kp_margin"] = g.kp_pred_h - g.kp_pred_a
    g["kp_total_pred"] = g.kp_pred_h + g.kp_pred_a
    return g


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--export-dir", required=True)
    ap.add_argument("--odds", required=True)
    ap.add_argument("--r-file", default=str(Path(__file__).parent.parent / "R" / "game_results.R"))
    ap.add_argument("--out", required=True)
    a = ap.parse_args()
    E = Path(a.export_dir)

    g = load_games(E, a.r_file)
    g = add_schedule_context(g)
    g = add_league_context(g)
    g = add_ratings(g, E)
    g = add_market(g, a.odds)
    g = add_kenpom_fanmatch(g, E)

    Path(a.out).parent.mkdir(parents=True, exist_ok=True)
    g.to_csv(a.out, index=False)

    print(f"wrote {a.out}: {len(g)} games x {g.shape[1]} cols")
    usable = g.AdjEM_h_eff.notna() & g.AdjEM_a_eff.notna()
    print(g.assign(usable=usable, snap=(g.from_prior_h == 0) & (g.from_prior_a == 0),
                   odds=g.mkt_spread.notna(), kp=g.kp_margin.notna())
           .groupby("season")[["usable", "snap", "odds", "kp"]].agg(["sum"]).droplevel(1, axis=1)
           .assign(games=g.groupby("season").size()).to_string())
    print("games usable overall:", int(usable.sum()), "of", len(g))


if __name__ == "__main__":
    main()
