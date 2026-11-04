"""Preseason team ratings projected from last year's ratings AND roster turnover.

Why: until KenPom's first snapshot of a season (~Nov 8) the model only had LAST season's final ratings, so a
team that lost most of its roster (e.g. a title team that graduated its stars) was still rated like last year's team.

Roster information comes from ESPN player box scores (sportsdataverse player_box_{season}.parquet, no key needed):

  ret_min / ret_prod  share of last season's minutes / production (pts + reb + ast + 2*(stl+blk) - tov) that is
                      back on the roster this season
  ret_top5            share of last season's minutes held by the five biggest returning minute-getters
  n_ret               returning players who averaged >= 10 minutes
  in_min / in_prod    incoming transfers (>= 15 mpg elsewhere last season): their minutes per game / 200, and
                      their production as a share of this team's last-season production

"Roster" = players who appeared in the team's first K games this season (K=1: available after one game; using
K=2 or 3 changes nothing). A team with no games yet falls back to a prior-only projection.

AdjEM is a ridge regression of the season's FINAL KenPom AdjEM on those
features plus last season's and the previous season's final ratings. Walk-forward: a season is projected by models
fit on EARLIER seasons only (the first projectable season uses the other seasons, since nothing precedes it).
Leave-one-season-out RMSE on final AdjEM: 7.61 (last year's rating as-is) -> 7.37 (regressed to the mean)
-> 6.25 (with roster features and the elite hinge; bias for teams rated 20+ is about +0.1 pts).
"""
import glob

import numpy as np
import pandas as pd
from sklearn.linear_model import Ridge
from sklearn.preprocessing import StandardScaler

RATING = ["AdjEM", "AdjOE", "AdjDE", "AdjTempo"]
PRIOR_FEATS = ["p_AdjEM", "p_AdjOE", "p_AdjDE", "p_AdjTempo", "p2_AdjEM", "p_elite"]
ROSTER_FEATS = ["ret_min", "ret_prod", "ret_top5", "n_ret", "in_min", "in_prod", "p_x_ret", "p_x_in"]
K_GAMES = 1
RATING_CAP = 35.0


def load_players(player_dir):
    cols = ["game_id", "season", "game_date", "athlete_id", "team_id", "minutes", "points", "rebounds",
            "assists", "steals", "blocks", "turnovers", "did_not_play"]
    files = sorted(glob.glob(f"{player_dir}/player_box_*.parquet"))
    p = pd.concat([pd.read_parquet(f, columns=cols) for f in files], ignore_index=True)
    p = p.dropna(subset=["athlete_id", "team_id", "game_id"])
    p["athlete_id"] = p.athlete_id.astype("int64")
    p["team_id"] = p.team_id.astype("int64")
    p["game_date"] = pd.to_datetime(p.game_date)
    p["minutes"] = p.minutes.fillna(0.0)
    p.loc[p.did_not_play.fillna(False).astype(bool), "minutes"] = 0.0
    p["prod"] = (p[["points", "rebounds", "assists"]].fillna(0).sum(axis=1)
                 + 2 * p[["steals", "blocks"]].fillna(0).sum(axis=1) - p.turnovers.fillna(0))
    return p.drop_duplicates(["game_id", "team_id", "athlete_id"])


def roster_features(p, k=K_GAMES):
    """One row per (season, team_id) that has played at least one game: the roster features above."""
    tg = p[["season", "team_id", "game_id", "game_date"]].drop_duplicates().sort_values(
        ["season", "team_id", "game_date", "game_id"])
    tg["n"] = tg.groupby(["season", "team_id"]).cumcount()
    p = p.merge(tg[["season", "team_id", "game_id", "n"]], on=["season", "team_id", "game_id"])
    ps = p[p.minutes > 0].groupby(["season", "team_id", "athlete_id"]).agg(
        g=("game_id", "nunique"), mins=("minutes", "sum"), prod=("prod", "sum")).reset_index()
    ps["mpg"] = ps.mins / ps.g
    tt = ps.groupby(["season", "team_id"])[["mins", "prod"]].sum().rename(
        columns={"mins": "tmins", "prod": "tprod"})
    early = p[(p.n < k) & (p.minutes > 0)][["season", "team_id", "athlete_id"]].drop_duplicates()
    by_season = {s: g for s, g in ps.groupby("season")}
    rows = []
    for (s, tid), g in early.groupby(["season", "team_id"]):
        prev = by_season.get(s - 1)
        if prev is None or (s - 1, tid) not in tt.index:
            continue
        ids = set(g.athlete_id)
        tm, tp = tt.loc[(s - 1, tid)]
        pv = prev[prev.team_id == tid]
        ret = pv[pv.athlete_id.isin(ids)]
        new = prev[prev.athlete_id.isin(ids) & (prev.team_id != tid) & (prev.mpg >= 15) & (prev.g >= 10)]
        new = new.sort_values("mins").drop_duplicates("athlete_id", keep="last")
        rows.append(dict(season=s, team_id=tid, ret_min=ret.mins.sum() / tm, ret_prod=ret["prod"].sum() / max(tp, 1),
                         ret_top5=ret.nlargest(5, "mins").mins.sum() / tm, n_ret=int((ret.mpg >= 10).sum()),
                         in_min=new.mpg.sum() / 200, in_prod=new["prod"].sum() / max(tp, 1)))
    return pd.DataFrame(rows)


def team_season_table(final, roster, id_map):
    """final: kp_ratings (TeamName, season, RATING cols). roster: roster_features(). id_map: season, team_id, TeamName.
    Returns one row per (season, TeamName) with prior-rating features, roster features (NaN if unknown) and the
    season's own final ratings (NaN for a season that has not finished)."""
    f = final[["TeamName", "season"] + RATING]
    p1 = f.assign(season=f.season + 1).rename(columns={c: f"p_{c}" for c in RATING})
    p2 = f[["TeamName", "season", "AdjEM"]].assign(season=lambda x: x.season + 2).rename(columns={"AdjEM": "p2_AdjEM"})
    t = p1.merge(p2, on=["TeamName", "season"], how="left")
    t["p2_AdjEM"] = t.p2_AdjEM.fillna(t.p_AdjEM)
    t = t.merge(id_map, on=["TeamName", "season"], how="left")
    t = t.merge(roster, on=["season", "team_id"], how="left")
    # p_elite: elite programs keep more of their strength than a straight line through last year's rating says
    # (without it the projection ran 3.4 points too low for teams rated 20+). A hinge, not a square, so a record
    # rating cannot be extrapolated wildly; ratings are capped at 35 (the model has seen ~39 only once).
    # The x_ terms let roster turnover matter more for strong teams.
    pc = t.p_AdjEM.clip(upper=RATING_CAP)
    t["p_elite"] = np.maximum(pc - 15, 0)
    t["p_x_ret"] = pc * t.ret_prod
    t["p_x_in"] = pc * t.in_prod
    return t.merge(f, on=["TeamName", "season"], how="left")


def _fit(tr, feats, target):
    sc = StandardScaler().fit(tr[feats])
    m = Ridge(alpha=10.0).fit(sc.transform(tr[feats]), tr[target])
    return sc, m


def project(table, seasons, strict=True):
    """Preseason projections for the given seasons -> DataFrame(TeamName, season, pre_<rating>...).
    strict: fit only on earlier seasons (falls back to all other seasons if there are none).

    Only AdjEM is regressed. The change it implies versus last year's rating is split evenly between offense and
    defense (AdjOE up / AdjDE down by half each), so AdjOE + AdjDE and tempo keep last year's values. Projecting
    OE, DE and tempo separately made early-season TOTALS worse (14.48 -> 15.02 MAE on games where a team still
    used last year's ratings) while giving the same margin gain."""
    out = []
    labelled = table[table.AdjEM.notna() & table.ret_min.notna()]
    for s in seasons:
        tr = labelled[labelled.season < s] if strict else labelled[labelled.season != s]
        if len(tr) < 200:
            tr = labelled[labelled.season != s]
        te = table[table.season == s].copy()
        if te.empty:
            continue
        has_r = te.ret_min.notna().values
        full = _fit(tr, PRIOR_FEATS + ROSTER_FEATS, "AdjEM")
        base = _fit(tr, PRIOR_FEATS, "AdjEM")
        pred = base[1].predict(base[0].transform(te[PRIOR_FEATS].fillna(tr[PRIOR_FEATS].mean())))
        if has_r.any():
            pred = pred.copy()
            pred[has_r] = full[1].predict(full[0].transform(te.loc[has_r, PRIOR_FEATS + ROSTER_FEATS]))
        pred = np.where(te.p_AdjEM.notna().values, pred, np.nan)
        delta = pred - te.p_AdjEM.values
        te["pre_AdjEM"] = pred
        te["pre_AdjOE"] = te.p_AdjOE + delta / 2
        te["pre_AdjDE"] = te.p_AdjDE - delta / 2
        te["pre_AdjTempo"] = te.p_AdjTempo
        out.append(te[["TeamName", "season"] + [f"pre_{c}" for c in RATING]])
    return pd.concat(out, ignore_index=True)
