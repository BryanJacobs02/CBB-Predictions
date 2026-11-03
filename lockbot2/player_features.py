"""Player-level team features from ESPN player box scores (sportsdataverse player_box_{season}.parquet).

Everything for a game is computed from information available before tip-off:

  * lost_min1 / lost_prod1  - rotation players (>= 8 min/g baseline) who did NOT play in the team's
    previous game, weighted by their baseline minutes / production (pts+reb+ast). A proxy for an injury
    or suspension the market already knows about and ratings have not caught up to.
    Baseline is measured through game t-2, so the missing game itself is never in it.
  * lost_min3              - same, for players who missed each of the last 3 games (baseline through t-4)
  * star_share             - top player's share of team production (how much one absence can hurt)
  * n_rot                  - number of players averaging >= 10 minutes
  * ret_min                - share of LAST season's minutes returning from players on this season's roster
  * in_min                 - established newcomers (played elsewhere last season) as minutes per game /200

Caveat: roster membership for ret_min / in_min is "appeared in >= 3 games this season", i.e. an approximation
of the preseason roster that peeks slightly at who ended up playing.
"""
import glob

import numpy as np
import pandas as pd

HL = 5            # EWM half-life in team games
ROT_MIN = 8.0     # baseline minutes to count as rotation for the "lost" features


def load_players(player_dir):
    cols = ["game_id", "game_date", "season", "team_id", "athlete_id", "minutes",
            "points", "rebounds", "assists", "did_not_play"]
    p = pd.concat([pd.read_parquet(f, columns=cols) for f in sorted(glob.glob(f"{player_dir}/player_box_*.parquet"))],
                  ignore_index=True)
    p = p.dropna(subset=["game_id", "team_id", "athlete_id"])
    p["game_id"] = p.game_id.astype("int64").astype(str)
    p["team_id"] = p.team_id.astype("int64").astype(str)
    p["athlete_id"] = p.athlete_id.astype("int64")
    p["game_date"] = pd.to_datetime(p.game_date)
    p["minutes"] = p.minutes.fillna(0.0)
    p.loc[p.did_not_play.fillna(False).astype(bool), "minutes"] = 0.0
    p["prod"] = p[["points", "rebounds", "assists"]].fillna(0).sum(axis=1)
    return p.drop_duplicates(["game_id", "team_id", "athlete_id"])


def roster_priors(p):
    """Per (team_id, season): ret_min and in_min from the previous season."""
    app = p[p.minutes > 0].groupby(["season", "team_id", "athlete_id"]).agg(
        n=("game_id", "nunique"), mins=("minutes", "sum")).reset_index()
    app["mpg"] = app.mins / app.n
    roster = app[app.n >= 3]
    last = app.rename(columns={"season": "prev", "mins": "pmins", "mpg": "pmpg", "team_id": "pteam"})
    last["season"] = last.prev + 1
    team_tot = app.groupby(["season", "team_id"]).mins.sum().rename("tot").reset_index()
    team_tot["season"] += 1

    # returning: on the same team last season
    same = roster.merge(last[["season", "pteam", "athlete_id", "pmins"]],
                        left_on=["season", "team_id", "athlete_id"], right_on=["season", "pteam", "athlete_id"])
    ret = same.groupby(["season", "team_id"]).pmins.sum().rename("ret").reset_index()
    ret = ret.merge(team_tot, on=["season", "team_id"], how="left")
    ret["ret_min"] = ret.ret / ret.tot

    # newcomers who played (>= 15 mpg, >= 10 games) for another team last season
    prev_any = last[(last.pmpg >= 15)][["season", "athlete_id", "pteam", "pmpg"]].drop_duplicates(["season", "athlete_id"])
    new = roster.merge(prev_any, on=["season", "athlete_id"])
    new = new[new.pteam != new.team_id]
    inn = (new.groupby(["season", "team_id"]).pmpg.sum() / 200).rename("in_min").reset_index()

    out = roster[["season", "team_id"]].drop_duplicates().merge(ret[["season", "team_id", "ret_min"]], how="left") \
        .merge(inn, how="left")
    out["in_min"] = out.in_min.fillna(0.0)
    out.loc[out.season == out.season.min(), ["ret_min", "in_min"]] = np.nan   # no prior season loaded
    return out


def team_season_form(g):
    """g: player rows of ONE team-season. Returns one row per team game."""
    # incomplete box scores (few players listed) would make everyone look absent: ignore those games
    tot = g.groupby("game_id").minutes.sum()
    g = g[g.game_id.isin(tot[tot >= 150].index)]
    if g.empty:
        return pd.DataFrame(columns=["game_id", "lost_min1", "lost_prod1", "lost_min3", "star_share", "n_rot"])
    games = g[["game_id", "game_date"]].drop_duplicates().sort_values(["game_date", "game_id"])
    order = {gid: i for i, gid in enumerate(games.game_id)}
    M = g.pivot_table(index="game_id", columns="athlete_id", values="minutes", aggfunc="sum").reindex(games.game_id).fillna(0.0)
    P = g.pivot_table(index="game_id", columns="athlete_id", values="prod", aggfunc="sum").reindex(games.game_id).fillna(0.0)
    B = M.ewm(halflife=HL, adjust=True).mean().values          # baseline minutes through game i
    BP = P.ewm(halflife=HL, adjust=True).mean().values
    m = M.values
    n = len(games)
    out = {k: np.full(n, np.nan) for k in ("lost_min1", "lost_prod1", "lost_min3", "star_share", "n_rot")}
    for t in range(1, n):
        if t >= 2:
            base, bp = B[t - 2], BP[t - 2]
            gone = (m[t - 1] == 0) & (base >= ROT_MIN)
            out["lost_min1"][t] = base[gone].sum()
            out["lost_prod1"][t] = bp[gone].sum()
        else:
            out["lost_min1"][t] = out["lost_prod1"][t] = 0.0
        if t >= 4:
            gone3 = (m[t - 1] == 0) & (m[t - 2] == 0) & (m[t - 3] == 0) & (B[t - 4] >= ROT_MIN)
            out["lost_min3"][t] = B[t - 4][gone3].sum()
        else:
            out["lost_min3"][t] = 0.0
        tot = BP[t - 1].sum()
        out["star_share"][t] = BP[t - 1].max() / tot if tot > 0 else np.nan
        out["n_rot"][t] = (B[t - 1] >= 10).sum()
    res = pd.DataFrame(out)
    res.insert(0, "game_id", games.game_id.values)
    return res


def team_player_features(player_dir):
    p = load_players(player_dir)
    rows = []
    for (team, season), g in p.groupby(["team_id", "season"], sort=False):
        f = team_season_form(g)
        f["team_id"], f["season"] = team, season
        rows.append(f)
    f = pd.concat(rows, ignore_index=True)
    return f.merge(roster_priors(p), on=["season", "team_id"], how="left").drop(columns="season")


FEATS = ["lost_min1", "lost_prod1", "lost_min3", "star_share", "n_rot", "ret_min", "in_min"]


def add_player_features(g, player_dir):
    f = team_player_features(player_dir)
    for tag, idcol in (("ph", "home_id"), ("pa", "away_id")):
        part = f.rename(columns={"team_id": idcol, **{c: f"{tag}_{c}" for c in FEATS}})
        g = g.merge(part[["game_id", idcol] + [f"{tag}_{c}" for c in FEATS]], on=["game_id", idcol], how="left")
    return g
