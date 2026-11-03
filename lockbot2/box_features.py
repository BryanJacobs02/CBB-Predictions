"""Point-in-time team form from ESPN box scores (sportsdataverse team_box_{season}.parquet).

Every feature for a game is computed from that team's PREVIOUS games in the same season
(shift(1) before any rolling window), so nothing from the game itself leaks in.

Per game: possessions ~ FGA - OREB + TO + 0.475*FTA; four-factor style rates for the team
and for what it allowed; pace. Smoothed with an exponentially weighted mean (half-life 6
games, needs >= 3 prior games).
"""
import glob

import numpy as np
import pandas as pd

STATS = ["off_eff", "def_eff", "pace", "efg", "tov", "orb", "ftr", "tpar",
         "def_efg", "def_tov", "drb", "def_ftr"]


def load_box(box_dir):
    b = pd.concat([pd.read_parquet(f) for f in sorted(glob.glob(f"{box_dir}/team_box_*.parquet"))],
                  ignore_index=True)
    for c in ("game_id", "team_id", "opponent_team_id"):
        b[c] = b[c].astype("int64").astype(str)
    b["game_date"] = pd.to_datetime(b.game_date)
    b["tov_all"] = b.total_turnovers.fillna(b.turnovers)
    b["poss"] = b.field_goals_attempted - b.offensive_rebounds + b.tov_all + 0.475 * b.free_throws_attempted
    return b.drop_duplicates(["game_id", "team_id"])


def team_game_rates(b):
    cols = ["game_id", "team_id", "field_goals_made", "field_goals_attempted", "three_point_field_goals_made",
            "offensive_rebounds", "defensive_rebounds", "free_throws_attempted", "poss", "tov_all", "team_score"]
    o = b[cols].add_prefix("o_")
    m = b.merge(o, left_on=["game_id", "opponent_team_id"], right_on=["o_game_id", "o_team_id"])
    pace = (m.poss + m.o_poss) / 2
    m["pace"] = pace
    m["off_eff"] = 100 * m.team_score / pace
    m["def_eff"] = 100 * m.o_team_score / pace
    m["efg"] = (m.field_goals_made + 0.5 * m.three_point_field_goals_made) / m.field_goals_attempted
    m["tov"] = m.tov_all / m.poss
    m["orb"] = m.offensive_rebounds / (m.offensive_rebounds + m.o_defensive_rebounds)
    m["ftr"] = m.free_throws_attempted / m.field_goals_attempted
    m["tpar"] = m.three_point_field_goals_attempted / m.field_goals_attempted
    m["def_efg"] = (m.o_field_goals_made + 0.5 * m.o_three_point_field_goals_made) / m.o_field_goals_attempted
    m["def_tov"] = m.o_tov_all / m.o_poss
    m["drb"] = m.defensive_rebounds / (m.defensive_rebounds + m.o_offensive_rebounds)
    m["def_ftr"] = m.o_free_throws_attempted / m.o_field_goals_attempted
    return m


def rolling_form(m):
    m = m.replace([np.inf, -np.inf], np.nan).sort_values(["team_id", "season", "game_date", "game_id"])
    grp = m.groupby(["team_id", "season"])
    out = m[["game_id", "team_id"]].copy()
    for s in STATS:
        out[f"{s}_ewm"] = grp[s].transform(lambda x: x.shift().ewm(halflife=6, min_periods=3).mean())
    return out


def add_box_features(g, box_dir):
    form = rolling_form(team_game_rates(load_box(box_dir)))
    cols = [c for c in form.columns if c not in ("game_id", "team_id")]
    for tag, idcol in (("bh", "home_id"), ("ba", "away_id")):
        part = form.rename(columns={"team_id": idcol, **{c: f"{tag}_{c}" for c in cols}})
        g = g.merge(part, on=["game_id", idcol], how="left")
    return g
