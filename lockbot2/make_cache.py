"""Lockbot 2.0 prediction cache: margin / total / win probability for every D1 matchup.

Trains the ridge+LightGBM blend (with box-score form features) for margin and total on every completed game in the data table,
describes each team by its latest point-in-time state (newest KenPom snapshot, box-score form
through the last game, schedule context), and scores every ordered pair for home and neutral.

Win probability = Normal CDF(margin / sigma). Sigma is the residual SD of the same model type
trained on all seasons before the newest one and scored on the newest one, so it is out-of-sample.

Usage:
  python3 -I lockbot2/make_cache.py --data lockbot2/out/games_model.csv \
      --export-dir <lockbot_export> --box-dir <boxdata> --season 2026 --out data/predictions_v2.csv
"""
import argparse
import sys
from pathlib import Path

import lightgbm as lgb
import numpy as np
import pandas as pd
from scipy.stats import norm
from sklearn.impute import SimpleImputer
from sklearn.linear_model import RidgeCV
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler

sys.path.insert(0, str(Path(__file__).parent))
import box_features as bf  # noqa: E402
import preseason as ps  # noqa: E402
from backtest_baselines import features  # noqa: E402

KW = dict(n_estimators=500, learning_rate=0.02, num_leaves=12, min_child_samples=80,
          subsample=0.8, subsample_freq=1, colsample_bytree=0.8, reg_lambda=10,
          random_state=0, n_jobs=2, verbose=-1)
RATING = ["AdjEM", "AdjOE", "AdjDE", "AdjTempo"]
HCA_TARGET = 4.1     # model's own average home-court edge in-season (2026 holdout, Jan 2026 as-of)


class Blend:
    """Ridge + LightGBM blend on the same features. Weight is on ridge."""
    def __init__(self, w_ridge, y, tr):
        X = features(tr, True)
        self.w = w_ridge
        self.ridge = make_pipeline(SimpleImputer(strategy="median"), StandardScaler(),
                                   RidgeCV(alphas=np.logspace(-1, 3, 12))).fit(X, tr[y])
        self.gbm = lgb.LGBMRegressor(**KW).fit(X, tr[y])

    def predict(self, X):
        return self.w * self.ridge.predict(X) + (1 - self.w) * self.gbm.predict(X)


def fit(tr):
    """Final models. Margin: 50/50 blend. Total: 70% ridge / 30% LightGBM.
    (Walk-forward 2024-26: blends beat either model alone by ~0.02 pts MAE; small but consistent.)"""
    return Blend(0.5, "margin", tr), Blend(0.7, "total", tr)


def current_box_state(box_dir, season, asof):
    """EWM of each team's box-score form INCLUDING its latest game before `asof` (no shift): the form
    to carry into the next game. Same smoothing as training (half-life 6, >=3 games). Teams with fewer
    than 3 games get NaN, exactly like early-season games in the training data."""
    m = bf.team_game_rates(bf.load_box(box_dir))
    m = m[(m.season == season) & (m.game_date < asof)].replace([np.inf, -np.inf], np.nan)
    cols = [f"{s}_ewm" for s in bf.STATS]
    if m.empty:
        return pd.DataFrame(columns=cols, index=pd.Index([], name="team_id"))
    m = m.sort_values(["team_id", "game_date", "game_id"])
    g = m.groupby("team_id")
    out = pd.DataFrame({"team_id": list(g.groups)}).set_index("team_id")
    for s in bf.STATS:
        out[f"{s}_ewm"] = g[s].apply(lambda x: x.ewm(halflife=6, min_periods=3).mean().iloc[-1])
    return out


def season_projection(d, E, player_dir, season):
    """Preseason ratings for `season` from last year's ratings + roster turnover (None if player data is missing)."""
    if not player_dir or not list(Path(player_dir).glob(f"player_box_{season}.parquet")):
        print(f"  no player box scores for {season} yet: teams without a snapshot use last year's final ratings")
        return None
    final = pd.read_csv(E / "kp_ratings.csv", usecols=["TeamName", "season"] + ps.RATING)
    names = pd.concat([d[["kp_home", "home_id"]].set_axis(["TeamName", "team_id"], axis=1),
                       d[["kp_away", "away_id"]].set_axis(["TeamName", "team_id"], axis=1)]).dropna()
    names["team_id"] = names.team_id.astype(float).astype("int64")       # ESPN ids are stable across seasons
    names = names.drop_duplicates("TeamName", keep="last")
    id_map = pd.concat([names.assign(season=y) for y in range(int(final.season.min()), season + 1)], ignore_index=True)
    roster = ps.roster_features(ps.load_players(player_dir))
    proj = ps.project(ps.team_season_table(final, roster, id_map), [season])
    n = int(roster[roster.season == season].shape[0])
    print(f"  preseason projection for {season}: roster info for {n} teams")
    return proj.set_index("TeamName")


def team_states(d, E, box_dir, season, asof, proj=None):
    """One row per team with everything features() needs, as of `asof`."""
    snaps = pd.read_csv(E / "kp_archive_weekly.csv", usecols=["TeamName", "season", "snap_date"] + RATING)
    snaps["snap_date"] = pd.to_datetime(snaps.snap_date)
    snaps = snaps[(snaps.season == season) & (snaps.snap_date <= asof)].sort_values("snap_date")
    final = pd.read_csv(E / "kp_ratings.csv", usecols=["TeamName", "season"] + RATING)
    prior = final[final.season == season - 1].set_index("TeamName")

    cur = d[(d.season == season) & (d.game_date < asof)]
    ref = d[d.season.isin([season, season - 1])].sort_values("game_date")     # ESPN ids are stable across seasons
    home = ref[["kp_home", "home_id"]].set_axis(["team", "espn"], axis=1)
    away = ref[["kp_away", "away_id"]].set_axis(["team", "espn"], axis=1)
    ids = pd.concat([home, away]).drop_duplicates("team", keep="last").set_index("team").espn
    box = current_box_state(box_dir, season, asof)

    rows = {}
    for team, grp in snaps.groupby("TeamName"):
        last = grp.iloc[-1]
        old = grp[grp.snap_date <= last.snap_date - pd.Timedelta(days=28)]
        r = {c: last[c] for c in RATING}
        r["snap_age"] = (asof - last.snap_date).days
        r["mom"] = last.AdjEM - old.iloc[-1].AdjEM if len(old) else np.nan
        r["from_prior"] = 0
        rows[team] = r
    for team in prior.index.difference(list(rows)):          # preseason: projected ratings (else last year's final)
        # No snapshot yet: treat the preseason projection as a normal snapshot. (Training has few
        # from_prior=1 games, so flagging it would push the model into a barely-seen regime.)
        if proj is not None and team in proj.index and proj.loc[team].notna().all():
            base = {c: proj.loc[team, f"pre_{c}"] for c in RATING}
        else:
            base = prior.loc[team, RATING].to_dict()
        rows[team] = {**base, "snap_age": 6.0, "mom": np.nan, "from_prior": 0}
    S = pd.DataFrame(rows).T.astype(float)
    S["prior_em"] = prior.AdjEM.reindex(S.index)
    if proj is not None:                                      # the model's prior_AdjEM feature = projection, as in training
        S["prior_em"] = proj.pre_AdjEM.reindex(S.index).fillna(S.prior_em)

    # schedule context from completed games before asof
    long = pd.concat([
        cur[["kp_home", "game_date", "margin"]].set_axis(["team", "date", "m"], axis=1),
        cur[["kp_away", "game_date", "margin"]].assign(margin=lambda x: -x.margin).set_axis(["team", "date", "m"], axis=1)])
    long = long[long.date < asof].sort_values(["team", "date"])
    lg = long.groupby("team")
    S["gp"] = lg.size().reindex(S.index).fillna(0)
    S["last5"] = lg.m.apply(lambda x: x.tail(5).mean()).reindex(S.index)
    S["rest"] = 4.0
    S["espn_id"] = ids.reindex(S.index)
    S = S.join(box.rename(columns=lambda c: "box_" + c), on="espn_id")
    return S


def pair_frame(S, a, b, neutral, asof, season, lg_total):
    """Feature rows in the layout features() expects, team a = 'home' slot, team b = 'away' slot."""
    A, B = S.loc[a].reset_index(drop=True), S.loc[b].reset_index(drop=True)
    d = pd.DataFrame(index=range(len(A)))
    for c in RATING:
        d[f"{c}_h_eff"], d[f"{c}_a_eff"] = A[c].values, B[c].values
    for tag, T in (("h", A), ("a", B)):
        d[f"gp_{tag}"], d[f"rest_{tag}"], d[f"last5_{tag}"] = T.gp.values, T.rest.values, T.last5.values
        d[f"mom_{tag}"], d[f"snap_age_{tag}"], d[f"from_prior_{tag}"] = T.mom.values, T.snap_age.values, T.from_prior.values
        d[f"prior_AdjEM_{tag}"] = T.prior_em.values
    d["neutral"], d["conf_game"], d["postseason"] = neutral, 0, 0
    d["lg_total_30d"] = lg_total
    for s in bf.STATS:
        d[f"bh_{s}_ewm"] = pd.to_numeric(A[f"box_{s}_ewm"], errors="coerce").values
        d[f"ba_{s}_ewm"] = pd.to_numeric(B[f"box_{s}_ewm"], errors="coerce").values
    d["game_date"] = asof
    d["season"] = season
    return d


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--data", required=True)
    ap.add_argument("--export-dir", required=True)
    ap.add_argument("--box-dir", required=True)
    ap.add_argument("--player-dir", default=None, help="folder with player_box_{season}.parquet (preseason projection)")
    ap.add_argument("--season", type=int, default=2026)
    ap.add_argument("--asof", default=None, help="YYYY-MM-DD; default = day after the last game")
    ap.add_argument("--out", required=True)
    a = ap.parse_args()

    d = pd.read_csv(a.data, parse_dates=["game_date"], dtype={"home_id": str, "away_id": str})
    d = d[d.AdjEM_h_eff.notna() & d.AdjEM_a_eff.notna()]      # same filter as eval_blend.py
    done = d[d.season == a.season]
    asof = pd.Timestamp(a.asof) if a.asof else (
        done.game_date.max() + pd.Timedelta(days=1) if len(done) else pd.Timestamp.today().normalize())
    d = d[d.game_date < asof]                                  # never train on games after the as-of date

    # out-of-sample sigma: train before the newest season that is complete (>= 4000 games), score that season
    n_by = d.groupby("season").size()
    sig_season = int(n_by[n_by >= 4000].index.max())
    tr, te = d[d.season < sig_season], d[d.season == sig_season]
    mg, _ = fit(tr)
    resid = te.margin - mg.predict(features(te, True))
    sigma = float(resid.std())
    print(f"sigma (OOS {sig_season}) = {sigma:.2f}, mean residual = {resid.mean():+.2f}, n={len(te)}")

    mg, tg = fit(d)
    proj = season_projection(d, Path(a.export_dir), a.player_dir, a.season)
    S = team_states(d, Path(a.export_dir), a.box_dir, a.season, asof, proj)
    S = S[S.espn_id.notna() & S.AdjEM.notna()]
    teams = sorted(S.index)
    recent = d[d.season == (a.season if len(done) else a.season - 1)]
    lg_total = recent.tail(3000).total.mean()
    print(f"{len(teams)} teams as of {asof.date()}, league total {lg_total:.1f}, "
          f"{int(S.box_efg_ewm.notna().sum())} with in-season box form, {int(S.snap_age.eq(6.0).sum())} on last year's final ratings (no snapshot yet)")

    ia, ib = np.meshgrid(np.arange(len(teams)), np.arange(len(teams)), indexing="ij")
    keep = ia != ib
    ta, tb = np.array(teams)[ia[keep]], np.array(teams)[ib[keep]]
    out = pd.DataFrame({"team_a": ta, "team_b": tb})
    for loc, neutral in (("a_home", 0), ("neutral", 1)):
        X = features(pair_frame(S, ta, tb, neutral, asof, a.season, lg_total), True)
        out[f"margin_{loc}"] = mg.predict(X)
        out[f"total_{loc}"] = tg.predict(X)
    # neutral court has no home slot: average both orderings so results don't depend on who is "a"
    rev = out.set_index(["team_a", "team_b"]).loc[list(zip(out.team_b, out.team_a))]
    out["margin_neutral"] = (out.margin_neutral.values - rev.margin_neutral.values) / 2
    out["total_neutral"] = (out.total_neutral.values + rev.total_neutral.values) / 2
    # Home-court edge sanity: in-season the model's average edge (home minus neutral) is ~4.1 pts. In the
    # first days of a season (no KenPom snapshot, no box form) it drifts to 5-6 because that regime is
    # rare in training. If the average drifts, rescale the edge (not the neutral number) back to target.
    hca = out.margin_a_home - out.margin_neutral
    if abs(hca.mean() - HCA_TARGET) > 0.5:
        print(f"home-court edge averaged {hca.mean():.2f} pts; rescaling to {HCA_TARGET}")
        out["margin_a_home"] = out.margin_neutral + hca * (HCA_TARGET / hca.mean())
    for loc in ("a_home", "neutral"):
        out[f"win_prob_{loc}"] = norm.cdf(out[f"margin_{loc}"] / sigma)
    out = out.round({c: 3 for c in out.columns if c not in ("team_a", "team_b")})
    out.attrs["sigma"] = sigma
    # ESPN team id -> KenPom name, so R scripts can place ESPN games in this cache
    tm = S.reset_index().rename(columns={"index": "kp_name", "TeamName": "kp_name"})[["kp_name", "espn_id"]]
    tm.to_csv(Path(a.out).parent / "team_map.csv", index=False)

    Path(a.out).parent.mkdir(parents=True, exist_ok=True)
    out.to_csv(a.out, index=False)
    Path(a.out).with_suffix(".meta.json").write_text(
        '{"sigma": %.3f, "asof": "%s", "season": %d, "n_teams": %d}' % (sigma, asof.date(), a.season, len(teams)))
    print(f"wrote {a.out}: {len(out)} matchups")
    chk = out[out.team_a.isin(teams[:0] + ["Duke", "Houston", "Kansas"]) & out.team_b.isin(["Duke", "Houston", "Kansas"])]
    print(chk[["team_a", "team_b", "margin_a_home", "margin_neutral", "total_neutral", "win_prob_neutral"]].to_string(index=False))


if __name__ == "__main__":
    main()
