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
from backtest_baselines import features  # noqa: E402

KW = dict(n_estimators=500, learning_rate=0.02, num_leaves=12, min_child_samples=80,
          subsample=0.8, subsample_freq=1, colsample_bytree=0.8, reg_lambda=10,
          random_state=0, n_jobs=2, verbose=-1)
RATING = ["AdjEM", "AdjOE", "AdjDE", "AdjTempo"]


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


def current_box_state(box_dir, season):
    """EWM of each team's box-score form INCLUDING its latest game (no shift): the form to
    carry into the next game. Same smoothing as training (half-life 6, >=3 games)."""
    m = bf.team_game_rates(bf.load_box(box_dir))
    m = m[m.season == season].replace([np.inf, -np.inf], np.nan)
    m = m.sort_values(["team_id", "game_date", "game_id"])
    g = m.groupby("team_id")
    out = pd.DataFrame({"team_id": list(g.groups)}).set_index("team_id")
    for s in bf.STATS:
        out[f"{s}_ewm"] = g[s].apply(lambda x: x.ewm(halflife=6, min_periods=3).mean().iloc[-1])
    return out


def team_states(d, E, box_dir, season, asof):
    """One row per team with everything features() needs, as of `asof`."""
    snaps = pd.read_csv(E / "kp_archive_weekly.csv", usecols=["TeamName", "season", "snap_date"] + RATING)
    snaps["snap_date"] = pd.to_datetime(snaps.snap_date)
    snaps = snaps[(snaps.season == season) & (snaps.snap_date <= asof)].sort_values("snap_date")
    final = pd.read_csv(E / "kp_ratings.csv", usecols=["TeamName", "season"] + RATING)
    prior = final[final.season == season - 1].set_index("TeamName")

    cur = d[d.season == season]
    home = cur[["kp_home", "home_id"]].set_axis(["team", "espn"], axis=1)
    away = cur[["kp_away", "away_id"]].set_axis(["team", "espn"], axis=1)
    ids = pd.concat([home, away]).drop_duplicates("team").set_index("team").espn
    box = current_box_state(box_dir, season)

    rows = {}
    for team, grp in snaps.groupby("TeamName"):
        last = grp.iloc[-1]
        old = grp[grp.snap_date <= last.snap_date - pd.Timedelta(days=28)]
        r = {c: last[c] for c in RATING}
        r["snap_age"] = (asof - last.snap_date).days
        r["mom"] = last.AdjEM - old.iloc[-1].AdjEM if len(old) else np.nan
        r["from_prior"] = 0
        rows[team] = r
    for team in prior.index.difference(list(rows)):          # preseason: last year's final ratings
        rows[team] = {**prior.loc[team, RATING].to_dict(), "snap_age": np.nan, "mom": np.nan, "from_prior": 1}
    S = pd.DataFrame(rows).T.astype(float)
    S["prior_em"] = prior.AdjEM.reindex(S.index)

    # schedule context from completed games before asof
    long = pd.concat([
        cur[["kp_home", "game_date", "margin"]].set_axis(["team", "date", "m"], axis=1),
        cur[["kp_away", "game_date", "margin"]].assign(margin=lambda x: -x.margin).set_axis(["team", "date", "m"], axis=1)])
    long = long[long.date <= asof].sort_values(["team", "date"])
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
        d[f"bh_{s}_ewm"], d[f"ba_{s}_ewm"] = A[f"box_{s}_ewm"].values, B[f"box_{s}_ewm"].values
    d["game_date"] = asof
    d["season"] = season
    return d


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--data", required=True)
    ap.add_argument("--export-dir", required=True)
    ap.add_argument("--box-dir", required=True)
    ap.add_argument("--season", type=int, default=2026)
    ap.add_argument("--asof", default=None, help="YYYY-MM-DD; default = day after the last game")
    ap.add_argument("--out", required=True)
    a = ap.parse_args()

    d = pd.read_csv(a.data, parse_dates=["game_date"], dtype={"home_id": str, "away_id": str})
    d = d[d.AdjEM_h_eff.notna() & d.AdjEM_a_eff.notna() & d.bh_efg_ewm.notna() & d.ba_efg_ewm.notna()]
    asof = pd.Timestamp(a.asof) if a.asof else d[d.season == a.season].game_date.max() + pd.Timedelta(days=1)

    # out-of-sample sigma: train before the newest season, score the newest
    tr, te = d[d.season < a.season], d[d.season == a.season]
    mg, _ = fit(tr)
    resid = te.margin - mg.predict(features(te, True))
    sigma = float(resid.std())
    bias = float(resid.mean())
    print(f"sigma (OOS {a.season}) = {sigma:.2f}, mean residual = {bias:+.2f}, n={len(te)}")

    mg, tg = fit(d)
    S = team_states(d, Path(a.export_dir), a.box_dir, a.season, asof)
    S = S[S.espn_id.notna() & S.box_efg_ewm.notna() & S.AdjEM.notna()]
    teams = sorted(S.index)
    lg_total = d[d.season == a.season].tail(3000).total.mean()
    print(f"{len(teams)} teams as of {asof.date()}, league total {lg_total:.1f}")

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
    for loc in ("a_home", "neutral"):
        out[f"win_prob_{loc}"] = norm.cdf(out[f"margin_{loc}"] / sigma)
    out = out.round({c: 3 for c in out.columns if c not in ("team_a", "team_b")})
    out.attrs["sigma"] = sigma

    Path(a.out).parent.mkdir(parents=True, exist_ok=True)
    out.to_csv(a.out, index=False)
    Path(a.out).with_suffix(".meta.json").write_text(
        '{"sigma": %.3f, "asof": "%s", "season": %d, "n_teams": %d}' % (sigma, asof.date(), a.season, len(teams)))
    print(f"wrote {a.out}: {len(out)} matchups")
    chk = out[out.team_a.isin(teams[:0] + ["Duke", "Houston", "Kansas"]) & out.team_b.isin(["Duke", "Houston", "Kansas"])]
    print(chk[["team_a", "team_b", "margin_a_home", "margin_neutral", "total_neutral", "win_prob_neutral"]].to_string(index=False))


if __name__ == "__main__":
    main()
