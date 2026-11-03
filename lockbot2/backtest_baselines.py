"""Walk-forward baselines for Lockbot 2.0.

Each test season is predicted by models trained ONLY on earlier seasons. Hyperparameters
are fixed up front (no tuning on test data). 2026 is also compared against the
DraftKings closing line and KenPom's own fanmatch predictions on identical games.

Usage: python3 -I lockbot2/backtest_baselines.py --data lockbot2/out/games_model.csv
"""
import argparse
import json
from pathlib import Path

import lightgbm as lgb
import numpy as np
import pandas as pd
from scipy.stats import norm
from sklearn.impute import SimpleImputer
from sklearn.linear_model import LinearRegression, RidgeCV
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler

TEST_SEASONS = [2024, 2025, 2026]


def features(d):
    X = pd.DataFrame(index=d.index)
    for c in ["AdjEM", "AdjOE", "AdjDE", "AdjTempo"]:
        X[f"{c}_h"], X[f"{c}_a"] = d[f"{c}_h_eff"], d[f"{c}_a_eff"]
    X["em_diff"] = X.AdjEM_h - X.AdjEM_a
    X["tempo_avg"] = (X.AdjTempo_h + X.AdjTempo_a) / 2
    X["pts_h"] = X.tempo_avg * (X.AdjOE_h + X.AdjDE_a) / 100
    X["pts_a"] = X.tempo_avg * (X.AdjOE_a + X.AdjDE_h) / 100
    for c in ["gp_h", "gp_a", "rest_h", "rest_a", "last5_h", "last5_a", "mom_h", "mom_a",
              "snap_age_h", "snap_age_a", "neutral", "conf_game", "postseason",
              "from_prior_h", "from_prior_a"]:
        X[c] = d[c]
    X["prior_em_h"], X["prior_em_a"] = d.prior_AdjEM_h, d.prior_AdjEM_a
    X["lg_total"] = d.lg_total_30d
    X["doy"] = (pd.to_datetime(d.game_date) - pd.to_datetime((d.season - 1).astype(str) + "-11-01")).dt.days
    return X


def fit_predict(tr, te):
    """Return dict of predictions {model: (margin, total)} for the test frame."""
    Xtr, Xte = features(tr), features(te)
    out = {}
    # B0: KenPom-style formula, coefficients fit on training seasons
    f0m = lambda X: np.c_[X.em_diff * X.tempo_avg / 100, 1 - X.neutral]
    f0t = lambda X: np.c_[X.pts_h, X.pts_a, X.lg_total.fillna(X.lg_total.median())]
    m0 = LinearRegression().fit(f0m(Xtr), tr.margin)
    t0 = LinearRegression().fit(f0t(Xtr), tr.total)
    out["formula"] = (m0.predict(f0m(Xte)), t0.predict(f0t(Xte)))
    # Ridge on all features
    rm = make_pipeline(SimpleImputer(strategy="median"), StandardScaler(), RidgeCV(alphas=np.logspace(-1, 3, 12)))
    rt = make_pipeline(SimpleImputer(strategy="median"), StandardScaler(), RidgeCV(alphas=np.logspace(-1, 3, 12)))
    out["ridge"] = (rm.fit(Xtr, tr.margin).predict(Xte), rt.fit(Xtr, tr.total).predict(Xte))
    # LightGBM, fixed conservative settings
    kw = dict(n_estimators=500, learning_rate=0.02, num_leaves=12, min_child_samples=80,
              subsample=0.8, subsample_freq=1, colsample_bytree=0.8, reg_lambda=10,
              random_state=0, n_jobs=2, verbose=-1)
    gm = lgb.LGBMRegressor(**kw).fit(Xtr, tr.margin)
    gt = lgb.LGBMRegressor(**kw).fit(Xtr, tr.total)
    out["lightgbm"] = (gm.predict(Xte), gt.predict(Xte))
    return out


def mae(a, b):
    return float(np.mean(np.abs(np.asarray(a) - np.asarray(b))))


def ats_table(margin, pred_margin, mkt_spread, thresholds):
    """Simulated ATS betting at -110: bet the side the model likes more than the market does."""
    edge = pred_margin + mkt_spread          # model home margin minus market home margin (= -spread)
    rows = []
    for t in thresholds:
        bet_home, bet_away = edge > t, edge < -t
        cover_home = (margin + mkt_spread) > 0
        push = (margin + mkt_spread) == 0
        sel = (bet_home | bet_away) & ~push
        win = np.where(bet_home, cover_home, ~cover_home)[sel]
        dog = np.where(bet_home, mkt_spread > 0, mkt_spread < 0)[sel]
        n = int(sel.sum())
        rows.append({"edge>=": t, "bets": n, "win%": round(100 * win.mean(), 1) if n else None,
                     "ROI%": round(100 * (win.mean() * 0.9091 - (1 - win.mean())), 1) if n else None,
                     "on_underdog%": round(100 * dog.mean(), 1) if n else None})
    return pd.DataFrame(rows)


def ou_table(total, pred_total, mkt_total, thresholds):
    edge = pred_total - mkt_total
    rows = []
    for t in thresholds:
        over, under = edge > t, edge < -t
        push = total == mkt_total
        sel = (over | under) & ~push
        win = np.where(over, total > mkt_total, total < mkt_total)[sel]
        n = int(sel.sum())
        rows.append({"edge>=": t, "bets": n, "win%": round(100 * win.mean(), 1) if n else None,
                     "ROI%": round(100 * (win.mean() * 0.9091 - (1 - win.mean())), 1) if n else None,
                     "on_over%": round(100 * over[sel].mean(), 1) if n else None})
    return pd.DataFrame(rows)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--data", required=True)
    ap.add_argument("--out", default=None)
    a = ap.parse_args()
    d = pd.read_csv(a.data, parse_dates=["game_date"])
    d = d[d.AdjEM_h_eff.notna() & d.AdjEM_a_eff.notna()].reset_index(drop=True)
    results, preds = {}, {}

    print("=== Walk-forward (train on earlier seasons only), all D1 games ===")
    rows = []
    for ts in TEST_SEASONS:
        tr, te = d[d.season < ts], d[d.season == ts].copy()
        P = fit_predict(tr, te)
        preds[ts] = (te, P)
        for name, (pm, pt) in P.items():
            rows.append({"season": ts, "model": name, "n": len(te),
                         "margin_MAE": round(mae(pm, te.margin), 3),
                         "margin_RMSE": round(float(np.sqrt(np.mean((pm - te.margin) ** 2))), 3),
                         "winner_acc%": round(100 * float(np.mean((pm > 0) == (te.margin > 0))), 1),
                         "total_MAE": round(mae(pt, te.total), 3)})
    res = pd.DataFrame(rows)
    print(res.to_string(index=False))
    results["walk_forward"] = rows

    # ---- 2026: same-game comparison with market and KenPom ----
    te, P = preds[2026]
    S = te.mkt_spread.notna() & te.kp_margin.notna()
    print(f"\n=== 2026, identical games with market spread AND KenPom fanmatch (n={int(S.sum())}) ===")
    cmp_rows = []
    mk_margin = -te.mkt_spread[S]
    cands = {"DraftKings close": (mk_margin, te.mkt_total[S]), "KenPom fanmatch": (te.kp_margin[S], te.kp_total_pred[S])}
    for k, (pm, pt) in P.items():
        cands[f"Lockbot2 {k}"] = (pd.Series(pm, index=te.index)[S], pd.Series(pt, index=te.index)[S])
    St = S & te.mkt_total.notna()
    for name, (pm, pt) in cands.items():
        y = te.margin[S]
        row = {"model": name, "margin_MAE": round(mae(pm, y), 3),
               "winner_acc%": round(100 * float(np.mean((pm > 0) == (y > 0))), 1)}
        ptt = pt.reindex(te.index)[St] if hasattr(pt, "reindex") else None
        row["total_MAE"] = round(mae(ptt, te.total[St]), 3)
        cmp_rows.append(row)
    print(pd.DataFrame(cmp_rows).to_string(index=False))
    results["same_games_2026"] = cmp_rows

    # ---- win probability calibration: sigma estimated from the previous (2025) fold ----
    te25, P25 = preds[2025]
    sig = float(np.std(te25.margin - P25["lightgbm"][0]))
    scale = sig * np.sqrt(3) / np.pi
    pm = pd.Series(P["lightgbm"][0], index=te.index)
    y = te.home_win
    wp_cal = pd.Series(norm.cdf(pm / sig), index=te.index)
    wp_v1 = 1 / (1 + np.exp(-pm / 7.0))                     # Lockbot 1.0's fixed scale
    both = te.mkt_wp_home.notna() & te.kp_wp_home.notna()
    def brier(p, m): return round(float(np.mean((p[m] - y[m]) ** 2)), 4)
    def ll(p, m):
        q = np.clip(p[m], 1e-4, 1 - 1e-4)
        return round(float(-np.mean(y[m] * np.log(q) + (1 - y[m]) * np.log(1 - q))), 4)
    print(f"\n=== 2026 win-probability quality, same games (n={int(both.sum())}) ===")
    print(f"residual sigma from 2025 fold: {sig:.2f} pts -> logistic scale ~{scale:.2f} (Lockbot 1.0 used 7.0)")
    cal = pd.DataFrame({
        "source": ["DraftKings (de-vigged ML)", "KenPom fanmatch", "Lockbot2 lightgbm, calibrated", "Lockbot2 lightgbm, 1.0-style /7.0"],
        "Brier": [brier(te.mkt_wp_home, both), brier(te.kp_wp_home, both), brier(wp_cal, both), brier(wp_v1, both)],
        "LogLoss": [ll(te.mkt_wp_home, both), ll(te.kp_wp_home, both), ll(wp_cal, both), ll(wp_v1, both)]})
    print(cal.to_string(index=False))
    results["winprob_2026"] = {"sigma": sig, "logistic_scale": scale, "table": cal.to_dict("records")}

    # ---- does the model know anything the market doesn't? ----
    T = te[S].copy()
    T["pm"] = pm[S]
    T["mm"] = -T.mkt_spread
    X = np.c_[np.ones(len(T)), T.mm, T.pm - T.mm]
    beta, *_ = np.linalg.lstsq(X, T.margin, rcond=None)
    resid = T.margin - X @ beta
    cov = np.linalg.inv(X.T @ X) * resid.var()
    print(f"\n=== Margin = a + b1*market + b2*(model - market), 2026 (n={len(T)}) ===")
    print(f"b1={beta[1]:.3f}  b2={beta[2]:.3f}  (b2 t-stat {beta[2] / np.sqrt(cov[2, 2]):.2f}; "
          "b2 > 0 means the model's disagreement with the market carries real information)")
    results["information_test"] = {"b1": float(beta[1]), "b2": float(beta[2]), "t_b2": float(beta[2] / np.sqrt(cov[2, 2]))}

    th = [0, 1, 2, 3, 4, 5]
    print("\n=== Simulated ATS betting at -110 (break-even win% = 52.4), Lockbot2 lightgbm, 2026 ===")
    ats = ats_table(T.margin.values, T.pm.values, T.mkt_spread.values, th)
    print(ats.to_string(index=False))
    TT = te[St].copy()
    TT["pt"] = pd.Series(P["lightgbm"][1], index=te.index)[St]
    print("\n=== Simulated over/under betting at -110, Lockbot2 lightgbm, 2026 ===")
    ou = ou_table(TT.total.values, TT.pt.values, TT.mkt_total.values, th)
    print(ou.to_string(index=False))
    results["ats_sim"], results["ou_sim"] = ats.to_dict("records"), ou.to_dict("records")

    if a.out:
        Path(a.out).parent.mkdir(parents=True, exist_ok=True)
        Path(a.out).write_text(json.dumps(results, indent=2, default=float))


if __name__ == "__main__":
    main()
