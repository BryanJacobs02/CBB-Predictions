"""Walk-forward metrics for the shipped ridge+LightGBM blend -> data/eval_v2.json (read by the app)."""
import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd
from scipy.stats import norm

sys.path.insert(0, str(Path(__file__).parent))
from make_cache import fit  # noqa: E402
from backtest_baselines import features  # noqa: E402


def main(data="lockbot2/out/games_model.csv", base="lockbot2/out/backtest_results.json", out="data/eval_v2.json"):
    d = pd.read_csv(data, parse_dates=["game_date"])
    d = d[d.AdjEM_h_eff.notna() & d.AdjEM_a_eff.notna()]
    res = json.load(open(base))
    wf, preds = [], {}
    for s in (2024, 2025, 2026):
        tr, te = d[d.season < s], d[d.season == s]
        mg, tg = fit(tr)
        X = features(te, True)
        pm, pt = mg.predict(X), tg.predict(X)
        preds[s] = te.assign(pm=pm, pt=pt)
        wf.append({"season": s, "model": "blend", "n": len(te),
                   "margin_MAE": round(float(np.abs(te.margin - pm).mean()), 3),
                   "margin_RMSE": round(float(np.sqrt(((te.margin - pm) ** 2).mean())), 3),
                   "winner_acc%": round(float(((pm > 0) == (te.margin > 0)).mean() * 100), 1),
                   "total_MAE": round(float(np.abs(te.total - pt).mean()), 3)})
    t = preds[2026]
    g = t[t.mkt_spread.notna() & t.kp_margin.notna()]
    def row(name, pm_, pt_):
        return {"model": name,
                "margin_MAE": round(float(np.abs(g.margin - pm_).mean()), 3),
                "winner_acc%": round(float(((pm_ > 0) == (g.margin > 0)).mean() * 100), 1),
                "total_MAE": round(float(np.abs(g.total - pt_).mean()), 3)}
    same = [row("DraftKings close", -g.mkt_spread, g.mkt_total),
            row("KenPom fanmatch", g.kp_margin, g.kp_total_pred),
            row("Lockbot 2.0 blend", g.pm, g.pt)]
    sigma = float((preds[2026].margin - preds[2026].pm).std())
    y = g.home_win

    def wp_row(name, p_):
        return {"source": name, "Brier": round(float(((p_ - y) ** 2).mean()), 4),
                "LogLoss": round(float(-(y * np.log(p_) + (1 - y) * np.log(1 - p_)).mean()), 4)}
    tab = [wp_row("DraftKings (de-vigged ML)", g.mkt_wp_home), wp_row("KenPom fanmatch", g.kp_wp_home),
           wp_row("Lockbot 2.0 blend, calibrated", norm.cdf(g.pm / sigma))]
    json.dump({"walk_forward": wf, "same_games_2026": same,
               "winprob_2026": {"sigma": sigma, "table": tab},
               "information_test": res["information_test"]}, open(out, "w"), indent=1)
    print(pd.DataFrame(wf).to_string(index=False)); print(pd.DataFrame(same).to_string(index=False)); print(pd.DataFrame(tab))
    print("games in same-game set:", len(g))


if __name__ == "__main__":
    main()
