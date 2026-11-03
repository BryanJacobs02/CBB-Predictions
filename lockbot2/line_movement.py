"""Does the model beat the opening line, the closing line, or neither? (2026, DraftKings via ESPN)

Run from the project root after build_dataset.py with --odds:  python3 -I lockbot2/line_movement.py
Findings are summarized in lockbot2/README.md and drive the defaults in R/betting_v2.R.
"""
import sys; sys.path.insert(0,"lockbot2")
import numpy as np, pandas as pd, statsmodels.api as sm
from scipy.stats import norm
from make_cache import fit
from backtest_baselines import features
d=pd.read_csv("lockbot2/out/games_model.csv",parse_dates=["game_date"])
d=d[d.AdjEM_h_eff.notna()&d.AdjEM_a_eff.notna()]
tr,te=d[d.season<2026],d[d.season==2026]
mg,tg=fit(tr); X=features(te,True)
te=te.assign(pm=mg.predict(X),pt=tg.predict(X))
o=te[te.mkt_spread.notna()&te.mkt_spread_open.notna()&te.mkt_total.notna()&te.mkt_total_open.notna()].copy()
print("games with open+close:",len(o))
# 1. does the model anticipate line movement? (spread, home-margin terms: line = -spread)
mk_open,mk_close=-o.mkt_spread_open,-o.mkt_spread
move=mk_close-mk_open; gap=o.pm-mk_open
r=sm.OLS(move,sm.add_constant(gap)).fit(cov_type="HC1")
print(f"SPREAD  move on (model-open): coef {r.params.iloc[1]:.3f} se {r.bse.iloc[1]:.3f} t {r.tvalues.iloc[1]:.1f}; sd(move)={move.std():.2f}")
tm,tgap=o.mkt_total-o.mkt_total_open,o.pt-o.mkt_total_open
r2=sm.OLS(tm,sm.add_constant(tgap)).fit(cov_type="HC1")
print(f"TOTAL   move on (model-open): coef {r2.params.iloc[1]:.3f} se {r2.bse.iloc[1]:.3f} t {r2.tvalues.iloc[1]:.1f}; sd(move)={tm.std():.2f}")
# 2. result vs OPEN line: is the model better against open than against close?
for nm,lineH in (("open",mk_open),("close",mk_close)):
    r3=sm.OLS(o.margin-lineH,sm.add_constant(o.pm-lineH)).fit(cov_type="HC1")
    print(f"actual-{nm} on (model-{nm}): coef {r3.params.iloc[1]:.3f} se {r3.bse.iloc[1]:.3f} t {r3.tvalues.iloc[1]:.1f}")
# 3. the shipped betting rule, bets on spread at -110, various trust weights
def sim(weight,sig=11.2,min_ev=0.02):
    edge=o.pm-mk_close                          # model home margin minus market home margin
    p_home=norm.cdf(weight*edge/sig); 
    ev_h=p_home*0.9091-(1-p_home); ev_a=(1-p_home)*0.9091-p_home
    bet_h=(ev_h>=min_ev); bet_a=(ev_a>=min_ev)
    cover_h=(o.margin-mk_close)>0; push=(o.margin-mk_close)==0
    sel=(bet_h|bet_a)&~push
    win=np.where(bet_h,cover_h,~cover_h)[sel]
    n=int(sel.sum()); 
    return n, (win.mean() if n else np.nan), (win.mean()*0.9091-(1-win.mean()) if n else np.nan)
print("\nshipped rule on 2026, ATS at -110 (min EV 2%):")
for w in (0.10,0.20,0.30,0.50,1.0):
    n,wr,roi=sim(w); print(f"  weight {w:.2f}: bets {n:4d}  win% {100*wr:5.1f}  ROI {100*roi:+5.1f}%")

print("\n=== betting against the OPEN line (bet at open, graded vs open line), -110 ===")
def sim_line(line_h, pred, actual, weight, sig, min_ev=0.02, kind="spread"):
    edge=pred-line_h
    p_over=norm.cdf(weight*edge/sig)
    ev_o=p_over*0.9091-(1-p_over); ev_u=(1-p_over)*0.9091-p_over
    bo,bu=ev_o>=min_ev,ev_u>=min_ev
    res=actual-line_h; push=res==0
    sel=(bo|bu)&~push
    win=np.where(bo,res>0,res<0)[sel]; n=int(sel.sum())
    return n,(win.mean() if n else np.nan)
for nm,line,pred,act,sig in (("spread",mk_open,o.pm,o.margin,11.2),("total",o.mkt_total_open,o.pt,o.total,16.8)):
    print(nm)
    for w in (0.2,0.3,0.4,0.5,0.7):
        n,wr=sim_line(line,pred,act,w,sig); roi=wr*0.9091-(1-wr)
        print(f"  weight {w:.1f}: bets {n:4d}  win% {100*wr:5.1f}  ROI {100*roi:+5.1f}%")
# threshold view on raw edge, open spread
print("spread vs OPEN by raw edge (pts):")
edge=(o.pm-mk_open); res=o.margin-mk_open
for t in (1,2,3,4,5):
    sel=(edge.abs()>=t)&(res!=0); w=np.where(edge>0,res>0,res<0)[sel]
    print(f"  |edge|>={t}: bets {int(sel.sum()):4d}  win% {100*w.mean():5.1f}")
print("total vs OPEN by raw edge (pts):")
edge=(o.pt-o.mkt_total_open); res=o.total-o.mkt_total_open
for t in (1,2,3,4,5):
    sel=(edge.abs()>=t)&(res!=0); w=np.where(edge>0,res>0,res<0)[sel]
    print(f"  |edge|>={t}: bets {int(sel.sum()):4d}  win% {100*w.mean():5.1f}")
# month check: stability of the open-line signal
o["mon"]=o.game_date.dt.to_period("M")
print("open-line spread coef by month:")
for m,g in o.groupby("mon"):
    if len(g)<300: continue
    r=sm.OLS(g.margin-(-g.mkt_spread_open),sm.add_constant(g.pm-(-g.mkt_spread_open))).fit(cov_type="HC1")
    print("  ",m,len(g),round(r.params.iloc[1],2))

print("\n=== calibration vs OPEN line ===")
for nm,line,pred,act in (("spread",mk_open,o.pm,o.margin),("total",o.mkt_total_open,o.pt,o.total)):
    r=sm.OLS(act-line,sm.add_constant(pred-line)).fit(cov_type="HC1")
    print(f"{nm}: weight {r.params.iloc[1]:.3f} (se {r.bse.iloc[1]:.3f}), intercept {r.params.iloc[0]:+.2f}, resid sd vs open {(act-line).std():.2f}, resid sd after model {r.resid.std():.2f}")
# how far apart are open and close, how often is the open line missing/odd
print("open==close share (spread):", (mk_open==mk_close).mean().round(3), "| |model-open| median", (o.pm-mk_open).abs().median().round(2))
# split by market size proxy: |close spread| large = lopsided games
o["big"]=o.mkt_spread.abs()>=10
for b,g in o.groupby("big"):
    r=sm.OLS(g.margin+g.mkt_spread_open,sm.add_constant(g.pm+g.mkt_spread_open)).fit(cov_type="HC1")
    print("lopsided (|spread|>=10):" if b else "closer games:", len(g), "weight vs open", round(r.params.iloc[1],2))
