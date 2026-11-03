"""Turn raw The Odds API snapshots (scripts/pull_odds_api.R) into an odds file the pipeline can read.

For every game, uses the LAST snapshot taken before tip-off (a closing-line proxy) and DraftKings'
spread and total from it. Games are matched to ESPN game_ids by date (+-1 day) and normalized team names.

Usage:
  python3 -I lockbot2/odds_api_merge.py --raw lockbot_oddsapi/raw --export-dir lockbot_export \
      --out lockbot_oddsapi/odds_2025.csv
Then:  python3 lockbot2/build_dataset.py ... --odds lockbot_oddsapi/odds_2025.csv,<other odds csvs>
"""
import argparse
import json
import sys
from pathlib import Path

import pandas as pd

sys.path.insert(0, str(Path(__file__).parent))
from team_match import norm  # noqa: E402


def parse_snapshots(raw_dir):
    rows = []
    for f in sorted(Path(raw_dir).glob("*.json")):
        j = json.loads(f.read_text())
        snap = pd.Timestamp(j["timestamp"])
        for e in j.get("data", []):
            commence = pd.Timestamp(e["commence_time"])
            if snap >= commence:
                continue                                    # line taken after tip-off: ignore
            dk = next((b for b in e.get("bookmakers", []) if b["key"] == "draftkings"), None)
            if dk is None:
                continue
            r = {"event_id": e["id"], "commence": commence, "snap": snap,
                 "home": e["home_team"], "away": e["away_team"]}
            for m in dk["markets"]:
                if m["key"] == "spreads":
                    o = next((x for x in m["outcomes"] if x["name"] == e["home_team"]), None)
                    if o:
                        r["spread_home"], r["spread_price"] = o["point"], o["price"]
                elif m["key"] == "totals":
                    o = next((x for x in m["outcomes"] if x["name"] == "Over"), None)
                    if o:
                        r["total"], r["total_price"] = o["point"], o["price"]
            rows.append(r)
    return pd.DataFrame(rows)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--raw", required=True)
    ap.add_argument("--export-dir", required=True)
    ap.add_argument("--out", required=True)
    a = ap.parse_args()

    snaps = parse_snapshots(a.raw)
    if snaps.empty:
        sys.exit("no usable snapshots found")
    # last snapshot before tip for each event; first snapshot = "open" proxy
    snaps = snaps.sort_values("snap")
    close = snaps.groupby("event_id").tail(1).set_index("event_id")
    first = snaps.groupby("event_id").head(1).set_index("event_id")
    close["spread_open"], close["total_open"] = first.spread_home, first.total
    close = close.reset_index()
    close["date_et"] = close.commence.dt.tz_convert("America/New_York").dt.normalize().dt.tz_localize(None)
    close["key"] = close.home.map(norm) + "|" + close.away.map(norm)

    s = pd.read_csv(Path(a.export_dir) / "schedule_extra.csv", dtype={"game_id": str})
    s["game_date"] = pd.to_datetime(s.game_date, utc=True, errors="coerce").dt.tz_convert("America/New_York") \
        .dt.normalize().dt.tz_localize(None)
    s["key"] = s.home_display_name.map(norm) + "|" + s.away_display_name.map(norm)
    s = s.drop_duplicates("game_id")

    m = close.merge(s[["game_id", "key", "game_date"]], on="key", how="left")
    m = m[(m.game_date - m.date_et).abs() <= pd.Timedelta(days=1)]
    m = m.sort_values("snap").drop_duplicates("game_id", keep="last")
    matched = len(m)
    out = pd.DataFrame({
        "game_id": m.game_id,
        "has_odds": "TRUE",
        "point_spread_home_close_line": m.spread_home,
        "point_spread_home_open_line": m.spread_open,
        "total_over_close_line": "o" + m.total.astype(str),
        "total_over_open_line": "o" + m.total_open.astype(str),
        "moneyline_home_close_odds": "",
        "moneyline_away_close_odds": "",
    })
    Path(a.out).parent.mkdir(parents=True, exist_ok=True)
    out.to_csv(a.out, index=False)
    print(f"{len(close)} odds events -> {matched} matched to ESPN games ({100 * matched / len(close):.1f}%) -> {a.out}")
    bad = close[~close.event_id.isin(set(close.event_id[close.key.isin(s.key)]))]
    if len(bad):
        print("most common unmatched names (add to team_match.MANUAL_EXTRA if D1 teams):")
        print(pd.concat([bad.home, bad.away]).value_counts().head(15).to_string())


if __name__ == "__main__":
    main()
