"""One command to rebuild the Lockbot 2.0 prediction cache from fresh data.

Run AFTER scripts/refresh_data.R (which pulls the current season's KenPom + ESPN data):

    python3 lockbot2/refresh.py                  # rebuild everything for the current season
    python3 lockbot2/refresh.py --eval           # also refresh data/eval_v2.json (slower)
    python3 lockbot2/refresh.py --season 2027 --asof 2026-12-01   # pretend it is another date

Steps: download newest ESPN team + player box scores (GitHub) -> rebuild the modeling table -> retrain the
ridge+LightGBM blend on every completed game -> write data/predictions_v2.csv (+ .meta.json).
Then commit data/, and deploy (R: deploy_lockbot()).
"""
import argparse
import datetime as dt
import subprocess
import sys
import urllib.error
import urllib.request
from pathlib import Path

HERE = Path(__file__).parent
ROOT = HERE.parent
BOX_URL = ("https://github.com/sportsdataverse/sportsdataverse-data/releases/download/"
           "espn_mens_college_basketball_team_boxscores/team_box_{y}.parquet")


def current_season(today=None):
    today = today or dt.date.today()
    return today.year + 1 if today.month >= 11 else today.year


def download_box(box_dir, season, first=2022):
    box_dir.mkdir(parents=True, exist_ok=True)
    for y in range(first, season + 1):
        f = box_dir / f"team_box_{y}.parquet"
        if f.exists() and y < season:
            continue                                   # finished seasons never change
        try:
            urllib.request.urlretrieve(BOX_URL.format(y=y), f)
            print(f"  box scores {y}: {f.stat().st_size / 1e6:.1f} MB")
        except urllib.error.HTTPError as e:
            if y == season:
                print(f"  box scores {y}: not published yet ({e.code}); continuing without them")
                f.unlink(missing_ok=True)
            else:
                raise


PLAYER_URL = ("https://github.com/sportsdataverse/sportsdataverse-data/releases/download/"
              "espn_mens_college_basketball_player_boxscores/player_box_{y}.parquet")


def download_players(player_dir, season, first=2022):
    """Player box scores feed the preseason projection (roster turnover). Optional: failures are not fatal."""
    player_dir.mkdir(parents=True, exist_ok=True)
    for y in range(first, season + 1):
        f = player_dir / f"player_box_{y}.parquet"
        if f.exists() and y < season:
            continue
        try:
            urllib.request.urlretrieve(PLAYER_URL.format(y=y), f)
            print(f"  player box scores {y}: {f.stat().st_size / 1e6:.1f} MB")
        except urllib.error.HTTPError as e:
            print(f"  player box scores {y}: not available ({e.code}); preseason projection may be skipped")
            f.unlink(missing_ok=True)


def run(cmd):
    print("\n$", " ".join(str(c) for c in cmd))
    subprocess.run([str(c) for c in cmd], check=True, cwd=ROOT)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--export-dir", default=str(ROOT / "lockbot_export"))
    ap.add_argument("--box-dir", default=str(ROOT / "lockbot_box"))
    ap.add_argument("--player-dir", default=str(ROOT / "lockbot_players"))
    ap.add_argument("--season", type=int, default=current_season())
    ap.add_argument("--asof", default=None, help="YYYY-MM-DD (default: day after the last completed game)")
    ap.add_argument("--odds", default=None, help="optional odds csv(s), comma-separated (evaluation only)")
    ap.add_argument("--eval", action="store_true", help="also rewrite data/eval_v2.json")
    a = ap.parse_args()

    print(f"Season {a.season}")
    download_box(Path(a.box_dir), a.season)
    download_players(Path(a.player_dir), a.season)

    table = ROOT / "lockbot2" / "out" / "games_model.csv"
    cmd = [sys.executable, "-I", HERE / "build_dataset.py", "--export-dir", a.export_dir,
           "--box-dir", a.box_dir, "--preseason-dir", a.player_dir, "--out", table]
    if a.odds:
        cmd += ["--odds", a.odds]
    run(cmd)

    cmd = [sys.executable, "-I", HERE / "make_cache.py", "--data", table, "--export-dir", a.export_dir,
           "--box-dir", a.box_dir, "--player-dir", a.player_dir, "--season", a.season, "--out", ROOT / "data" / "predictions_v2.csv"]
    if a.asof:
        cmd += ["--asof", a.asof]
    run(cmd)

    if a.eval:
        run([sys.executable, "-I", HERE / "eval_blend.py"])

    print("\nDone. To publish:\n"
          "  git add data/ && git commit -m 'Refresh predictions' && git push\n"
          "  then in R:  source('global.R'); deploy_lockbot()")


if __name__ == "__main__":
    main()
