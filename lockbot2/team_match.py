"""ESPN <-> KenPom team matching for Lockbot 2.0.

Matching is done at the ESPN team-id level (ids are stable; display names are not).
KenPom renames teams between seasons (McNeese St. -> McNeese), so matching is run
once per season against that season's KenPom team list. Passes, in order:
  1. MANUAL_EXTRA below (verified fixes), then the hand-written MANUAL_CROSSWALK in
     R/game_results.R, translated through ALIASES when KenPom's name changed
  2. normalized prefix match ("creighton bluejays" starts with "creighton")
Lockbot 1.0's crosswalk is NOT used: it contained wrong pairs (e.g. Alabama -> Alabama St.).
"""
import re
import unicodedata
from pathlib import Path

import pandas as pd

# ESPN display name -> KenPom name, added after auditing unmatched D1 teams
MANUAL_EXTRA = {
    "Ole Miss Rebels": "Mississippi",
    "Sam Houston Bearkats": "Sam Houston St.",
    "SE Louisiana Lions": "Southeastern Louisiana",
    "Loyola Maryland Greyhounds": "Loyola MD",
    "Omaha Mavericks": "Nebraska Omaha",
    "McNeese Cowboys": "McNeese St.",
    "Nicholls Colonels": "Nicholls St.",
    "Long Island University Sharks": "LIU",
    "Tarleton Texans": "Tarleton St.",
    "UL Monroe Warhawks": "Louisiana Monroe",
    "UT Martin Skyhawks": "Tennessee Martin",
    "Pennsylvania Quakers": "Penn",
    "UAlbany Great Danes": "Albany",
    "Saint Francis Red Wolves": "St. Francis PA",
    "St. Francis (PA) Red Flash": "St. Francis PA",
    "Cal State Northridge Matadors": "Cal St. Northridge",
    "IU Indianapolis Jaguars": "IUPUI",
    "SIU Edwardsville Cougars": "SIUE",
    "Florida International Panthers": "FIU",
    "California Baptist Lancers": "Cal Baptist",
    "South Carolina Upstate Spartans": "USC Upstate",
    "Texas A&M-Corpus Christi Islanders": "Texas A&M Corpus Chris",
    "Kansas City Roos": "UMKC",
    "Utah Tech Trailblazers": "Dixie St.",
    "Houston Christian Huskies": "Houston Baptist",
}

# KenPom names that are the same team under different names in different seasons
ALIASES = [
    ("McNeese St.", "McNeese"), ("Nicholls St.", "Nicholls"), ("IUPUI", "IU Indy"),
    ("Houston Baptist", "Houston Christian"), ("Dixie St.", "Utah Tech"),
    ("Cal St. Northridge", "CSUN"), ("Louisiana Monroe", "UL Monroe"),
    ("Texas A&M Commerce", "East Texas A&M"), ("Albany", "UAlbany"),
    ("St. Francis PA", "Saint Francis"), ("UMKC", "Kansas City"),
]
ALIAS = {}
for grp in ALIASES:
    for a in grp:
        ALIAS[a] = grp


def norm(x):
    x = unicodedata.normalize("NFKD", str(x)).encode("ascii", "ignore").decode().lower()
    x = x.replace("&", " and ")
    x = re.sub(r"[.'`]", "", x)
    x = re.sub(r"[^a-z0-9 ]", " ", x)
    x = re.sub(r"\bst\b", "state", x)
    return re.sub(r"\s+", " ", x).strip()


def read_manual_crosswalk(r_file):
    """Parse the  "ESPN name" = "KenPom name"  pairs out of R/game_results.R."""
    text = Path(r_file).read_text()
    block = text.split("MANUAL_CROSSWALK <- c(", 1)[1].split("\n)", 1)[0]
    return dict(re.findall(r'"([^"]+)"\s*=\s*"([^"]+)"', block))


def build_team_map(sched_d1, kp_names, r_file):
    """One season. Returns (espn_id -> kenpom_name, diagnostics)."""
    teams = pd.concat([
        sched_d1[["game_date", "home_id", "home_display_name"]].set_axis(["d", "id", "name"], axis=1),
        sched_d1[["game_date", "away_id", "away_display_name"]].set_axis(["d", "id", "name"], axis=1),
    ]).sort_values("d").drop_duplicates("id", keep="last")
    games_per_id = pd.concat([sched_d1.home_id, sched_d1.away_id]).value_counts()
    teams["games"] = teams.id.map(games_per_id)

    kp_set = set(kp_names)
    manual = read_manual_crosswalk(r_file)
    manual.update(MANUAL_EXTRA)
    kp_norm = {k: norm(k) for k in kp_names}

    def resolve(k):
        """Return k, or an alias of k that exists in this season's KenPom list."""
        if k in kp_set:
            return k
        for alt in ALIAS.get(k, ()):
            if alt in kp_set:
                return alt
        return None

    mapping, how = {}, {}
    for r in teams.itertuples():
        kp = resolve(manual[r.name]) if r.name in manual else None
        h = "manual"
        if kp is None:
            e = norm(r.name)
            cands = [k for k, kn in kp_norm.items() if e == kn or e.startswith(kn + " ")]
            if cands:
                kp, h = max(cands, key=lambda k: len(kp_norm[k])), "prefix"
        if kp:
            mapping[r.id], how[r.id] = kp, h

    inv = {}
    for i, k in mapping.items():
        inv.setdefault(k, []).append(i)
    conflicts = {k: v for k, v in inv.items() if len(v) > 1}
    unmatched = teams[~teams.id.isin(mapping)].sort_values("games", ascending=False)
    return mapping, {
        "n_espn_teams": len(teams), "n_matched": len(mapping),
        "how": pd.Series(how).value_counts().to_dict(), "conflicts": conflicts,
        "unmatched": unmatched[["id", "name", "games"]],
        "kenpom_teams_without_espn": sorted(kp_set - set(mapping.values())),
    }
