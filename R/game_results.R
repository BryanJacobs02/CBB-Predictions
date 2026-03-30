fetch_season_results <- function(season, max_retries = 3, pause_sec = 2) {
  attempt <- 0
  result  <- NULL
  
  while (attempt < max_retries && is.null(result)) {
    attempt <- attempt + 1
    tryCatch({
      raw <- hoopR::load_mbb_schedule(seasons = season)
      
      if (attempt == 1) {
        message(glue("Available columns: {paste(names(raw), collapse=', ')}"))
      }
      
      home_col <- intersect(c("home_display_name", "home_name",
                              "home_team_display_name"), names(raw))[1]
      away_col <- intersect(c("away_display_name", "away_name",
                              "away_team_display_name"), names(raw))[1]
      
      if (is.na(home_col) || is.na(away_col)) {
        message("Could not find team name columns. Available: ",
                paste(names(raw), collapse=", "))
        return(tibble())
      }
      
      result <- raw |>
        filter(!is.na(home_score), !is.na(away_score),
               status_type_completed == TRUE) |>
        transmute(
          game_id      = as.character(game_id),
          game_date    = as.Date(game_date),
          home_team    = .data[[home_col]],
          away_team    = .data[[away_col]],
          home_score   = as.integer(home_score),
          away_score   = as.integer(away_score),
          neutral_site = neutral_site,
          home_win     = as.integer(home_score > away_score)
        )
      
      Sys.sleep(pause_sec)
    }, error = function(e) {
      message(glue("Season {season} attempt {attempt} failed: {e$message}"))
      if (attempt < max_retries) Sys.sleep(pause_sec * attempt)
    })
  }
  
  if (is.null(result)) {
    warning(glue("Could not fetch season {season} after {max_retries} attempts."))
    return(tibble())
  }
  result
}

load_game_results <- memoise(function(seasons = c(SEASON_YEAR - 2,
                                                  SEASON_YEAR - 1,
                                                  SEASON_YEAR)) {
  current_season <- if (month(Sys.Date()) >= 11) {
    as.integer(format(Sys.Date(), "%Y")) + 1
  } else {
    as.integer(format(Sys.Date(), "%Y"))
  }
  seasons <- seasons[seasons <= current_season]
  
  message(glue("Loading seasons: {paste(seasons, collapse=', ')}"))
  map_dfr(seasons, function(s) {
    message(glue("  Fetching {s}..."))
    fetch_season_results(s)
  }) |>
    distinct(game_id, .keep_all = TRUE) |>
    arrange(game_date)
}, cache = cache)

# ── Manual crosswalk ──────────────────────────────────────────────────────────
MANUAL_CROSSWALK <- c(
  "App State Mountaineers"                 = "Appalachian St.",
  "Appalachian State Mountaineers"         = "Appalachian St.",
  "UTSA Roadrunners"                       = "UT San Antonio",
  "LIU Sharks"                             = "LIU Brooklyn",
  "UTEP Miners"                            = "UTEP",
  "UAB Blazers"                            = "UAB",
  "UConn Huskies"                          = "Connecticut",
  "UMass Minutemen"                        = "Massachusetts",
  "UCF Knights"                            = "Central Florida",
  "UNLV Rebels"                            = "UNLV",
  "USC Trojans"                            = "Southern California",
  "LSU Tigers"                             = "LSU",
  "Louisiana State Tigers"                 = "LSU",
  "SMU Mustangs"                           = "SMU",
  "TCU Horned Frogs"                       = "TCU",
  "Texas Christian Horned Frogs"           = "TCU",
  "VCU Rams"                               = "VCU",
  "VMI Keydets"                            = "VMI",
  "BYU Cougars"                            = "BYU",
  "FIU Panthers"                           = "Florida Intl",
  "FAU Owls"                               = "Florida Atlantic",
  "FGCU Eagles"                            = "FL Gulf Coast",
  "SIU Edwardsville Cougars"               = "SIU Edwardsville",
  "UIW Cardinals"                          = "Incarnate Word",
  "IUPUI Jaguars"                          = "IUPUI",
  "UIC Flames"                             = "Illinois Chicago",
  "UNC Asheville Bulldogs"                 = "UNC Asheville",
  "UNC Greensboro Spartans"                = "UNC Greensboro",
  "UNC Wilmington Seahawks"                = "UNC Wilmington",
  "NC State Wolfpack"                      = "NC State",
  "North Carolina State Wolfpack"          = "NC State",
  "North Carolina Tar Heels"               = "North Carolina",
  "Mississippi Rebels"                     = "Ole Miss",
  "Mississippi St. Bulldogs"               = "Mississippi St.",
  "Miami Hurricanes"                       = "Miami FL",
  "Miami FL Hurricanes"                    = "Miami FL",
  "Miami (OH) Redhawks"                    = "Miami OH",
  "Ohio State Buckeyes"                    = "Ohio St.",
  "Michigan State Spartans"                = "Michigan St.",
  "Kansas State Wildcats"                  = "Kansas St.",
  "Iowa State Cyclones"                    = "Iowa St.",
  "Colorado State Rams"                    = "Colorado St.",
  "Boise State Broncos"                    = "Boise St.",
  "Arizona State Sun Devils"               = "Arizona St.",
  "Arkansas State Red Wolves"              = "Arkansas St.",
  "Ball State Cardinals"                   = "Ball St.",
  "Bowling Green Falcons"                  = "Bowling Green",
  "Florida State Seminoles"                = "Florida St.",
  "Georgia State Panthers"                 = "Georgia St.",
  "Idaho State Bengals"                    = "Idaho St.",
  "Illinois State Redbirds"                = "Illinois St.",
  "Indiana State Sycamores"                = "Indiana St.",
  "Kent State Golden Flashes"              = "Kent St.",
  "McNeese State Cowboys"                  = "McNeese St.",
  "Mississippi Valley State Delta Devils"  = "Mississippi Val.",
  "Montana State Bobcats"                  = "Montana St.",
  "Morgan State Bears"                     = "Morgan St.",
  "Murray State Racers"                    = "Murray St.",
  "New Mexico State Aggies"                = "New Mexico St.",
  "Nicholls State Colonels"                = "Nicholls St.",
  "Norfolk State Spartans"                 = "Norfolk St.",
  "Northwestern State Demons"              = "Northwestern St.",
  "Oklahoma State Cowboys"                 = "Oklahoma St.",
  "Oregon State Beavers"                   = "Oregon St.",
  "Penn State Nittany Lions"               = "Penn St.",
  "Portland State Vikings"                 = "Portland St.",
  "Sacramento State Hornets"               = "Sacramento St.",
  "Sam Houston State Bearkats"             = "Sam Houston St.",
  "San Diego State Aztecs"                 = "San Diego St.",
  "Savannah State Tigers"                  = "Savannah St.",
  "South Carolina State Bulldogs"          = "South Carolina St.",
  "Southeast Missouri State Redhawks"      = "SE Missouri St.",
  "Southeast Missouri Redhawks"            = "SE Missouri St.",
  "Southern Illinois Salukis"              = "Southern Illinois",
  "Stephen F. Austin Lumberjacks"          = "SF Austin",
  "Tarleton State Texans"                  = "Tarleton St.",
  "Tennessee State Tigers"                 = "Tennessee St.",
  "Tennessee Tech Golden Eagles"           = "Tennessee Tech",
  "Texas A&M Corpus Christi Islanders"     = "TAM C. Christi",
  "Texas Southern Tigers"                  = "Texas Southern",
  "Texas State Bobcats"                    = "Texas St.",
  "Utah State Aggies"                      = "Utah St.",
  "Washington State Cougars"               = "Washington St.",
  "Weber State Wildcats"                   = "Weber St.",
  "Wichita State Shockers"                 = "Wichita St.",
  "Winston-Salem State Rams"               = "Winston-Salem St.",
  "Wright State Raiders"                   = "Wright St.",
  "Youngstown State Penguins"              = "Youngstown St.",
  "Albany Great Danes"                     = "Albany",
  "American University Eagles"             = "American",
  "Arkansas Pine Bluff Golden Lions"       = "Arkansas Pine Bluff",
  "Army Black Knights"                     = "Army",
  "Belmont Bruins"                         = "Belmont",
  "Brown Bears"                            = "Brown",
  "Bucknell Bison"                         = "Bucknell",
  "Butler Bulldogs"                        = "Butler",
  "Campbell Camels"                        = "Campbell",
  "Charleston Southern Buccaneers"         = "Charleston So.",
  "Chattanooga Mocs"                       = "Chattanooga",
  "Chicago St. Cougars"                    = "Chicago St.",
  "Cleveland St. Vikings"                  = "Cleveland St.",
  "Coastal Carolina Chanticleers"          = "Coastal Carolina",
  "Columbia Lions"                         = "Columbia",
  "Cornell Big Red"                        = "Cornell",
  "Dartmouth Big Green"                    = "Dartmouth",
  "Davidson Wildcats"                      = "Davidson",
  "Dayton Flyers"                          = "Dayton",
  "Delaware Fightin Blue Hens"             = "Delaware",
  "Denver Pioneers"                        = "Denver",
  "DePaul Blue Demons"                     = "DePaul",
  "Detroit Mercy Titans"                   = "Detroit",
  "Drake Bulldogs"                         = "Drake",
  "Drexel Dragons"                         = "Drexel",
  "Duke Blue Devils"                       = "Duke",
  "Duquesne Dukes"                         = "Duquesne",
  "East Carolina Pirates"                  = "East Carolina",
  "Eastern Illinois Panthers"              = "Eastern Illinois",
  "Eastern Kentucky Colonels"              = "Eastern Kentucky",
  "Eastern Michigan Eagles"                = "Eastern Michigan",
  "Eastern Washington Eagles"              = "Eastern Washington",
  "Elon Phoenix"                           = "Elon",
  "Evansville Purple Aces"                 = "Evansville",
  "Fairfield Stags"                        = "Fairfield",
  "Fairleigh Dickinson Knights"            = "F. Dickinson",
  "Florida Gators"                         = "Florida",
  "Florida A&M Rattlers"                   = "Florida A&M",
  "Fordham Rams"                           = "Fordham",
  "Fresno State Bulldogs"                  = "Fresno St.",
  "Furman Paladins"                        = "Furman",
  "Gardner-Webb Runnin' Bulldogs"          = "Gardner-Webb",
  "George Mason Patriots"                  = "George Mason",
  "George Washington Colonials"            = "George Washington",
  "Georgetown Hoyas"                       = "Georgetown",
  "Georgia Bulldogs"                       = "Georgia",
  "Georgia Southern Eagles"               = "Georgia Southern",
  "Georgia Tech Yellow Jackets"            = "Georgia Tech",
  "Gonzaga Bulldogs"                       = "Gonzaga",
  "Grambling Tigers"                       = "Grambling St.",
  "Grand Canyon Antelopes"                 = "Grand Canyon",
  "Hampton Pirates"                        = "Hampton",
  "Hartford Hawks"                         = "Hartford",
  "Harvard Crimson"                        = "Harvard",
  "Hawaii Rainbow Warriors"                = "Hawaii",
  "High Point Panthers"                    = "High Point",
  "Hofstra Pride"                          = "Hofstra",
  "Holy Cross Crusaders"                   = "Holy Cross",
  "Houston Cougars"                        = "Houston",
  "Howard Bison"                           = "Howard",
  "Idaho Vandals"                          = "Idaho",
  "Illinois Fighting Illini"               = "Illinois",
  "Indiana Hoosiers"                       = "Indiana",
  "Iona Gaels"                             = "Iona",
  "Iowa Hawkeyes"                          = "Iowa",
  "IPFW Mastodons"                         = "Fort Wayne",
  "Purdue Fort Wayne Mastodons"            = "Fort Wayne",
  "Jackson State Tigers"                   = "Jackson St.",
  "Jacksonville Dolphins"                  = "Jacksonville",
  "Jacksonville State Gamecocks"           = "Jacksonville St.",
  "James Madison Dukes"                    = "James Madison",
  "Kansas Jayhawks"                        = "Kansas",
  "Kennesaw State Owls"                    = "Kennesaw St.",
  "Kentucky Wildcats"                      = "Kentucky",
  "Lafayette Leopards"                     = "Lafayette",
  "Lamar Cardinals"                        = "Lamar",
  "Lehigh Mountain Hawks"                  = "Lehigh",
  "Liberty Flames"                         = "Liberty",
  "Lipscomb Bisons"                        = "Lipscomb",
  "Long Beach State Beach"                 = "Long Beach St.",
  "Longwood Lancers"                       = "Longwood",
  "Louisiana Ragin' Cajuns"                = "Louisiana",
  "Louisiana Tech Bulldogs"                = "Louisiana Tech",
  "Louisville Cardinals"                   = "Louisville",
  "Loyola Chicago Ramblers"                = "Loyola Chicago",
  "Loyola Maryland Greyhounds"             = "Loyola Maryland",
  "Loyola Marymount Lions"                 = "Loyola Marymount",
  "Maine Black Bears"                      = "Maine",
  "Manhattan Jaspers"                      = "Manhattan",
  "Marist Red Foxes"                       = "Marist",
  "Marquette Golden Eagles"                = "Marquette",
  "Marshall Thundering Herd"               = "Marshall",
  "Maryland Terrapins"                     = "Maryland",
  "Massachusetts Lowell River Hawks"       = "UMass Lowell",
  "UMass Lowell River Hawks"               = "UMass Lowell",
  "Memphis Tigers"                         = "Memphis",
  "Mercer Bears"                           = "Mercer",
  "Michigan Wolverines"                    = "Michigan",
  "Middle Tennessee Blue Raiders"          = "Middle Tennessee",
  "Milwaukee Panthers"                     = "Milwaukee",
  "Minnesota Golden Gophers"               = "Minnesota",
  "Missouri Tigers"                        = "Missouri",
  "Monmouth Hawks"                         = "Monmouth",
  "Montana Grizzlies"                      = "Montana",
  "Morehead State Eagles"                  = "Morehead St.",
  "Mount St. Mary's Mountaineers"          = "Mt. St. Mary's",
  "Navy Midshipmen"                        = "Navy",
  "Nebraska Cornhuskers"                   = "Nebraska",
  "Nevada Wolf Pack"                       = "Nevada",
  "New Hampshire Wildcats"                 = "New Hampshire",
  "New Mexico Lobos"                       = "New Mexico",
  "New Orleans Privateers"                 = "New Orleans",
  "Niagara Purple Eagles"                  = "Niagara",
  "North Carolina A&T Aggies"              = "North Carolina A&T",
  "North Dakota Fighting Hawks"            = "North Dakota",
  "North Dakota State Bison"               = "North Dakota St.",
  "North Florida Ospreys"                  = "North Florida",
  "North Texas Mean Green"                 = "North Texas",
  "Northeastern Huskies"                   = "Northeastern",
  "Northern Arizona Lumberjacks"           = "Northern Arizona",
  "Northern Colorado Bears"                = "Northern Colorado",
  "Northern Illinois Huskies"              = "Northern Illinois",
  "Northern Iowa Panthers"                 = "Northern Iowa",
  "Northern Kentucky Norse"                = "Northern Kentucky",
  "Northwestern Wildcats"                  = "Northwestern",
  "Notre Dame Fighting Irish"              = "Notre Dame",
  "Oakland Golden Grizzlies"               = "Oakland",
  "Ohio Bobcats"                           = "Ohio",
  "Oklahoma Sooners"                       = "Oklahoma",
  "Old Dominion Monarchs"                  = "Old Dominion",
  "Oral Roberts Golden Eagles"             = "Oral Roberts",
  "Oregon Ducks"                           = "Oregon",
  "Pacific Tigers"                         = "Pacific",
  "Pennsylvania Quakers"                   = "Pennsylvania",
  "Pepperdine Waves"                       = "Pepperdine",
  "Pittsburgh Panthers"                    = "Pittsburgh",
  "Prairie View A&M Panthers"              = "Prairie View",
  "Presbyterian Blue Hose"                 = "Presbyterian",
  "Princeton Tigers"                       = "Princeton",
  "Providence Friars"                      = "Providence",
  "Purdue Boilermakers"                    = "Purdue",
  "Quinnipiac Bobcats"                     = "Quinnipiac",
  "Radford Highlanders"                    = "Radford",
  "Rhode Island Rams"                      = "Rhode Island",
  "Rice Owls"                              = "Rice",
  "Richmond Spiders"                       = "Richmond",
  "Rider Broncs"                           = "Rider",
  "Robert Morris Colonials"                = "Robert Morris",
  "Rutgers Scarlet Knights"                = "Rutgers",
  "Saint Joseph's Hawks"                   = "Saint Joseph's",
  "Saint Louis Billikens"                  = "Saint Louis",
  "Saint Mary's Gaels"                     = "Saint Mary's",
  "Saint Peter's Peacocks"                 = "St. Peter's",
  "San Diego Toreros"                      = "San Diego",
  "San Francisco Dons"                     = "San Francisco",
  "San Jose State Spartans"                = "San Jose St.",
  "Santa Clara Broncos"                    = "Santa Clara",
  "Seattle Redhawks"                       = "Seattle",
  "Seton Hall Pirates"                     = "Seton Hall",
  "Siena Saints"                           = "Siena",
  "South Alabama Jaguars"                  = "South Alabama",
  "South Carolina Gamecocks"               = "South Carolina",
  "South Dakota Coyotes"                   = "South Dakota",
  "South Dakota State Jackrabbits"         = "South Dakota St.",
  "South Florida Bulls"                    = "South Florida",
  "Southeastern Louisiana Lions"           = "SE Louisiana",
  "Southern Jaguars"                       = "Southern",
  "Southern Miss Golden Eagles"            = "Southern Miss",
  "Southern Utah Thunderbirds"             = "Southern Utah",
  "St. Bonaventure Bonnies"                = "St. Bonaventure",
  "St. Francis Brooklyn Terriers"          = "St. Francis NY",
  "St. Francis PA Red Flash"               = "St. Francis PA",
  "St. John's Red Storm"                   = "St. John's",
  "Stanford Cardinal"                      = "Stanford",
  "Stetson Hatters"                        = "Stetson",
  "Stony Brook Seawolves"                  = "Stony Brook",
  "Syracuse Orange"                        = "Syracuse",
  "Temple Owls"                            = "Temple",
  "Tennessee Volunteers"                   = "Tennessee",
  "Texas Longhorns"                        = "Texas",
  "Texas A&M Aggies"                       = "Texas A&M",
  "Texas Tech Red Raiders"                 = "Texas Tech",
  "Toledo Rockets"                         = "Toledo",
  "Towson Tigers"                          = "Towson",
  "Troy Trojans"                           = "Troy",
  "Tulane Green Wave"                      = "Tulane",
  "Tulsa Golden Hurricane"                 = "Tulsa",
  "UC Davis Aggies"                        = "UC Davis",
  "UC Irvine Anteaters"                    = "UC Irvine",
  "UC Riverside Highlanders"               = "UC Riverside",
  "UC San Diego Tritons"                   = "UC San Diego",
  "UC Santa Barbara Gauchos"               = "UC Santa Barbara",
  "UCLA Bruins"                            = "UCLA",
  "UT Arlington Mavericks"                 = "UT Arlington",
  "UT Martin Skyhawks"                     = "UT Martin",
  "UT Rio Grande Valley Vaqueros"          = "UT Rio Grande Valley",
  "Valparaiso Beacons"                     = "Valparaiso",
  "Vanderbilt Commodores"                  = "Vanderbilt",
  "Vermont Catamounts"                     = "Vermont",
  "Villanova Wildcats"                     = "Villanova",
  "Virginia Cavaliers"                     = "Virginia",
  "Virginia Tech Hokies"                   = "Virginia Tech",
  "Wagner Seahawks"                        = "Wagner",
  "Wake Forest Demon Deacons"              = "Wake Forest",
  "Washington Huskies"                     = "Washington",
  "West Virginia Mountaineers"             = "West Virginia",
  "Western Carolina Catamounts"            = "Western Carolina",
  "Western Illinois Leathernecks"          = "Western Illinois",
  "Western Kentucky Hilltoppers"           = "Western Kentucky",
  "Western Michigan Broncos"               = "Western Michigan",
  "William & Mary Tribe"                   = "William & Mary",
  "Winthrop Eagles"                        = "Winthrop",
  "Wisconsin Badgers"                      = "Wisconsin",
  "Wofford Terriers"                       = "Wofford",
  "Wyoming Cowboys"                        = "Wyoming",
  "Xavier Musketeers"                      = "Xavier",
  "Yale Bulldogs"                          = "Yale"
)

# ── Name crosswalk ────────────────────────────────────────────────────────────
build_name_crosswalk <- function(kenpom_names, espn_names) {
  result <- tibble(kenpom = kenpom_names, espn = NA_character_, dist = NA_real_)
  
  manual_hits <- MANUAL_CROSSWALK[espn_names]
  manual_hits <- manual_hits[!is.na(manual_hits)]
  
  for (espn_name in names(manual_hits)) {
    kp_name <- manual_hits[[espn_name]]
    if (kp_name %in% kenpom_names) {
      result$espn[result$kenpom == kp_name] <- espn_name
      result$dist[result$kenpom == kp_name] <- 0
    }
  }
  
  still_unmatched <- result |> dplyr::filter(is.na(espn)) |> dplyr::pull(kenpom)
  
  if (length(still_unmatched) > 0) {
    strip_mascot <- function(x) {
      mascots <- paste0(
        "\\b(wildcats|tigers|bulldogs|eagles|hawks|bears|",
        "lions|wolves|panthers|cougars|falcons|owls|rams|broncos|huskies|",
        "aggies|rebels|trojans|gators|seminoles|knights|cardinals|pirates|",
        "spartans|tar heels|crimson tide|wolverines|hoosiers|huskers|",
        "cornhuskers|buckeyes|longhorns|razorbacks|volunteers|commodores|",
        "demon deacons|mountaineers|hokies|cavaliers|hurricanes|gophers|",
        "badgers|illini|hawkeyes|cyclones|boilermakers|sooners|cowboys|",
        "jayhawks|bearcats|golden bears|golden eagles|blue devils|",
        "blue demons|orange|wave|rainbow warriors|rainbow|anteaters|",
        "tritons|gauchos|highlanders|matadors|49ers|roadrunners|miners|",
        "lumberjacks|colonels|kings|dukes|friars|gaels|bonnies|",
        "peacocks|red storm|billikens|musketeers|flyers|ramblers|",
        "penguins|zips|rockets|chippewas|redhawks|bobcats|",
        "bison|buffalo|bulls|sun devils|salukis|purple aces|purple eagles|",
        "red foxes|leopards|buccaneers|patriots|colonials|jaspers|",
        "scarlet knights|golden rams|golden flashes|golden flash|",
        "fighting irish|fighting illini|fighting hawks|",
        "mean green|green wave|golden hurricane|red raiders|",
        "blue raiders|blue hens|chanticleers|ospreys|mocs|",
        "catamounts|grizzlies|seawolves|retrievers|terrapins|terriers|",
        "utes|running rebels|red wolves|hilltoppers|leathernecks|braves|chiefs)\\b"
      )
      tolower(x) |>
        stringr::str_replace_all(mascots, "") |>
        stringr::str_squish()
    }
    
    espn_stripped <- setNames(sapply(espn_names, strip_mascot), espn_names)
    
    normalize <- function(x) {
      x |>
        tolower() |>
        stringr::str_replace_all("\\bst\\.?\\b", "st") |>
        stringr::str_replace_all("\\.", "") |>
        stringr::str_replace_all("[^a-z0-9 ]", "") |>
        stringr::str_squish()
    }
    
    kp_norm   <- normalize(still_unmatched)
    espn_norm <- normalize(espn_stripped)
    
    fuzzy <- map_dfr(seq_along(still_unmatched), function(j) {
      dists    <- stringdist::stringdist(kp_norm[j], espn_norm, method = "jw")
      best_idx <- which.min(dists)
      tibble(
        kenpom = still_unmatched[j],
        espn   = if (dists[best_idx] < 0.12) espn_names[best_idx]
        else NA_character_,
        dist   = dists[best_idx]
      )
    })
    
    for (i in seq_len(nrow(fuzzy))) {
      if (!is.na(fuzzy$espn[i])) {
        result$espn[result$kenpom == fuzzy$kenpom[i]] <- fuzzy$espn[i]
        result$dist[result$kenpom == fuzzy$kenpom[i]] <- fuzzy$dist[i]
      }
    }
  }
  
  result
}

# ── Training labels with point-in-time features ───────────────────────────────
build_actual_training_labels <- function(graph,
                                         snapshots,
                                         seasons = c(SEASON_YEAR - 2,
                                                     SEASON_YEAR - 1,
                                                     SEASON_YEAR)) {
  results    <- load_game_results(seasons)
  team_names <- graph$team_names
  idx        <- setNames(seq_along(team_names) - 1L, team_names)
  
  if (nrow(results) == 0) stop("No game results loaded.")
  
  espn_teams <- unique(c(results$home_team, results$away_team))
  crosswalk  <- build_name_crosswalk(team_names, espn_teams)
  matched    <- crosswalk |> filter(!is.na(espn))
  
  n_matched   <- nrow(matched)
  n_unmatched <- sum(is.na(crosswalk$espn))
  message(glue("{n_matched} KenPom teams matched, {n_unmatched} unmatched."))
  
  if (n_matched < 50)
    stop(glue("Only {n_matched} teams matched. Check team name formats."))
  
  espn_to_kp <- setNames(matched$kenpom, matched$espn)
  
  games <- results |>
    filter(home_team %in% names(espn_to_kp),
           away_team %in% names(espn_to_kp)) |>
    mutate(
      kp_home = espn_to_kp[home_team],
      kp_away = espn_to_kp[away_team]
    ) |>
    filter(kp_home %in% team_names, kp_away %in% team_names) |>
    arrange(game_date)
  
  message(glue("Building per-game feature vectors for {nrow(games)} games..."))
  
  # safe extractor: handles list columns and length != 1 edge cases
  safe_numeric <- function(df_row, cols) {
    vals <- sapply(cols, function(col) {
      val <- df_row[[col]]
      if (is.list(val)) return(NA_real_)
      v <- suppressWarnings(as.numeric(val))
      if (length(v) != 1) return(NA_real_)
      v
    })
    vals[is.na(vals)] <- 0
    vals
  }
  
  labels <- map_dfr(seq_len(nrow(games)), function(i) {
    row      <- games[i, ]
    snap_key <- get_snapshot_for_date(row$game_date, snapshots)
    
    if (is.null(snap_key)) return(tibble())
    
    snap <- snapshots[[snap_key]]
    
    # Only atomic numeric columns — exclude list columns
    numeric_cols <- names(snap)[sapply(snap, function(col) {
      is.numeric(col) && !is.list(col)
    })]
    
    home_row <- snap |> filter(TeamName == row$kp_home) |> slice(1)
    away_row <- snap |> filter(TeamName == row$kp_away) |> slice(1)
    
    if (nrow(home_row) == 0 || nrow(away_row) == 0) return(tibble())
    
    home_feats <- safe_numeric(home_row, numeric_cols)
    away_feats <- safe_numeric(away_row, numeric_cols)
    
    tibble(
      team_a_idx = idx[row$kp_home],
      team_b_idx = idx[row$kp_away],
      winner     = as.integer(row$home_win),
      score_a    = row$home_score,
      score_b    = row$away_score,
      game_date  = as.character(row$game_date),
      neutral    = as.integer(row$neutral_site),
      feats_a    = as.character(jsonlite::toJSON(home_feats)),
      feats_b    = as.character(jsonlite::toJSON(away_feats))
    )
  })
  
  message(glue("Training labels: {nrow(labels)} games with point-in-time features."))
  labels
}