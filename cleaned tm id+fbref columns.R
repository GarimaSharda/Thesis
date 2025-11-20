rename_and_save_premier_league_csv <- function(input_file, output_file) {
  library(readr)
  library(dplyr)
  
  rename_map <- c(
    player_name         = "Player",
    season2             = "Season",
    nation              = "Nation",
    age                 = "Age",
    rank                = "Rk",
    minutes             = "Min",
    transfermarkt_id    = "transfermarkt_id",
    position            = "Pos",
    current_club        = "Squad",
    dob                 = "Born",
    matches_played      = "MP",
    starts              = "Starts",
    X90s                = "90s",
    goals_12            = "Gls...12",
    assists_13          = "Ast...13",
    goals_assists_14    = "G+A...14",
    goals_PK_15         = "G-PK...15",
    penalty_goals       = "PK",
    penalty_attempts    = "PKatt",
    yellow_cards        = "CrdY",
    red_cards           = "CrdR",
    goals_20            = "Gls...20",
    assists_21          = "Ast...21",
    GA_22               = "G+A...22",
    goals_PK_23         = "G-PK...23",
    goals_assists_minus_PK = "G+A-PK",
    matches             = "Matches",
    goals_against       = "GA",
    goals_against_per_90 = "GA90",
    shots_on_target_against = "SoTA",
    saves               = "Saves",
    save_percentage_31  = "Save%...31",
    wins                = "W",
    draws               = "D",
    losses              = "L",
    clean_sheets        = "CS",
    penalties_against   = "PKA",
    penalty_saves       = "PKsv",
    penalty_misses      = "PKm",
    save_percentage_40  = "Save%...40"
    # Add further mappings as needed
  )
  
  df <- read_csv(input_file, show_col_types = FALSE)
  
  # Only rename those columns that exist in the current data (old names)
  present <- rename_map[names(rename_map) %in% names(rename_map) & rename_map %in% names(df)]
  df_clean <- dplyr::rename(df, !!!present)
  
  write_csv(df_clean, output_file)
  cat("File saved as", output_file, "\n")
}

# Usage:
rename_and_save_premier_league_csv(
  "D:/thesis data/premier_league_player_stats_with_tm_ids.csv",
  "D:/thesis data/premier_league_player_stats_cleaned.csv"
  )
