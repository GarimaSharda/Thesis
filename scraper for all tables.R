library(rvest)
library(dplyr)
library(readr)
library(stringi)
library(stringr)
library(purrr)

make_slug <- function(name) {
  name %>%
    tolower() %>%
    stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[^a-z0-9]", "-") %>%
    str_replace_all("-+", "-")
}

extract_single <- function(nodeset) {
  out <- html_text(nodeset, trim=TRUE)
  if (length(out) == 0) NA else out[1]
}

scrape_tm_player <- function(player_id, player_name, player_slug) {
  Sys.sleep(runif(1, 1, 2))
  url_profile <- paste0("https://www.transfermarkt.com/", player_slug, "/profil/spieler/", player_id)
  page <- tryCatch(read_html(url_profile), error = function(e) NA)
  if (is.na(page)) {
    return(tibble(
      player_name = player_name,
      transfermarkt_id = player_id,
      home_name = NA, dob_age = NA, place_birth = NA, height = NA, citizenship = NA,
      position = NA, preferred_foot = NA, agent = NA, current_club = NA, date_joined = NA,
      contract_expiry = NA, outfitter = NA, shirt_number = NA,
      retired = NA, retirement_date = NA, retirement_year = NA,
      years_career = NA_integer_, retirement_age = NA_integer_,
      debut_date = NA, debut_club = NA, debut_competition = NA, debut_age = NA, first_club = NA,
      last_club = NA, most_games_for = NA,
      injury_count_total = NA, days_missed_total = NA, games_missed_total = NA,
      career_sum_matches_played = NA, career_sum_goals = NA,
      career_sum_assists = NA, career_sum_minutes = NA,
      suspension_type = NA_character_, suspension_days = NA_real_
    ))
  }
  
  safe_extract <- function(xpath) {
    tryCatch(extract_single(html_nodes(page, xpath = xpath)), error = function(e) NA)
  }
  
  # Extract fields for calculations
  dob_raw <- safe_extract('//span[contains(text(),"Date of birth")]/following-sibling::span')
  dob_str <- str_extract(dob_raw, "\\d{2}/\\d{2}/\\d{4}")
  dob_dt <- suppressWarnings(as.Date(dob_str, "%d/%m/%Y"))
  
  # Retirement info
  page_text <- html_text(page)
  retired <- if (str_detect(page_text, "Retired since:")) "Yes" else "No"
  retirement_date <- str_extract(page_text, "Retired since:\\s*\\d{2}/\\d{2}/\\d{4}") %>%
    str_extract("\\d{2}/\\d{2}/\\d{4}")
  retirement_year <- if (!is.na(retirement_date)) as.numeric(substr(retirement_date, 7, 10)) else NA
  retirement_date_dt <- suppressWarnings(as.Date(retirement_date, "%d/%m/%Y"))
  
  # Debut info
  debut_url <- paste0("https://www.transfermarkt.com/", player_slug, "/debuets/spieler/", player_id)
  page_debut <- tryCatch(read_html(debut_url), error = function(e) NA)
  debut_date <- NA_character_
  if (!is.na(page_debut)) {
    debut_table <- tryCatch(html_table(page_debut, fill = TRUE)[[1]], error = function(e) NULL)
    if (!is.null(debut_table) && nrow(debut_table) > 1 && "Date" %in% names(debut_table)) {
      data_rows <- debut_table[-1, ]
      idx <- which(!is.na(data_rows$Date) & data_rows$Date != "")
      if (length(idx) > 0) debut_date <- as.character(data_rows$Date[idx[1]])
    }
  }
  debut_date_dt <- suppressWarnings(as.Date(debut_date, "%d/%m/%Y"))
  
  # Calculate years_career and retirement_age
  today <- Sys.Date()
  years_career <- if (!is.na(debut_date_dt) && !is.na(retirement_date_dt)) {
    as.integer(as.numeric(difftime(retirement_date_dt, debut_date_dt, units = "days")) / 365.25)
  } else {
    NA_integer_
  }
  retirement_age <- if (!is.na(dob_dt) && !is.na(retirement_date_dt)) {
    as.integer(floor(as.numeric(difftime(retirement_date_dt, dob_dt, units = "days")) / 365.25))
  } else if (!is.na(dob_dt)) {
    as.integer(floor(as.numeric(difftime(today, dob_dt, units = "days")) / 365.25))
  } else {
    NA_integer_
  }
  
  # Suspensions/Absences extraction
  ausfaelle_url <- paste0("https://www.transfermarkt.com/", player_slug, "/ausfaelle/spieler/", player_id)
  page_ausfaelle <- tryCatch(read_html(ausfaelle_url), error = function(e) NA)
  suspension_tbl <- tryCatch(html_table(page_ausfaelle, fill = TRUE)[[1]], error = function(e) NULL)
  has_data <- !is.null(suspension_tbl) && nrow(suspension_tbl) > 0 && all(c("Season", "Absence/Suspension", "Days") %in% colnames(suspension_tbl))
  if (has_data) {
    susp_filter <- suspension_tbl %>%
      filter(grepl("suspension|absence", `Absence/Suspension`, ignore.case = TRUE))
    suspension_days <- sum(as.numeric(gsub("[^0-9]", "", susp_filter$Days)), na.rm = TRUE)
    suspension_type <- paste(unique(susp_filter$`Absence/Suspension`), collapse = "; ")
    if (is.nan(suspension_days) || suspension_days == 0) suspension_days <- NA_real_
    if (suspension_type == "") suspension_type <- NA_character_
  } else {
    suspension_type <- NA_character_
    suspension_days <- NA_real_
  }
  
  # All other fields as before
  tibble(
    player_name      = player_name,
    transfermarkt_id = player_id,
    home_name        = safe_extract('//span[contains(text(),"Name in home country")]/following-sibling::span'),
    dob_age          = dob_raw,
    place_birth      = safe_extract('//span[contains(text(),"Place of birth")]/following-sibling::span'),
    height           = safe_extract('//span[contains(text(),"Height")]/following-sibling::span'),
    citizenship      = safe_extract('//span[contains(text(),"Citizenship")]/following-sibling::span'),
    position         = safe_extract('//span[contains(text(),"Position")]/following-sibling::span'),
    preferred_foot   = safe_extract('//span[contains(text(),"Foot")]/following-sibling::span'),
    agent            = safe_extract('//span[contains(text(),"Player agent")]/following-sibling::span'),
    current_club     = {
      nodes <- html_nodes(page, ".data-header__club a")
      if (length(nodes) == 0) NA else html_text(nodes[1], trim = TRUE)
    },
    date_joined      = safe_extract('//span[contains(text(),"Joined")]/following-sibling::span'),
    contract_expiry  = safe_extract('//span[contains(text(),"Contract expires")]/following-sibling::span'),
    outfitter        = safe_extract('//span[contains(text(),"Outfitter")]/following-sibling::span'),
    shirt_number     = NA,
    retired          = retired,
    retirement_date  = retirement_date,
    retirement_year  = retirement_year,
    years_career     = years_career,
    retirement_age   = retirement_age,
    debut_date        = debut_date,
    debut_club        = debut_club,
    debut_competition = debut_competition,
    debut_age         = debut_age,
    first_club        = NA,
    last_club         = last_club,
    most_games_for    = most_games_for,
    injury_count_total = injuries$injury_count_total,
    days_missed_total  = injuries$days_missed_total,
    games_missed_total = injuries$games_missed_total,
    career_sum_matches_played = matches_played,
    career_sum_goals         = goals,
    career_sum_assists       = assists,
    career_sum_minutes       = minutes,
    suspension_type          = suspension_type,
    suspension_days          = suspension_days
  )
}

safe_scrape_tm <- possibly(
  scrape_tm_player,
  otherwise = tibble(
    player_name = NA_character_,
    transfermarkt_id = NA_integer_,
    home_name = NA_character_,
    dob_age = NA_character_,
    place_birth = NA_character_,
    height = NA_character_,
    citizenship = NA_character_,
    position = NA_character_,
    preferred_foot = NA_character_,
    agent = NA_character_,
    current_club = NA_character_,
    date_joined = NA_character_,
    contract_expiry = NA_character_,
    outfitter = NA_character_,
    shirt_number = NA,
    retired = NA_character_,
    retirement_date = NA_character_,
    retirement_year = NA_integer_,
    years_career = NA_integer_,
    retirement_age = NA_integer_,
    debut_date = NA_character_,
    debut_club = NA_character_,
    debut_competition = NA_character_,
    debut_age = NA_character_,
    first_club = NA_character_,
    last_club = NA_character_,
    most_games_for = NA_character_,
    injury_count_total = NA_integer_,
    days_missed_total = NA_integer_,
    games_missed_total = NA_integer_,
    career_sum_matches_played = NA_integer_,
    career_sum_goals = NA_integer_,
    career_sum_assists = NA_integer_,
    career_sum_minutes = NA_integer_,
    suspension_type = NA_character_,
    suspension_days = NA_real_
  )
)


players <- read_csv("D:/thesis data/premier_league_player_stats_cleaned.csv", show_col_types = FALSE) %>%
  slice(1:30) %>%   # Remove or modify to process more
  mutate(playerslug = make_slug(player_name))

scraped_tm <- map2_df(
  players$transfermarkt_id,
  players$playerslug,
  ~ safe_scrape_tm(.x, players$player_name[which(players$transfermarkt_id == .x)[1]], .y)
)

print(head(scraped_tm, 10))
summary(scraped_tm)
str(scraped_tm)
