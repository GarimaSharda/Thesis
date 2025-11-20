# --- Libraries ---
library(rvest)
library(dplyr)
library(stringr)
library(readr)
library(purrr)

# --- Helper function to find Transfermarkt ID ---
get_tm_id <- function(player_name) {
  base_url <- "https://www.transfermarkt.com/schnellsuche/ergebnis/schnellsuche"
  query <- URLencode(player_name)
  search_url <- paste0(base_url, "?query=", query)
  
  # Attempt to read the page
  page <- tryCatch(read_html(search_url), error = function(e) NA)
  if (is.na(page)) return(NA)
  
  # Extract first player profile link
  link_nodes <- html_nodes(page, "a[href*='/spieler/']")
  hrefs <- html_attr(link_nodes, "href")
  player_links <- hrefs[str_detect(hrefs, "/spieler/")]
  
  if (length(player_links) == 0) return(NA)
  
  # Extract numeric player ID
  id <- str_extract(player_links[1], "(?<=/spieler/)[0-9]+")
  return(id)
}

# --- Rate-limited wrapper with retries ---
safe_get_tm_id <- function(player_name, retries = 3) {
  for (i in 1:retries) {
    id <- tryCatch(get_tm_id(player_name), error = function(e) NA)
    if (!is.na(id)) return(id)
    Sys.sleep(runif(1, 1, 3))  # wait and retry
  }
  return(NA)
}

# --- MAIN SCRIPT ---
# Load your CSV (update to your actual path)
file_path <- "D:\\thesis data\\premier_league_player_stats_2010_2015.csv"
players <- read_csv(file_path, show_col_types = FALSE)

# Use correct column name
player_names <- unique(players$Player)

# Optional: Display progress
cat("Collecting Transfermarkt IDs for", length(player_names), "players...\n")

# Map over each player with random delay
player_ids <- map_chr(player_names, ~{
  Sys.sleep(runif(1, 1.5, 3))  # random delay to avoid rate-limiting
  cat("Processing:", .x, "\n")
  safe_get_tm_id(.x)
})

# Combine lookup table
tm_lookup <- data.frame(Player = player_names, transfermarkt_id = player_ids)

#checkcountofuniqueplayers
num_unique_players <- players %>%
  distinct(Player) %>%
  nrow()

cat("Number of unique players:", num_unique_players, "\n")

# Merge back with dataset
players_enriched <- left_join(players, tm_lookup, by = "Player")

# Save enriched CSV
output_path <- "D:\\thesis data\\premier_league_player_stats_with_tm_ids.csv"
write_csv(players_enriched, output_path)

message("✅ Transfermarkt IDs appended successfully and saved to:")
message(output_path)
