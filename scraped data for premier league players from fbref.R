library(rvest)
library(xml2)
library(stringr)
library(dplyr)

years <- 2010:2015
all_player_stats <- list() # collect dataframes in a list

for (year in years) {
  season_urls <- c(
    paste0(
      "https://fbref.com/en/comps/9/", year, "-", year + 1,
      "/stats/", year, "-", year + 1,
      "-Premier-League-Stats"
    ),
    paste0(
      "https://fbref.com/en/comps/9/", year, "-", year + 1,
      "/keepers/", year, "-", year + 1,
      "-Premier-League-Stats"
    )
  )
  for (season_url in season_urls) {
    cat("Processing:", season_url, "\n")
    page <- tryCatch(read_html(season_url), error=function(e) NULL)
    if (is.null(page)) next
    
    comments <- page %>% html_nodes(xpath = "//comment()") %>% html_text()
    for (comment in comments) {
      tbl_html <- tryCatch(read_html(comment), error=function(e) NULL)
      if (!is.null(tbl_html)) {
        tables_in_comment <- tbl_html %>% html_nodes("table")
        for (tbl in tables_in_comment) {
          df <- tbl %>% html_table(fill = TRUE)
          candidate_row <- which(apply(df, 1, function(r) any(grepl("^Player$", r))))
          if (length(candidate_row) > 0) {
            header_row <- candidate_row[1]
            colnames(df) <- as.character(unlist(df[header_row,]))
            df <- df[-c(1:header_row),]
            if ("Player" %in% names(df)) {
              df$Season <- paste0(year, "-", year + 1)
              all_player_stats[[length(all_player_stats)+1]] <- df
            }
          }
        }
      }
    }
  }
}

# Use dplyr::bind_rows to combine (will fill NAs for missing columns)
all_player_stats_df <- bind_rows(all_player_stats)
all_player_stats_df <- all_player_stats_df[!is.na(all_player_stats_df$Player) & all_player_stats_df$Player != "", ]

write.csv(all_player_stats_df, "D:/thesis data/premier_league_player_stats_2010_2015.csv", row.names = FALSE)
cat("Exported all player stats for 2010-2011 to 2015-2016 to 'premier_league_player_stats_2010_2015.csv'\n")
