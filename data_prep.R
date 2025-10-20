# This script preprocesses the NFL play-by-play dataset by:
#  - Loading the large CSV file
#  - Extracting the season from `gameid`
#  - Subsetting from 2009–2018 down to 2009–2016 (to reduce file size for deployment)
#  - Selecting only relevant columns
#  - Saving as a compact RDS file for use in the Shiny app

library(readr)
library(dplyr)
library(stringr)

# 1. Load the large CSV
big_file <- "~/Downloads/NFL Play by Play 2009-2018 (v5).csv"
raw <- read_csv(big_file, show_col_types = FALSE)

# 2. Extract the season year from the first 4 digits of gameid
raw <- raw %>%
  mutate(season = as.numeric(str_sub(game_id, 1, 4)))

# 3. Filter the dataset to only 2009-2016
filtered <- raw %>%
  filter(season >= 2009, season <= 2016)

# 4. Select only useful columns 
keep_cols <- c("gameid", "season", "qtr", "down", "ydstogo", "yardline_100",
               "posteam", "defteam", "home_team", "away_team",
               "play_type", "pass", "rush", "air_yards",
               "yards_gained", "epa", "wp", "home_score", "away_score", "drive")

filtered <- filtered %>%
  select(any_of(keep_cols))

# 5. Save as RDS
dir.create("project2/data", recursive = TRUE, showWarnings = FALSE)
saveRDS(filtered, "project2/data/nfl_pbp_2009_2016.rds")

cat("RDS file created at project2/data/nfl_pbp_2009_2016.rds\n")

