# Project 2: NFL Play-by-Play Explorer (Shiny)

An interactive Shiny app to explore NFL play-by-play data (2009–2016/18): filter by team, play type, down & distance, and visualize metrics like yards gained and EPA.

## Purpose
Help users quickly slice, summarize, and visualize NFL play data for basic analysis and class demos.

## Data
- **Source:** Kaggle — NFL Play by Play (2009–2016) by Max Horowitz  
  https://www.kaggle.com/datasets/maxhorowitz/nflplaybyplay2009to2016
- **Local file (not committed):** Place the CSV at `data/nfl_play_by_play_2009_2016.csv` (or your local path in `app.R`).  
  Add large CSVs to `.gitignore`.

## How to Run
```r
# in R
install.packages(c("shiny","bslib","dplyr","tidyr","ggplot2","DT","shinycssloaders","readr"))
shiny::runApp()
