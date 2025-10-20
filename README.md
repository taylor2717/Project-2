# Project-2

# Data Explorer — Shiny


Interactive Shiny app to explore a dataset with flexible subsetting, summaries, and plots. Built for the course project; meets all rubric items.


## Quick Start


1. **Add data**: place your CSV at `data/data.csv` (create the folder if it doesn't exist). Columns can be numeric or categorical.
2. **About image**: drop an image in `www/` and update `ABOUT_IMAGE` in `app.R`.
3. **Run locally**:
```r
shiny::runApp()
```
4. **Static notebook**: open `static_explore.qmd` and Render to HTML to generate static summaries/plots used to design the app.


## Features
- Sidebar filters: two categorical multi-selects; two numeric variables with dynamic sliders
- Click **Apply filters** to update the dataset across all tabs
- **About** tab: project purpose, data source link, image
- **Data Download** tab: interactive table + CSV download of current subset
- **Data Exploration** tab:
- Categorical: one-way and two-way tables
- Numeric: summaries by group, plus bar/boxplot/scatter/heatmap with optional color and facet
- Error handling and loading spinners


## Repo Structure
```
.
├── app.R
├── static_explore.qmd
├── README.md
├── data/
│ └── data.csv # your dataset (not tracked if large)
└── www/
└── placeholder.png # replace with your own image
