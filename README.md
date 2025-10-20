# Project 2: NFL Play-by-Play Explorer (Shiny)

This Shiny app allows users to explore NFL play-by-play data through **interactive filters**, **summary tables**, and **visualizations**.  
Users can subset the data by team, play type, and numeric variables like yards gained or EPA, then view customized summaries and plots.

**Main Features:**
- Sidebar filters to select categorical and numeric variables.  
- Interactive data table with download option.  
- One-way and two-way contingency tables for categorical variables.  
- Numeric summaries (mean, median, SD) across categories.  
- Multiple plot types (bar, boxplot, scatter, heatmap) with optional color and faceting.

**Data Source:** [Kaggle - NFL Play by Play (2009–2018)](https://www.kaggle.com/datasets/maxhorowitz/nflplaybyplay2009to2016)

To run the app:
```r
shiny::runApp()

