# Project 2: NFL Play-by-Play Explorer (Shiny)

This Shiny app allows users to explore NFL play-by-play data through **interactive filters**, **summary tables**, and **visualizations**.  
Users can subset the data by team, play type, and numeric variables like yards gained or EPA, then view customized summaries and plots.

**Main Features:**
- Sidebar filters to select categorical and numeric variables.  
- Interactive data table with download option.  
- One-way and two-way contingency tables for categorical variables.  
- Numeric summaries (mean, median, SD) across categories.  
- Multiple plot types (bar, boxplot, scatter, heatmap, violin, 2D bins) with optional color and faceting.

**Data Source:** [Kaggle - NFL Play by Play (2009–2018)](https://www.kaggle.com/datasets/maxhorowitz/nflplaybyplay2009to2016)

**Data Information:**

This project uses the NFL Play-by-Play dataset (originally spanning 2009–2018).  
Because the original CSV file was too large to store in the repository and deploy to [shinyapps.io](https://www.shinyapps.io), the dataset was **subsetted to include only the 2009–2016 seasons**.  

A preprocessing script (`data_prep.R`) extracts the `season` from the `gameid` column, filters the data to the desired years, and saves a smaller `.rds` file used by the Shiny app.

### Static Exploration File
The file `project2.qmd` contains the **static data exploration** for this project.  
It includes one-way and two-way contingency tables, numerical summaries of quantitative variables by categorical groups, and six different plots (including multivariate and faceted visualizations). These analyses were developed prior to building the Shiny app and served as the foundation for the app’s interactive visualizations.




