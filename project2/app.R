# ---- Packages ----
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rlang)
library(stringr)
library(shinycssloaders)
library(DT)
library(readr)

# ---- Data load ----
DATA_PATH <- "data/nfl_pbp_2009_2016.rds"
NFL_Data <- readRDS(DATA_PATH)

ABOUT_IMAGE <- "nfl_logo.png"  

# ---- Helpers ----
is_numeric_like <- function(x){
  is.numeric(x) || (is.character(x) && suppressWarnings(!any(is.na(as.numeric(x)))))
}
num_range <- function(v){
  vnum <- suppressWarnings(as.numeric(v))
  c(floor(min(vnum, na.rm=TRUE)), ceiling(max(vnum, na.rm=TRUE)))
}

# ---- Light NFL preprocessing ----
if ("pass" %in% names(NFL_Data) || "rush" %in% names(NFL_Data)) {
  if (!"pass_rush" %in% names(NFL_Data)) {
    NFL_Data$pass_rush <- dplyr::case_when(
      ( "pass" %in% names(NFL_Data) && NFL_Data$pass == 1 ) ~ "Pass",
      ( "rush" %in% names(NFL_Data) && NFL_Data$rush == 1 ) ~ "Rush",
      TRUE ~ "Other"
    )
  }
}
for (col in c("down","qtr","play_type")){
  if (col %in% names(NFL_Data)) NFL_Data[[col]] <- as.factor(NFL_Data[[col]])
}

# ---- Variable candidates ----
NUM_CANDIDATES <- names(NFL_Data)[vapply(NFL_Data, is_numeric_like, logical(1))]
CAT_CANDIDATES <- setdiff(names(NFL_Data), NUM_CANDIDATES)

# Fallbacks
if (length(CAT_CANDIDATES) < 2){
  if (length(NUM_CANDIDATES) >= 1){
    NFL_Data[[".auto_cat1"]] <- cut(suppressWarnings(as.numeric(NFL_Data[[NUM_CANDIDATES[1]]])), breaks = 3)
    CAT_CANDIDATES <- c(CAT_CANDIDATES, ".auto_cat1")
  }
}
if (length(CAT_CANDIDATES) < 2){
  NFL_Data[[".auto_cat2"]] <- sample(LETTERS[1:3], nrow(NFL_Data), replace = TRUE)
  CAT_CANDIDATES <- c(CAT_CANDIDATES, ".auto_cat2")
}
if (length(NUM_CANDIDATES) < 2){
  NFL_Data[[".auto_num2"]] <- seq_len(nrow(NFL_Data))
  NUM_CANDIDATES <- unique(c(NUM_CANDIDATES, ".auto_num2"))
}

# ---- Base data used app-wide ----
BASE <- NFL_Data

# ---- UI ----
ui <- page_fillable(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "Data Explorer — Shiny",
  layout_columns(
    col_widths = c(3, 9),
    
    # Sidebar -----------------------------------------------------
    card(
      full_screen = FALSE,
      card_header("Filters"),
      card_body(
        helpText("Choose filters, then click 'Apply filters' to update the app."),
        
        # Categorical filters (multiselect)
        selectInput(
          "cat_var1", "Categorical variable 1:",
          choices = CAT_CANDIDATES,
          selected = if ("posteam" %in% CAT_CANDIDATES) "posteam" else CAT_CANDIDATES[1]
        ),
        uiOutput("cat_levels1"),
        hr(),
        selectInput(
          "cat_var2", "Categorical variable 2:",
          choices = CAT_CANDIDATES,
          selected = if ("play_type" %in% CAT_CANDIDATES) "play_type"
          else if (length(CAT_CANDIDATES) >= 2) CAT_CANDIDATES[2] else CAT_CANDIDATES[1]
        ),
        uiOutput("cat_levels2"),
        hr(),
        
        # Numeric variable 1 with dynamic slider
        selectInput(
          "num_var1", "Numeric variable 1:",
          choices = NUM_CANDIDATES,
          selected = if ("yards_gained" %in% NUM_CANDIDATES) "yards_gained" else NUM_CANDIDATES[1]
        ),
        uiOutput("num_slider1"),
        hr(),
        
        # Numeric variable 2 with dynamic slider
        selectInput(
          "num_var2", "Numeric variable 2:",
          choices = NUM_CANDIDATES,
          selected = if ("epa" %in% NUM_CANDIDATES) "epa"
          else if (length(NUM_CANDIDATES) >= 2) NUM_CANDIDATES[2] else NUM_CANDIDATES[1]
        ),
        uiOutput("num_slider2"),
        hr(),
        
        actionButton("apply", "Apply filters", class = "btn-primary"),
        helpText("Data updates only when you click this button.")
      )
    ),
    
    # Main Panel --------------------------------------------------
    card(
      full_screen = TRUE,
      navset_tab(
        id = "main_tabs",
        
        nav_panel(
          "About", value = "about",
          layout_columns(
            card(
              card_body(
                h4("Purpose"),
                p("This app lets you explore a dataset via interactive filtering and summaries."),
                h4("Data"),
                p(HTML("NFL play-by-play (2009–2016) from Kaggle. <a href='https://www.kaggle.com/datasets/maxhorowitz/nflplaybyplay2009to2016' target='_blank'>Dataset page</a>. Columns include teams, downs, play types, yards gained, EPA, and more.")),
                h4("Navigation"),
                tags$ul(
                  tags$li(HTML("<b>Sidebar</b>: choose subsets by categories and numeric ranges, then click <i>Apply filters</i>.")),
                  tags$li(HTML("<b>Data Download</b>: preview and download the (sub)set as CSV.")),
                  tags$li(HTML("<b>Data Exploration</b>: build contingency tables, summary stats, and plots with faceting."))
                ),
                h4("Note"),
                p("Click button in bottom right hand corner to expand data/plot(s).")
              )
            ),
            card(
              card_body(
                tags$img(src = ABOUT_IMAGE, alt = "About image",
                         style = "max-width:100%; height:auto; border-radius:12px; box-shadow: 0 2px 10px rgba(0,0,0,0.2);")
              )
            )
          )
        ),
        
        nav_panel(
          "Data Download", value = "download",
          layout_columns(
            card(
              card_header("Preview"),
              card_body(
                withSpinner(DTOutput("tbl"), type = 6)
              )
            ),
            card(
              card_header("Download"),
              card_body(
                downloadButton("dl", "Download CSV")
              )
            )
          )
        ),
        
        nav_panel(
          "Data Exploration", value = "explore",
          layout_columns(
            col_widths = c(4, 8),
            
            # Controls
            card(
              card_header("Exploration Controls"),
              card_body(
                radioButtons(
                  "explore_mode", "Show:",
                  choices = c("Categorical summaries", "Numeric summaries + plots"),
                  inline = FALSE
                ),
                
                # Categorical summaries controls
                conditionalPanel(
                  condition = "input.explore_mode == 'Categorical summaries'",
                  selectInput("cat_oneway", "One-way table: choose variable",
                              choices = CAT_CANDIDATES, selected = CAT_CANDIDATES[1]),
                  selectInput("cat_twoway_rows", "Two-way table: rows",
                              choices = CAT_CANDIDATES, selected = CAT_CANDIDATES[1]),
                  selectInput("cat_twoway_cols", "Two-way table: cols",
                              choices = CAT_CANDIDATES,
                              selected = if (length(CAT_CANDIDATES) >= 2) CAT_CANDIDATES[2] else CAT_CANDIDATES[1])
                ),
                
                # Numeric summaries & plots controls
                conditionalPanel(
                  condition = "input.explore_mode == 'Numeric summaries + plots'",
                  selectInput(
                    "num_summary", "Numeric variable to summarize",
                    choices = NUM_CANDIDATES,
                    selected = if ("epa" %in% NUM_CANDIDATES) "epa" else NUM_CANDIDATES[1]
                  ),
                  selectInput(
                    "by_cat", "Summarize across levels of (categorical)",
                    choices = CAT_CANDIDATES,
                    selected = if ("play_type" %in% CAT_CANDIDATES) "play_type" else CAT_CANDIDATES[1]
                  ),
                  checkboxInput("show_sd", "Show SD column", value = TRUE),
                  hr(),
                  selectInput(
                    "plot_type", "Plot type",
                    choices = c("Bar (count)", "Boxplot by group", "Scatter (num1 vs num2)", "Violin by group", "2D bins (num1 vs num2)", "Heatmap (two cats)"),
                    selected = "Boxplot by group"
                  ),
                  selectInput("plot_color", "Color by (optional)", choices = c("(none)", CAT_CANDIDATES), selected = "(none)"),
                  selectInput("facet_var", "Facet by (optional)", choices = c("(none)", CAT_CANDIDATES), selected = "(none)")
                )
              )
            ),
            
            # Outputs
            card(
              card_header("Results"),
              card_body(
                conditionalPanel(
                  condition = "input.explore_mode == 'Categorical summaries'",
                  layout_columns(
                    card(card_header("One-way table"), withSpinner(DTOutput("oneway"), type = 6)),
                    card(card_header("Two-way table"), withSpinner(DTOutput("twoway"), type = 6))
                  )
                ),
                conditionalPanel(
                  condition = "input.explore_mode == 'Numeric summaries + plots'",
                  layout_columns(
                    card(card_header("Numeric summaries"), withSpinner(DTOutput("num_summ"), type = 6)),
                    card(card_header("Plot"), withSpinner(plotOutput("plt", height = 500), type = 6))
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session){
  
  # dynamic UI for category level pickers
  output$cat_levels1 <- renderUI({
    req(input$cat_var1)
    lvls <- sort(unique(BASE[[input$cat_var1]]))
    selectizeInput("cat_vals1", label = "Keep levels", choices = lvls, selected = lvls,
                   multiple = TRUE, options = list(plugins = list("remove_button")))
  })
  output$cat_levels2 <- renderUI({
    req(input$cat_var2)
    lvls <- sort(unique(BASE[[input$cat_var2]]))
    selectizeInput("cat_vals2", label = "Keep levels", choices = lvls, selected = lvls,
                   multiple = TRUE, options = list(plugins = list("remove_button")))
  })
  
  # dynamic numeric sliders based on chosen variables
  output$num_slider1 <- renderUI({
    req(input$num_var1)
    rng <- num_range(BASE[[input$num_var1]])
    sliderInput("num_rng1", paste0("Range for ", input$num_var1),
                min = rng[1], max = rng[2], value = rng, step = 1)
  })
  output$num_slider2 <- renderUI({
    req(input$num_var2)
    rng <- num_range(BASE[[input$num_var2]])
    sliderInput("num_rng2", paste0("Range for ", input$num_var2),
                min = rng[1], max = rng[2], value = rng, step = 1)
  })
  
  # reactive store for filtered data
  rv <- reactiveValues(data = BASE)
  
  observeEvent(input$apply, {
    df <- BASE
    
    # Apply categorical filters
    if (!is.null(input$cat_vals1) && length(input$cat_vals1) > 0) {
      df <- df |> filter(.data[[input$cat_var1]] %in% input$cat_vals1)
    }
    if (!is.null(input$cat_vals2) && length(input$cat_vals2) > 0) {
      df <- df |> filter(.data[[input$cat_var2]] %in% input$cat_vals2)
    }
    
    # Apply numeric filters
    if (!is.null(input$num_rng1)) {
      v <- suppressWarnings(as.numeric(df[[input$num_var1]]))
      df <- df[!is.na(v) & v >= input$num_rng1[1] & v <= input$num_rng1[2], , drop = FALSE]
    }
    if (!is.null(input$num_rng2)) {
      v <- suppressWarnings(as.numeric(df[[input$num_var2]]))
      df <- df[!is.na(v) & v >= input$num_rng2[1] & v <= input$num_rng2[2], , drop = FALSE]
    }
    
    rv$data <- df
  }, ignoreInit = TRUE)
  
  current_data <- reactive({ rv$data })
  
  # --- Data table + download ---
  output$tbl <- renderDT({
    dat <- current_data()
    validate(need(nrow(dat) > 0, "No rows after filtering. Adjust your filters and click Apply."))
    datatable(
      dat,
      extensions = c("Scroller", "Buttons"),
      options = list(
        dom = 'Bfrtip', buttons = c('copy','csv','excel'),
        deferRender = TRUE, scrollX = TRUE, scrollY = 400, scroller = TRUE
      ),
      rownames = FALSE
    )
  })
  
  output$dl <- downloadHandler(
    filename = function() paste0("filtered_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) {
      write.csv(current_data(), file, row.names = FALSE)
    }
  )
  
  # --- Categorical summaries ---
  output$oneway <- renderDT({
    df <- current_data(); req(input$cat_oneway)
    validate(need(nrow(df) > 0, "No rows available."))
    tbl <- df |>
      filter(!is.na(.data[[input$cat_oneway]])) |>
      count(.data[[input$cat_oneway]], name = "n") |>
      arrange(desc(n)) |>
      mutate(pct = round(100*n/sum(n), 2))
    datatable(tbl, options = list(dom = 't', pageLength = 10), rownames = FALSE)
  })
  
  output$twoway <- renderDT({
    df <- current_data(); req(input$cat_twoway_rows, input$cat_twoway_cols)
    validate(need(nrow(df) > 0, "No rows available."))
    tmp <- df |>
      filter(!is.na(.data[[input$cat_twoway_rows]]), !is.na(.data[[input$cat_twoway_cols]])) |>
      count(.data[[input$cat_twoway_rows]], .data[[input$cat_twoway_cols]]) |>
      group_by(.data[[input$cat_twoway_rows]]) |>
      mutate(row_pct = round(100*n/sum(n), 2)) |>
      ungroup()
    wide_counts <- tmp |>
      select(all_of(c(input$cat_twoway_rows, input$cat_twoway_cols, "n"))) |>
      pivot_wider(names_from = all_of(input$cat_twoway_cols), values_from = n, values_fill = 0)
    datatable(wide_counts, options = list(scrollX = TRUE))
  })
  
  # --- Numeric summaries + plots ---
  output$num_summ <- renderDT({
    df <- current_data(); req(input$num_summary, input$by_cat)
    validate(need(nrow(df) > 0, "No rows available."))
    v <- input$num_summary; g <- input$by_cat
    out <- df |>
      filter(!is.na(.data[[v]]), !is.na(.data[[g]])) |>
      group_by(.data[[g]]) |>
      summarise(
        n = dplyr::n(),
        mean = mean(as.numeric(.data[[v]]), na.rm = TRUE),
        median = median(as.numeric(.data[[v]]), na.rm = TRUE),
        sd = sd(as.numeric(.data[[v]]), na.rm = TRUE),
        .groups = 'drop'
      ) |>
      arrange(desc(n))
    if (!isTRUE(input$show_sd)) out <- select(out, -sd)
    datatable(out, options = list(dom = 't'))
  })
  
  output$plt <- renderPlot({
    df <- current_data(); validate(need(nrow(df) > 0, "No rows available."))
    
    col_by <- if (identical(input$plot_color, "(none)")) NULL else input$plot_color
    facet_by <- if (identical(input$facet_var, "(none)")) NULL else input$facet_var
    legend_title <- if (!is.null(col_by)) col_by else NULL
    
    p <- ggplot()
    
    if (input$plot_type == "Bar (count)") {
      req(input$by_cat)
      p <- ggplot(df, aes(x = .data[[input$by_cat]], fill = if (!is.null(col_by)) .data[[col_by]] else NULL)) +
        geom_bar(position = "dodge") +
        labs(x = input$by_cat, y = "Count", title = "Bar chart (counts)", fill = legend_title) +
        theme_minimal()
      
    } else if (input$plot_type == "Boxplot by group") {
      req(input$num_summary, input$by_cat)
      p <- ggplot(df, aes(x = .data[[input$by_cat]], y = suppressWarnings(as.numeric(.data[[input$num_summary]])),
                          fill = if (!is.null(col_by)) .data[[col_by]] else NULL)) +
        geom_boxplot(outlier.alpha = 0.4) +
        labs(x = input$by_cat, y = input$num_summary, title = "Boxplot by group", fill = legend_title) +
        theme_minimal()
      
    } else if (input$plot_type == "Scatter (num1 vs num2)") {
      req(input$num_var1, input$num_var2)
      p <- ggplot(df, aes(x = suppressWarnings(as.numeric(.data[[input$num_var1]])),
                          y = suppressWarnings(as.numeric(.data[[input$num_var2]])),
                          color = if (!is.null(col_by)) .data[[col_by]] else NULL)) +
        geom_point(alpha = 0.6) +
        labs(x = input$num_var1, y = input$num_var2, title = "Scatterplot", fill = legend_title) +
        theme_minimal()
      
    } else if (input$plot_type == "Violin by group") {
      req(input$num_summary, input$by_cat)
      p <- ggplot(
        df,
        aes(
          x = .data[[input$by_cat]],
          y = suppressWarnings(as.numeric(.data[[input$num_summary]])),
          fill = if (!is.null(col_by)) .data[[col_by]] else NULL
        )
      ) +
        geom_violin(trim = FALSE, alpha = 0.8) +
        labs(x = input$by_cat, y = input$num_summary, title = "Violin plot by group") +
        theme_minimal()
      
    } else if (input$plot_type == "2D bins (num1 vs num2)") {
      req(input$num_var1, input$num_var2)
      p <- ggplot(
        df,
        aes(
          x = suppressWarnings(as.numeric(.data[[input$num_var1]])),
          y = suppressWarnings(as.numeric(.data[[input$num_var2]]))
        )
      ) +
        geom_bin2d(bins = 30) +
        labs(x = input$num_var1, y = input$num_var2, title = "2D-binned density") +
        theme_minimal()
      
      
    } else if (input$plot_type == "Heatmap (two cats)") {
      req(input$cat_twoway_rows, input$cat_twoway_cols)
      tmp <- df |> count(.data[[input$cat_twoway_rows]], .data[[input$cat_twoway_cols]])
      names(tmp) <- c("rows","cols","n")
      p <- ggplot(tmp, aes(x = cols, y = rows, fill = n)) +
        geom_tile() +
        labs(x = input$cat_twoway_cols, y = input$cat_twoway_rows, title = "Heatmap of counts", fill = legend_title) +
        theme_minimal()
    }
    
    if (!is.null(facet_by)) p <- p + facet_wrap(vars(.data[[facet_by]]), scales = "free_y")
    p
  })
}

shinyApp(ui, server)
