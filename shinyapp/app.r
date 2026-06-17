# ============================================================
# Shiny App: Crime Hotspot Analysis Dashboard
# India District-Level IPC Data (2001-2014)
# ============================================================

library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(DT)
library(tidyr)
library(stringr)
library(scales)
library(forcats)

# ── Helpers ──────────────────────────────────────────────────

TIER_COLOURS <- c(
  "Critical" = "#d62828",
  "High"     = "#f4a261",
  "Moderate" = "#2a9d8f",
  "Low"      = "#adb5bd"
)

CLUSTER_PAL <- c("#e63946","#f4a261","#2a9d8f","#264653",
                  "#e9c46a","#a8dadc","#6d6875")

theme_app <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background  = element_rect(fill = "#ffffff", colour = NA),
      panel.background = element_rect(fill = "#ffffff", colour = NA),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", colour = "#1a1a2e"),
      legend.background = element_rect(fill = "#ffffff", colour = NA)
    )
}

# ── Load data ─────────────────────────────────────────────────

load_data <- function() {
  base <- if (file.exists("data/processed/crime_cleaned.csv")) "." else ".."

  df <- read_csv(file.path(base, "data/processed/crime_cleaned.csv"),
                 show_col_types = FALSE)

  hotspots <- if (file.exists(file.path(base, "data/processed/district_hotspot_scores.csv"))) {
    read_csv(file.path(base, "data/processed/district_hotspot_scores.csv"),
             show_col_types = FALSE)
  } else {
    # Compute on-the-fly
    df %>%
      group_by(state, district) %>%
      summarise(
        mean_total    = mean(total_ipc_crimes,       na.rm = TRUE),
        mean_violent  = mean(violent_crimes_total,    na.rm = TRUE),
        mean_women    = mean(women_crimes_total,      na.rm = TRUE),
        years_present = n(),
        .groups = "drop"
      ) %>%
      mutate(
        z_total   = (mean_total   - mean(mean_total))   / sd(mean_total),
        z_violent = (mean_violent - mean(mean_violent)) / sd(mean_violent),
        z_women   = (mean_women   - mean(mean_women))   / sd(mean_women),
        hotspot_score = 0.5 * z_total + 0.3 * z_violent + 0.2 * z_women,
        hotspot_tier  = case_when(
          hotspot_score >= 2.0 ~ "Critical",
          hotspot_score >= 1.0 ~ "High",
          hotspot_score >= 0.0 ~ "Moderate",
          TRUE                 ~ "Low"
        )
      )
  }

  clusters <- if (file.exists(file.path(base, "data/processed/district_clusters.csv"))) {
    read_csv(file.path(base, "data/processed/district_clusters.csv"),
             show_col_types = FALSE)
  } else {
    NULL
  }

  list(df = df, hotspots = hotspots, clusters = clusters)
}

dat <- load_data()
df       <- dat$df
hotspots <- dat$hotspots
clusters <- dat$clusters

all_states   <- sort(unique(df$state))
all_years    <- sort(unique(df$year))
crime_cols   <- c("murder","rape","kidnapping_abduction","dacoity","robbery",
                  "burglary","theft","auto_theft","riots","cheating",
                  "dowry_deaths","assault_on_women_with_intent_to_outrage_her_modesty",
                  "cruelty_by_husband_or_his_relatives","causing_death_by_negligence",
                  "arson","total_ipc_crimes","violent_crimes_total","women_crimes_total")
crime_cols   <- intersect(crime_cols, names(df))
crime_labels <- setNames(crime_cols,
  str_to_title(str_replace_all(crime_cols, "_", " ")))

# ─────────────────────────────────────────────────────────────
# UI
# ─────────────────────────────────────────────────────────────

ui <- dashboardPage(
  skin = "red",

  dashboardHeader(title = "🔍 Crime Hotspot Analysis"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("📊 Overview",         tabName = "overview",   icon = icon("chart-line")),
      menuItem("🔥 Hotspot Map",       tabName = "hotspot",    icon = icon("fire")),
      menuItem("📈 Trend Analysis",    tabName = "trends",     icon = icon("timeline")),
      menuItem("🤖 Cluster Analysis",  tabName = "clusters",   icon = icon("object-group")),
      menuItem("🗂️ Data Explorer",     tabName = "explorer",   icon = icon("table"))
    ),
    hr(),
    # Global filters
    selectInput("sel_state", "Filter by State",
                choices = c("All States" = "ALL", all_states),
                selected = "ALL"),
    sliderInput("sel_years", "Year Range",
                min = min(all_years), max = max(all_years),
                value = c(min(all_years), max(all_years)),
                step = 1, sep = "")
  ),

  dashboardBody(
    tags$head(tags$style(HTML("
      .skin-red .main-header .logo { background-color: #1a1a2e !important; }
      .skin-red .main-header .navbar { background-color: #16213e !important; }
      .skin-red .main-sidebar { background-color: #0f3460 !important; }
      .content-wrapper { background-color: #f7f7f7 !important; }
      .value-box .inner h3 { font-size: 24px; }
    "))),

    tabItems(

      # ── Overview Tab ────────────────────────────────────────
      tabItem(tabName = "overview",
        h2("National Crime Overview", style = "color:#1a1a2e; font-weight:bold;"),
        p("India District-Level IPC Crime Data | 2001 – 2014"),

        fluidRow(
          valueBoxOutput("vbox_total",    width = 3),
          valueBoxOutput("vbox_violent",  width = 3),
          valueBoxOutput("vbox_women",    width = 3),
          valueBoxOutput("vbox_districts",width = 3)
        ),

        fluidRow(
          box(title = "National Crime Trend", width = 8, status = "danger",
              solidHeader = TRUE,
              plotlyOutput("plot_national_trend", height = 340)),
          box(title = "Crime Category Share (Latest Year)", width = 4, status = "warning",
              solidHeader = TRUE,
              plotlyOutput("plot_donut", height = 340))
        ),

        fluidRow(
          box(title = "Top 15 States by Avg Annual Crimes", width = 6, status = "primary",
              solidHeader = TRUE,
              plotlyOutput("plot_top_states", height = 380)),
          box(title = "Crimes Against Women – Annual Trend", width = 6, status = "info",
              solidHeader = TRUE,
              plotlyOutput("plot_women_trend", height = 380))
        )
      ),

      # ── Hotspot Tab ─────────────────────────────────────────
      tabItem(tabName = "hotspot",
        h2("Crime Hotspot Identification", style = "color:#1a1a2e; font-weight:bold;"),

        fluidRow(
          column(4,
            sliderInput("top_n_hot", "Show Top N Districts",
                        min = 5, max = 50, value = 20, step = 5),
            selectInput("hot_crime", "Hotspot Based On",
                        choices = c(
                          "Composite Score"  = "hotspot_score",
                          "Total Crimes"     = "mean_total",
                          "Violent Crimes"   = "mean_violent",
                          "Crimes vs Women"  = "mean_women"
                        )),
            hr(),
            h4("Risk Tier Legend:"),
            div(style="color:#d62828; font-weight:bold;", "🔴 Critical: Score ≥ 2.0"),
            div(style="color:#f4a261; font-weight:bold;", "🟠 High: Score ≥ 1.0"),
            div(style="color:#2a9d8f; font-weight:bold;", "🟢 Moderate: Score ≥ 0.0"),
            div(style="color:#adb5bd; font-weight:bold;", "⚪ Low: Score < 0.0")
          ),
          column(8,
            box(title = "Hotspot Score Map (State Bubbles)", width = 12,
                status = "danger", solidHeader = TRUE,
                plotlyOutput("plot_bubble_map", height = 450))
          )
        ),

        fluidRow(
          box(title = "Top District Hotspots", width = 8, status = "warning",
              solidHeader = TRUE,
              plotlyOutput("plot_hotspot_bar", height = 420)),
          box(title = "Tier Distribution", width = 4, status = "info",
              solidHeader = TRUE,
              plotlyOutput("plot_tier_pie", height = 420))
        )
      ),

      # ── Trends Tab ──────────────────────────────────────────
      tabItem(tabName = "trends",
        h2("Detailed Crime Trend Analysis", style = "color:#1a1a2e; font-weight:bold;"),

        fluidRow(
          column(4,
            selectInput("trend_crime", "Crime Type",
                        choices = crime_labels,
                        selected = "total_ipc_crimes"),
            selectInput("trend_state", "State",
                        choices = c("National Total" = "ALL", all_states),
                        selected = "ALL"),
            radioButtons("trend_agg", "Aggregation",
                         choices = c("Sum" = "sum", "Mean per District" = "mean"),
                         selected = "sum")
          ),
          column(8,
            box(title = "Year-wise Crime Trend", width = 12,
                status = "primary", solidHeader = TRUE,
                plotlyOutput("plot_crime_trend", height = 360))
          )
        ),

        fluidRow(
          box(title = "District Comparison (same crime type)", width = 12,
              status = "info", solidHeader = TRUE,
              plotlyOutput("plot_district_compare", height = 380))
        )
      ),

      # ── Cluster Tab ─────────────────────────────────────────
      tabItem(tabName = "clusters",
        h2("Crime Pattern Clustering", style = "color:#1a1a2e; font-weight:bold;"),
        p("K-means clustering groups districts by their crime profile similarity."),

        if (!is.null(clusters)) {
          tagList(
            fluidRow(
              box(title = "Cluster Distribution by State", width = 8,
                  status = "danger", solidHeader = TRUE,
                  plotlyOutput("plot_cluster_state", height = 400)),
              box(title = "Cluster Summary", width = 4,
                  status = "warning", solidHeader = TRUE,
                  DTOutput("tbl_cluster_summary"))
            ),
            fluidRow(
              box(title = "Crime Profile per Cluster", width = 12,
                  status = "primary", solidHeader = TRUE,
                  plotlyOutput("plot_cluster_profile", height = 420))
            )
          )
        } else {
          fluidRow(box(
            width = 12, status = "warning",
            h4("⚠️ Run scripts/04_clustering.R first to generate cluster data.")
          ))
        }
      ),

      # ── Data Explorer Tab ────────────────────────────────────
      tabItem(tabName = "explorer",
        h2("Data Explorer", style = "color:#1a1a2e; font-weight:bold;"),

        fluidRow(
          column(3,
            selectInput("exp_state", "State",
                        choices = c("All" = "ALL", all_states), selected = "ALL"),
            selectInput("exp_year", "Year",
                        choices = c("All Years" = "ALL", all_years), selected = "ALL"),
            downloadButton("dl_data", "⬇ Download Filtered CSV",
                           class = "btn-primary")
          ),
          column(9,
            box(title = "District Crime Data", width = 12, status = "primary",
                solidHeader = TRUE,
                DTOutput("tbl_explorer"))
          )
        )
      )
    )
  )
)

# ─────────────────────────────────────────────────────────────
# SERVER
# ─────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── Reactive filtered data ──────────────────────────────────

  rdf <- reactive({
    d <- df %>% filter(year >= input$sel_years[1], year <= input$sel_years[2])
    if (input$sel_state != "ALL") d <- d %>% filter(state == input$sel_state)
    d
  })

  # ── Value Boxes ─────────────────────────────────────────────

  output$vbox_total <- renderValueBox({
    val <- rdf() %>% summarise(s = sum(total_ipc_crimes, na.rm = TRUE)) %>% pull(s)
    valueBox(comma(val), "Total IPC Cases", icon = icon("gavel"), color = "red")
  })

  output$vbox_violent <- renderValueBox({
    val <- rdf() %>% summarise(s = sum(violent_crimes_total, na.rm = TRUE)) %>% pull(s)
    valueBox(comma(val), "Violent Crimes", icon = icon("skull"), color = "orange")
  })

  output$vbox_women <- renderValueBox({
    val <- rdf() %>% summarise(s = sum(women_crimes_total, na.rm = TRUE)) %>% pull(s)
    valueBox(comma(val), "Crimes vs Women", icon = icon("venus"), color = "purple")
  })

  output$vbox_districts <- renderValueBox({
    val <- n_distinct(rdf()$district)
    valueBox(val, "Districts Covered", icon = icon("map-marker-alt"), color = "blue")
  })

  # ── National Trend ──────────────────────────────────────────

  output$plot_national_trend <- renderPlotly({
    trend <- rdf() %>%
      group_by(year) %>%
      summarise(
        `All IPC Crimes`       = sum(total_ipc_crimes,      na.rm = TRUE),
        `Violent Crimes`       = sum(violent_crimes_total,   na.rm = TRUE),
        `Property Crimes`      = sum(property_crimes_total,  na.rm = TRUE),
        `Crimes Against Women` = sum(women_crimes_total,     na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_longer(-year, names_to = "Type", values_to = "Count")

    p <- ggplot(trend, aes(year, Count, colour = Type, group = Type,
                           text = paste0(Type, "<br>Year: ", year,
                                         "<br>Cases: ", comma(Count)))) +
      geom_line(linewidth = 1.1) + geom_point(size = 2) +
      scale_y_continuous(labels = comma) +
      scale_colour_manual(values = c("#e63946","#f4a261","#2a9d8f","#a8dadc")) +
      labs(x = "Year", y = "Cases", colour = NULL) +
      theme_app()
    ggplotly(p, tooltip = "text") %>% layout(legend = list(orientation = "h"))
  })

  # ── Donut chart ─────────────────────────────────────────────

  output$plot_donut <- renderPlotly({
    latest <- rdf() %>% filter(year == max(year))
    cats <- tibble(
      Category = c("Theft","Cheating","Violent","Women-specific","Other"),
      Count = c(
        sum(latest$theft + latest$auto_theft, na.rm = TRUE),
        sum(latest$cheating, na.rm = TRUE),
        sum(latest$violent_crimes_total, na.rm = TRUE),
        sum(latest$women_crimes_total, na.rm = TRUE),
        sum(latest$other_ipc_crimes, na.rm = TRUE)
      )
    )
    plot_ly(cats, labels = ~Category, values = ~Count, type = "pie",
            hole = 0.4,
            marker = list(colors = c("#e63946","#f4a261","#2a9d8f","#264653","#a8dadc")),
            textinfo = "label+percent") %>%
      layout(showlegend = FALSE,
             paper_bgcolor = "white", plot_bgcolor = "white")
  })

  # ── Top States ──────────────────────────────────────────────

  output$plot_top_states <- renderPlotly({
    top <- rdf() %>%
      group_by(state, year) %>%
      summarise(total = sum(total_ipc_crimes, na.rm = TRUE), .groups = "drop") %>%
      group_by(state) %>%
      summarise(avg = mean(total), .groups = "drop") %>%
      slice_max(avg, n = 15) %>%
      mutate(state = fct_reorder(state, avg))

    p <- ggplot(top, aes(avg, state, fill = avg,
                          text = paste0(state, "<br>Avg: ", comma(round(avg))))) +
      geom_col(show.legend = FALSE) +
      scale_fill_gradient(low = "#f4a261", high = "#e63946") +
      scale_x_continuous(labels = comma) +
      labs(x = "Avg Annual Cases", y = NULL) +
      theme_app()
    ggplotly(p, tooltip = "text")
  })

  # ── Women crimes trend ───────────────────────────────────────

  output$plot_women_trend <- renderPlotly({
    wt <- rdf() %>%
      group_by(year) %>%
      summarise(
        Rape                = sum(rape, na.rm = TRUE),
        `Dowry Deaths`      = sum(dowry_deaths, na.rm = TRUE),
        `Cruelty by Husband`= sum(cruelty_by_husband_or_his_relatives, na.rm = TRUE),
        `Assault on Women`  = sum(assault_on_women_with_intent_to_outrage_her_modesty,
                                   na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_longer(-year, names_to = "Crime", values_to = "Count")

    p <- ggplot(wt, aes(year, Count, colour = Crime, group = Crime)) +
      geom_line(linewidth = 1) + geom_point(size = 2) +
      scale_y_continuous(labels = comma) +
      scale_colour_manual(values = c("#e63946","#f4a261","#2a9d8f","#264653")) +
      labs(x = "Year", y = "Cases", colour = NULL) +
      theme_app()
    ggplotly(p) %>% layout(legend = list(orientation = "h"))
  })

  # ── Bubble map ───────────────────────────────────────────────

  state_coords_r <- tibble::tribble(
    ~state,                           ~lat,    ~lon,
    "Andhra Pradesh",                  15.91,   79.74,
    "Arunachal Pradesh",               27.10,   93.62,
    "Assam",                           26.20,   92.94,
    "Bihar",                           25.09,   85.31,
    "Chhattisgarh",                    21.27,   81.86,
    "Goa",                             15.30,   74.12,
    "Gujarat",                         22.26,   71.20,
    "Haryana",                         29.06,   76.09,
    "Himachal Pradesh",                31.10,   77.17,
    "Jammu & Kashmir",                 33.73,   76.92,
    "Jharkhand",                       23.61,   85.28,
    "Karnataka",                       15.32,   75.71,
    "Kerala",                          10.85,   76.27,
    "Madhya Pradesh",                  22.97,   78.66,
    "Maharashtra",                     19.75,   75.71,
    "Manipur",                         24.66,   93.91,
    "Meghalaya",                       25.47,   91.37,
    "Mizoram",                         23.16,   92.95,
    "Nagaland",                        26.16,   94.56,
    "Odisha",                          20.95,   85.09,
    "Punjab",                          31.15,   75.34,
    "Rajasthan",                       27.02,   74.22,
    "Sikkim",                          27.53,   88.51,
    "Tamil Nadu",                      11.12,   78.66,
    "Telangana",                       18.11,   79.02,
    "Tripura",                         23.94,   91.99,
    "Uttar Pradesh",                   26.85,   80.91,
    "Uttarakhand",                     30.07,   79.09,
    "West Bengal",                     22.99,   87.85,
    "Delhi",                           28.70,   77.10,
    "Andaman & Nicobar Islands",       11.74,   92.66,
    "Chandigarh",                      30.74,   76.79,
    "Dadra & Nagar Haveli",            20.17,   73.00,
    "Daman & Diu",                     20.39,   72.83,
    "Puducherry",                      11.94,   79.81
  )

  output$plot_bubble_map <- renderPlotly({
    state_agg <- rdf() %>%
      group_by(state) %>%
      summarise(
        mean_total   = mean(total_ipc_crimes,      na.rm = TRUE),
        mean_violent = mean(violent_crimes_total,   na.rm = TRUE),
        mean_women   = mean(women_crimes_total,     na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        z_total   = (mean_total   - mean(mean_total))   / (sd(mean_total)   + 1e-9),
        z_violent = (mean_violent - mean(mean_violent)) / (sd(mean_violent) + 1e-9),
        z_women   = (mean_women   - mean(mean_women))   / (sd(mean_women)   + 1e-9),
        hotspot_score = 0.5 * z_total + 0.3 * z_violent + 0.2 * z_women
      ) %>%
      left_join(state_coords_r, by = "state") %>%
      filter(!is.na(lat))

    plot_ly(state_agg,
            x = ~lon, y = ~lat,
            type = "scatter", mode = "markers+text",
            size  = ~mean_total,
            color = ~hotspot_score,
            colors = c("#2a9d8f","#f4a261","#d62828"),
            text  = ~state,
            textposition = "top center",
            hovertemplate = paste0(
              "<b>%{text}</b><br>",
              "Avg Annual Cases: %{customdata[0]:,}<br>",
              "Hotspot Score: %{customdata[1]:.2f}<extra></extra>"
            ),
            customdata = ~cbind(round(mean_total), round(hotspot_score, 2)),
            marker = list(sizemode = "diameter", sizeref = 0.8, opacity = 0.7)
    ) %>%
      layout(
        xaxis = list(title = "Longitude", range = c(66, 99)),
        yaxis = list(title = "Latitude",  range = c(6,  38)),
        paper_bgcolor = "white", plot_bgcolor = "#e8f4f8"
      )
  })

  # ── Hotspot bar ──────────────────────────────────────────────

  output$plot_hotspot_bar <- renderPlotly({
    hot <- hotspots %>%
      arrange(desc(!!sym(input$hot_crime))) %>%
      head(input$top_n_hot) %>%
      mutate(
        label = paste0(str_trunc(district, 14), " (", str_sub(state, 1, 3), ")"),
        label = fct_reorder(label, !!sym(input$hot_crime))
      )

    p <- ggplot(hot, aes(!!sym(input$hot_crime), label, fill = hotspot_tier,
                          text = paste0(district, ", ", state,
                                         "<br>Score: ", round(hotspot_score, 2),
                                         "<br>Tier: ", hotspot_tier))) +
      geom_col() +
      scale_fill_manual(values = TIER_COLOURS) +
      scale_x_continuous(labels = if (input$hot_crime == "hotspot_score") waiver() else comma) +
      labs(x = input$hot_crime, y = NULL, fill = "Tier") +
      theme_app()
    ggplotly(p, tooltip = "text") %>% layout(legend = list(orientation = "h"))
  })

  # ── Tier pie ─────────────────────────────────────────────────

  output$plot_tier_pie <- renderPlotly({
    tiers <- hotspots %>%
      mutate(hotspot_tier = factor(hotspot_tier,
             levels = c("Critical","High","Moderate","Low"))) %>%
      count(hotspot_tier)

    plot_ly(tiers, labels = ~hotspot_tier, values = ~n, type = "pie",
            marker = list(colors = unname(TIER_COLOURS)),
            textinfo = "label+percent+value") %>%
      layout(showlegend = FALSE,
             paper_bgcolor = "white", plot_bgcolor = "white")
  })

  # ── Trend tab plots ──────────────────────────────────────────

  output$plot_crime_trend <- renderPlotly({
    col  <- input$trend_crime
    aggfn <- if (input$trend_agg == "sum") sum else mean

    d <- df %>%
      filter(year >= input$sel_years[1], year <= input$sel_years[2])

    if (input$trend_state != "ALL")
      d <- d %>% filter(state == input$trend_state)

    trend <- d %>%
      group_by(year) %>%
      summarise(value = aggfn(!!sym(col), na.rm = TRUE), .groups = "drop")

    p <- ggplot(trend, aes(year, value,
                            text = paste0("Year: ", year, "<br>Value: ", comma(round(value))))) +
      geom_area(fill = "#e63946", alpha = 0.25) +
      geom_line(colour = "#e63946", linewidth = 1.3) +
      geom_point(colour = "#e63946", size = 2.5) +
      scale_y_continuous(labels = comma) +
      scale_x_continuous(breaks = all_years) +
      labs(x = "Year", y = paste(input$trend_agg, "cases")) +
      theme_app() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = "text")
  })

  output$plot_district_compare <- renderPlotly({
    col <- input$trend_crime
    state_sel <- if (input$trend_state == "ALL") all_states[1] else input$trend_state

    top_d <- df %>%
      filter(state == state_sel) %>%
      group_by(district) %>%
      summarise(total = sum(!!sym(col), na.rm = TRUE), .groups = "drop") %>%
      slice_max(total, n = 8) %>% pull(district)

    d <- df %>%
      filter(state == state_sel, district %in% top_d,
             year >= input$sel_years[1], year <= input$sel_years[2])

    p <- ggplot(d, aes(year, !!sym(col), colour = district, group = district)) +
      geom_line(linewidth = 1) + geom_point(size = 2) +
      scale_y_continuous(labels = comma) +
      labs(x = "Year", y = "Cases",
           title = paste("Top Districts –", state_sel), colour = NULL) +
      theme_app()
    ggplotly(p) %>% layout(legend = list(orientation = "h"))
  })

  # ── Cluster plots ────────────────────────────────────────────

  if (!is.null(clusters)) {
    output$plot_cluster_state <- renderPlotly({
      csum <- clusters %>%
        group_by(state, cluster) %>%
        summarise(n = n(), .groups = "drop") %>%
        mutate(cluster = paste0("Cluster ", cluster))

      p <- ggplot(csum, aes(state, n, fill = cluster,
                             text = paste0(state, "<br>Cluster: ", cluster, "<br>n=", n))) +
        geom_col(position = "fill") +
        scale_fill_manual(values = CLUSTER_PAL) +
        scale_y_continuous(labels = percent) +
        coord_flip() +
        labs(x = NULL, y = "Proportion", fill = "Cluster") +
        theme_app()
      ggplotly(p, tooltip = "text") %>% layout(legend = list(orientation = "h"))
    })

    output$tbl_cluster_summary <- renderDT({
      clusters %>%
        group_by(cluster) %>%
        summarise(
          `# Districts` = n(),
          `Avg Total`   = round(mean(total_ipc_crimes, na.rm = TRUE)),
          .groups = "drop"
        ) %>%
        datatable(options = list(pageLength = 10, dom = "t"),
                  rownames = FALSE,
                  class = "compact stripe hover")
    })

    output$plot_cluster_profile <- renderPlotly({
      feat <- c("murder","rape","theft","robbery","riots","cheating","dowry_deaths","arson")
      feat <- intersect(feat, names(clusters))

      prof <- clusters %>%
        group_by(cluster) %>%
        summarise(across(all_of(feat), mean, na.rm = TRUE), .groups = "drop") %>%
        pivot_longer(-cluster, names_to = "crime", values_to = "avg") %>%
        mutate(
          crime   = str_to_title(str_replace_all(crime, "_", " ")),
          cluster = paste0("Cluster ", cluster)
        )

      p <- ggplot(prof, aes(crime, avg, fill = cluster,
                             text = paste0(cluster, "<br>", crime, "<br>Avg: ", round(avg)))) +
        geom_col(position = "dodge") +
        scale_fill_manual(values = CLUSTER_PAL) +
        coord_flip() +
        scale_y_continuous(labels = comma) +
        labs(x = NULL, y = "Avg Cases", fill = "Cluster") +
        theme_app()
      ggplotly(p, tooltip = "text") %>% layout(legend = list(orientation = "h"))
    })
  }

  # ── Data Explorer ────────────────────────────────────────────

  exp_data <- reactive({
    d <- df
    if (input$exp_state != "ALL") d <- d %>% filter(state == input$exp_state)
    if (input$exp_year  != "ALL") d <- d %>% filter(year  == as.integer(input$exp_year))
    d %>% select(state, district, year,
                 murder, rape, theft, robbery, riots, cheating,
                 dowry_deaths, total_ipc_crimes) %>%
      arrange(desc(total_ipc_crimes))
  })

  output$tbl_explorer <- renderDT({
    datatable(exp_data(),
              filter   = "top",
              options  = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE,
              class    = "compact stripe hover") %>%
      formatRound(columns = c("murder","rape","theft","robbery","riots",
                               "cheating","dowry_deaths","total_ipc_crimes"), digits = 0)
  })

  output$dl_data <- downloadHandler(
    filename = function() paste0("crime_data_", Sys.Date(), ".csv"),
    content  = function(file) write_csv(exp_data(), file)
  )
}

# ─────────────────────────────────────────────────────────────

shinyApp(ui, server)