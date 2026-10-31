# =====================================================================
# app.R — NVIDIA GPU Sales Dashboard
# Phase 4: a complete, running Shiny app wired to the real data.
# (Real-time self-updating comes in Phase 5 — for now, data loads once
#  when the app starts, same as our earlier scripts.)
# =====================================================================
# To run: open this file in RStudio and click the green "Run App"
# button at the top of the Source pane (it only appears for files
# named app.R, ui.R, or server.R).
# =====================================================================

library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(scales)
library(DT)

# ---- Load data once, when the app starts ----
gpu_sales <- read_csv("data/nvidia_gpu_sales_synthetic_2026.csv")

# =====================================================================
# UI — what gets drawn on screen
# =====================================================================
ui <- fluidPage(
  theme = bs_theme(version = 5),
  
  # Header row: title + live-refresh indicator
  fluidRow(
    column(8, h3("GPU sales dashboard")),
    column(4, div(
      style = "text-align:right; color:#888; font-size:13px; padding-top:14px;",
      "Last refreshed: ", textOutput("last_refreshed", inline = TRUE)
    ))
  ),
  
  # Filter row — every input here feeds the one filtered_data() reactive below
  fluidRow(
    column(3, selectInput("region_filter", "Region",
                          choices = c("All", sort(unique(gpu_sales$region))))),
    column(3, selectInput("family_filter", "GPU family",
                          choices = c("All", sort(unique(gpu_sales$gpu_family))))),
    column(3, selectInput("channel_filter", "Sales channel",
                          choices = c("All", sort(unique(gpu_sales$sales_channel))))),
    column(3, dateRangeInput("date_filter", "Date range",
                             start = min(gpu_sales$sale_date),
                             end   = max(gpu_sales$sale_date)))
  ),
  
  br(),
  
  # KPI cards
  layout_column_wrap(
    width = 1 / 4,
    value_box(title = "Total revenue",     value = textOutput("kpi_revenue")),
    value_box(title = "Units sold",        value = textOutput("kpi_units")),
    value_box(title = "SKUs tracked",      value = textOutput("kpi_skus")),
    value_box(title = "Low stock alerts",  value = textOutput("kpi_alerts"))
  ),
  
  br(),
  
  # Six tabs
  tabsetPanel(
    tabPanel("Overview",
             fluidRow(
               column(6, plotOutput("chart_family")),
               column(6, plotOutput("chart_trend"))
             )),
    tabPanel("Products",          plotOutput("chart_products")),
    tabPanel("Regional",          plotOutput("chart_region")),
    tabPanel("Customer segments", plotOutput("chart_segment")),
    tabPanel("Stock & pricing",   plotOutput("chart_stock")),
    tabPanel("Raw data",          DTOutput("raw_table"))
  )
)

# =====================================================================
# SERVER — what computes the values for each output
# =====================================================================
server <- function(input, output, session) {
  # ---- Self-regulating data source (Phase 5) ----
  gpu_sales_live <- reactiveFileReader(
    intervalMillis = 5000,                                  # check every 5 seconds
    session = session,
    filePath = "data/nvidia_gpu_sales_synthetic_2026.csv",
    readFunc = read_csv
  )
  
  # Keep filter dropdowns in sync if the refreshed data ever has
  # new regions/families/channels
  observeEvent(gpu_sales_live(), {
    df <- gpu_sales_live()
    updateSelectInput(session, "region_filter",
                      choices = c("All", sort(unique(df$region))), selected = input$region_filter)
    updateSelectInput(session, "family_filter",
                      choices = c("All", sort(unique(df$gpu_family))), selected = input$family_filter)
    updateSelectInput(session, "channel_filter",
                      choices = c("All", sort(unique(df$sales_channel))), selected = input$channel_filter)
  }) 
  # The ONE reactive every output depends on. Change any filter above,
  # and this re-runs automatically — nothing downstream needs to know
  # a filter changed, it just always reads the current filtered_data().
  filtered_data <- reactive({
    df <- gpu_sales_live()
    if (input$region_filter  != "All") df <- df %>% filter(region == input$region_filter)
    if (input$family_filter  != "All") df <- df %>% filter(gpu_family == input$family_filter)
    if (input$channel_filter != "All") df <- df %>% filter(sales_channel == input$channel_filter)
    df %>% filter(sale_date >= input$date_filter[1], sale_date <= input$date_filter[2])
  })
  
  output$last_refreshed <- renderText({
    gpu_sales_live()   # takes a dependency, so this fires whenever the file changes
    format(Sys.time(), "%H:%M:%S")
  })
  
  # ---- KPI cards ----
  output$kpi_revenue <- renderText(
    label_dollar(scale = 1e-6, suffix = "M")(sum(filtered_data()$revenue_usd))
  )
  output$kpi_units <- renderText(
    comma(sum(filtered_data()$units_sold))
  )
  output$kpi_skus <- renderText(
    as.character(n_distinct(filtered_data()$gpu_model))
  )
  output$kpi_alerts <- renderText({
    filtered_data() %>%
      group_by(gpu_model) %>%
      slice_max(sale_date, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      filter(stock_status %in% c("Low Stock", "Backordered", "Sold Out")) %>%
      nrow() %>%
      as.character()
  })
  
  # ---- Overview tab ----
  output$chart_family <- renderPlot({
    filtered_data() %>%
      group_by(gpu_family) %>%
      summarise(total_revenue = sum(revenue_usd), .groups = "drop") %>%
      ggplot(aes(x = gpu_family, y = total_revenue)) +
      geom_col(fill = "#76B900") +
      scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
      labs(title = "Revenue by GPU family", x = NULL, y = "Revenue") +
      theme_minimal()
  })
  
  output$chart_trend <- renderPlot({
    filtered_data() %>%
      mutate(sale_month = floor_date(sale_date, "month")) %>%
      group_by(sale_month) %>%
      summarise(monthly_revenue = sum(revenue_usd), .groups = "drop") %>%
      ggplot(aes(x = sale_month, y = monthly_revenue)) +
      geom_line(color = "#76B900", linewidth = 1) +
      geom_point(color = "#76B900") +
      scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
      labs(title = "Monthly revenue trend", x = NULL, y = "Revenue") +
      theme_minimal()
  })
  
  # ---- Products tab ----
  output$chart_products <- renderPlot({
    filtered_data() %>%
      group_by(gpu_model) %>%
      summarise(total_revenue = sum(revenue_usd), .groups = "drop") %>%
      ggplot(aes(x = reorder(gpu_model, total_revenue), y = total_revenue)) +
      geom_col(fill = "#76B900") +
      coord_flip() +
      scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
      labs(title = "Revenue by GPU model", x = NULL, y = "Revenue") +
      theme_minimal()
  })
  
  # ---- Regional tab ----
  output$chart_region <- renderPlot({
    filtered_data() %>%
      group_by(region, sales_channel) %>%
      summarise(revenue = sum(revenue_usd), .groups = "drop") %>%
      ggplot(aes(x = region, y = revenue, fill = sales_channel)) +
      geom_col(position = "stack") +
      scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
      labs(title = "Revenue by region and channel", x = NULL, y = "Revenue", fill = "Channel") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })
  
  # ---- Customer segments tab ----
  output$chart_segment <- renderPlot({
    filtered_data() %>%
      group_by(customer_segment) %>%
      summarise(total_revenue = sum(revenue_usd), .groups = "drop") %>%
      ggplot(aes(x = reorder(customer_segment, total_revenue), y = total_revenue)) +
      geom_col(fill = "#76B900") +
      coord_flip() +
      scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
      labs(title = "Revenue by customer segment", x = NULL, y = "Revenue") +
      theme_minimal()
  })
  
  # ---- Stock & pricing tab ----
  output$chart_stock <- renderPlot({
    filtered_data() %>%
      group_by(stock_status) %>%
      summarise(avg_premium_pct = mean(price_premium_pct), .groups = "drop") %>%
      ggplot(aes(x = reorder(stock_status, avg_premium_pct), y = avg_premium_pct)) +
      geom_col(fill = "#76B900") +
      coord_flip() +
      labs(title = "Avg. price premium by stock status", x = NULL, y = "Avg. premium (%)") +
      theme_minimal()
  })
  
  # ---- Raw data tab ----
  output$raw_table <- renderDT({
    filtered_data() %>%
      select(sale_date, gpu_model, region, sales_channel, customer_segment,
             units_sold, revenue_usd, stock_status)
  })
}

# =====================================================================
shinyApp(ui, server)