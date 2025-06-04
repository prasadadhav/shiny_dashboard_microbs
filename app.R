library(shiny)
library(shinydashboard)
library(leaflet)
library(readxl)
library(ggplot2)
library(dplyr)
library(bslib)
library(bsicons)
library(shinydashboard)
library(tidyr)
library(lubridate)
library(stringr)
library(ISOweek)
library(zoo)
library(plotly)

# ------------------------------------------------------------------------------
# Data
# ------------------------------------------------------------------------------
setwd("D:/03_Workspace/02_Leaflet/Tutorial_1")

data_sars <- read_excel("data/Data_SARCoV.xlsx")

data_sars <- data_sars %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
  mutate(across(where(is.character), ~ replace_na(., "")))

data_sars <- data_sars %>%
  mutate(
    iso_week = str_replace(`yyyy-w (SARS-CoV)`, "_", "-W"),
    `yyyy-w (SARS-CoV)` = ISOweek2date(paste0(iso_week, "-1"))
  )

data_sars <- data_sars %>%
  arrange(`yyyy-w (SARS-CoV)`) %>%
  mutate(
    mov_avg = rollmean(`SARS-CoV-Nat`, k = 5, fill = NA, align = "right"),
    mov_sd = rollapply(`SARS-CoV-Nat`, width = 5, FUN = sd, fill = NA, align = "right"),
    mov_se = mov_sd / sqrt(5),
    ci_upper = mov_avg + 1.96 * mov_se,
    ci_lower = mov_avg - 1.96 * mov_se
  )

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------
ui <- page_sidebar(
  
  # dark mode
  # input_dark_mode(id = "mode"), 
  
  # css
  tags$head(
    tags$style(HTML("
    html, body {
      height: auto !important;
      overflow-y: scroll;
    }

    .bslib-page-sidebar {
      height: auto !important;
      overflow-y: visible !important;
    }
  "))
  ),
  
  # title = "title panel",
  sidebar = sidebar("SARS-Cov 2"),
  
  
  style = "min-height: auto; overflow-y: auto;",
  
  #---------------
  # main Information 
  #---------------
  card(
    card_header("SARS-CoV-2", style = "font-size: 32px;"),
    card_title("Virus and disease overview", style = "font-size: 18px; font-weight: bold;"),
    "SARS-CoV-2, an acronym for Severe Acute Respiratory Syndrome Coronavirus 2, is the virus responsible for COVID-19, an infectious respiratory disease that is spread primarily through respiratory droplets and aerosols from coughing, sneezing, or talking.",
    "test",
    card_title("Detection in Wastewater and Public Health Relevance", style = "font-size: 18px; font-weight: bold;"),
    "In addition to being present in respiratory secretions, SARS-CoV-2 is shed in the faeces of infected individuals, making it detectable in wastewater. Because many infected people, including young children and asymptomatic individuals, can carry and spread the virus without showing symptoms, wastewater surveillance provides an invaluable method for tracking the virus in communities. This approach provides an early warning system for public health officials, enabling them to monitor virus levels and detect outbreaks even before clinical cases increase. Analysis of SARS-CoV-2 in wastewater has become a key public health tool, complementing traditional testing methods. It allows authorities to track the spread of the virus in different geographical areas, helping to identify trends over time and better allocate health resources.",
    # style = "height: 1040px;"
  ),
  
  #---------------
  # info box with plot
  #---------------
  layout_column_wrap(
    value_box(
      title = "h-RSV Trend",
      value = "Moderate",
      showcase = plotOutput("sparklinePlot"),
      showcase_layout = "bottom"
    ),
    
    value_box(
      title = "Influenza Trend",
      value = "Moderate",
      showcase = plotOutput("sparklinePlot"),
      showcase_layout = "bottom"
    ),
    
    value_box(
      title = "SARS-CoV-2 Trend",
      value = "Moderate",
      showcase = plotOutput("sparklinePlot"),
      showcase_layout = "bottom"
    ),
    # style = "height: 150px;"
  ),
  
  #---------------
  # information box
  # infoBoxes with fill=TRUE
  #---------------
  layout_column_wrap(
    no_of_boxes = 8,
    width = 1/8,
    
    value_box(
      title = "Customer lifetime value",
      value = "$5,000",
      showcase = bsicons::bs_icon("bank2"),
      theme = value_box_theme(bg = "#e6f2fd", fg = "#0B538E"),
      class = "border"
    ),
    
    value_box("Situation", "Moderate"),
    value_box("Population", "67%"),
    
    value_box("Cantons covered", 5),
    value_box("Treatment plants", 8),
    value_box("Approval", 3793 ),
    
    value_box("Progress", "80%", showcase = icon("list")),
    value_box("Approval", "95%", showcase = icon("thumbs-up")),
    
    # style = "height: 400px;"
  ),
  
  #---------------
  # plot and maps
  #---------------
  layout_columns(
    # map
    card(
      leafletOutput("myMap"),
      style = "height: 500px;",
      width = 1/3
    ),
    
    # plot
    card(
      # plotOutput("sarsPlot"),
      plotlyOutput("sarsPlot"),
      width = 2/3
    )
  )
)

# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  #---------------
  # Plot
  #---------------
  output$sarsPlot <- renderPlotly({  #renderPlot({
    
    sars_plot <- ggplot(data_sars, aes(x = `yyyy-w (SARS-CoV)`)) +
      geom_point(aes(y = `SARS-CoV-Nat`), color = "blue", size = 1) +
      geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "red", alpha = 0.2) +
      geom_line(aes(y = mov_avg), color = "red", size = 1) +
      # scale_y_log10() +
      theme_bw() +
      labs(
        x = "Week (yyyy-w)",
        y = "Concentration",
        title = "SARS-CoV-2 National"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    ggplotly(sars_plot)
  })
  
  # value box plot
  output$sparklinePlot <- renderPlot({
    ggplot(data_sars, aes(x = as.Date(`yyyy-w (SARS-CoV)`), y = `SARS-CoV-Nat`)) +
      geom_area(fill = "#e6f2fd") +
      geom_line(color = "#0B538E", size = 0.5) +
      theme_void() +
      theme(plot.margin = margin(0, 0, 0, 0))  # Removes padding
  }, res = 96)  # Increase to 144 for HiDPI displays
  
  #---------------
  # map
  #---------------
  output$myMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 6.1296, lat = 49.8153, zoom = 9.25)
  })
  
  #---------------
  # info box
  #---------------
  
}

shinyApp(ui, server)
