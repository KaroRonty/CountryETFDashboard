library(shiny)
library(DT)
library(plotly)
library(scales)
library(ggrepel)
library(quantmod)
library(lubridate)
library(tidyverse)

options("getSymbols.yahoo.warning" = FALSE)
options("getSymbols.warning4.0" = FALSE)
options(scipen = 1e9)

# Define countries for selection
selected_countries <- data.frame(
  country = c("Greece", "Russia", "Ireland", "Argentina", "Italy", "Austria",
              "Portugal", "Israel", "Spain", "Brazil", "Turkey", "Singapore",
              "Poland", "Belgium","China", "Norway", "Finland", "Netherlands",
              "United Kingdom", "New Zealand", "France", "Egypt", "Thailand",
              "Taiwan", "Australia", "South Korea", "Hong Kong", "Germany",
              "Sweden", "Chile", "India", "Switzerland", "Canada", "Mexico",
              "Peru", "South Africa", "Malaysia", "Japan", "Philippines",
              "Colombia", "Denmark", "USA", "Indonesia"))

ui <- fluidPage(
  tags$head(
    tags$style(".multi-wrapper {height: 700px; width: 75%}"),
    tags$style(".multi-wrapper .non-selected-wrapper,
               .multi-wrapper .selected-wrapper {height: 90%;}")
  ),
  sidebarLayout(
    sidebarPanel(
      # Country selector
      selectizeInput("countries", "Select countries",
                     choices = sort(as.character(selected_countries$country)),
                     selected = c("China", "Greece", "Russia", "Turkey", "USA"),
                     multiple = TRUE)),
    mainPanel(
      fluidRow(plotlyOutput("plot")),
      fluidRow(DTOutput("table"))
    )
  )
)