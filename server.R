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

# The starting date of the returns
start_date <- "2014-01-01"

# Make a data frame for countries and tickers
countries <- data.frame(
  c("Greece", "Russia", "Ireland", "Argentina", "Italy", "Austria", "Portugal",
    "Israel", "Spain", "Brazil", "Turkey", "Singapore", "Poland", "Belgium",
    "China", "Norway", "Finland", "Netherlands", "United Kingdom",
    "New Zealand", "France", "Egypt", "Thailand", "Taiwan", "Australia",
    "South Korea", "Hong Kong", "Germany", "Sweden", "Chile", "India",
    "Switzerland", "Canada", "Mexico", "Peru", "South Africa", "Malaysia",
    "Japan", "Philippines", "Colombia", "Denmark", "USA", "Indonesia"),
  c("GREK", "ERUS", "EIRL", "ARGT", "EWI", "EWO", "PGAL", "EIS", "EWP",
    "EWZ", "TUR", "EWS", "EPOL", "EWK", "MCHI", "NORW", "EFNL", "EWN",
    "EWU", "ENZL", "EWQ", "EGPT", "THD", "EWT", "EWA", "EWY", "EWH", "EWG",
    "EWD", "ECH", "INDY", "EWL", "EWC", "EWW", "EPU", "EZA", "EWM", "EWJ",
    "EPHE", "GXG", "EDEN", "VTI", "EIDO")
)

# Rename columns
countries <- countries %>% 
  rename(Country = 1,
         Ticker = 2)

# Intialize data frame for stock data
data <- as.data.frame(NULL)

# Put the return data to a data frame
for (i in 1:nrow(countries)) {
  tick <- as.character(countries$Ticker[i])
  temp <- get(getSymbols(tick, from = start_date))[, 6]
  # Remove clutter from column name
  names(temp) <- substr(names(temp), 1, gregexpr("\\.", names(temp))[[1]] - 1)
  # Add rownames to column for joining
  temp <- rownames_to_column(as.data.frame(temp))
  assign(tick, temp)
  if (i != 1) {
    data <- suppressMessages(full_join(as.data.frame(data), temp))
  } else {
    data <- rbind(data, temp)
  }
}

# Rename date column
data <- data %>% 
  rename(Date = 1)

# Calculate ETF returns
returns <- data %>% 
  mutate_if(is.numeric, function(x) x / x[1] - 1) %>% 
  mutate(Date = as.Date(Date)) %>% 
  rename_at(as.character(countries$Ticker), ~ as.character(countries$Country))

# Pivot and join tickers
returns_pivoted <- returns %>% 
  pivot_longer(Greece:Indonesia) %>% 
  rename(Country = 2,
         Return = 3) %>% 
  left_join(countries)

# Function for dynamically making the plot based on input countries
make_plot <- function(data, input){
  returns_dynamic <- data %>% 
    filter(Country %in% input)
  
  p <- returns_dynamic %>% 
    ggplot(aes(Date, Return, color = Country, label = Ticker)) +
    geom_line() +
    scale_x_date(date_labels = "%Y", breaks = "1 year") +
    scale_y_continuous(labels = scales::percent) +
    ggtitle("Returns of country ETFs") +
    xlab("Date") +
    ylab("Return") +
    theme_minimal()
  
  return(ggplotly(p))
}

# Define color ramp function ranging from blue to red
ramp <- colorRampPalette(c("#5A8AC6", "white", "#F8696B"))

# Function for getting the quantiles of each column
get_q <- function(n, df){
  quantile(df[, n], probs = seq_len(nrow(na.omit(df[, n]))) /
             (nrow(na.omit(df[, n])) - 1) - 1 /
             (nrow(na.omit(df[, n])) - 1),
           na.rm = TRUE)
}

# Function for condinational formatting of the tables
conditional_format <- function(df){
  datatable(df) %>%
    formatStyle(colnames(df)[2],
                backgroundColor = styleInterval(
                  get_q(2, df), 
                  ramp(nrow(na.omit(df[, 1])) + 1))) %>%
    formatStyle(colnames(df)[3],
                backgroundColor = styleInterval(
                  get_q(3, df), 
                  ramp(nrow(na.omit(df[, 1])) + 1)))
}

# Function for dynamically making the table based on input countries
make_table <- function(data, input){
  data %>% 
    filter(Country %in% input) %>% 
    group_by(Country) %>% 
    filter(Date == last(returns$Date)) %>%
    mutate(CAGR = ((Return + 1)^(1 / (interval(start_date,
                                               last(returns$Date)) /
                                        years(1))) - 1) %>% 
             round(3),
           `Total return` = round(Return, 3)) %>% 
    select(Country, CAGR, `Total return`, Ticker) %>% 
    arrange(-CAGR)
}

server <- function(input, output, session){
  
  # Render dynamic plot of ETF returns
  output$plot <- renderPlotly({
    make_plot(returns_pivoted, input$countries)
  })
  
  # Render dynamic table containing returns and tickers
  output$table <- renderDT({
    make_table(returns_pivoted, input$countries) %>% 
      conditional_format()
  })
}
