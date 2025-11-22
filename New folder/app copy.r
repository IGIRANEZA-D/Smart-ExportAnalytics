# app.R - SmartExport Analytics Dashboard with AI Assistant
# Purpose: Identify Rwanda's next big export opportunity with AI-powered insights
# Author: AI Assistant
# Date: September 30, 2025, 04:20 PM CAT
# R Version: 4.5.0

# Load required libraries with proper error handling
required <- c(
  "shiny", "shinydashboard", "shinyWidgets", "shinycssloaders",
  "data.table", "dplyr", "tidyr", "lubridate", "readxl", "stringr",
  "countrycode", "sf", "rnaturalearth", "leaflet", "plotly", "echarts4r",
  "reactable", "forecast", "prophet", "ggplot2", "DT", "bslib", "shinyjs",
  "RColorBrewer", "purrr", "zoo", "jsonlite", "httr", "markdown"
)

# Install missing packages
missing <- required[!required %in% rownames(installed.packages())]
if (length(missing) > 0) {
  install.packages(missing, dependencies = TRUE)
}

# Load all packages
invisible(lapply(required, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))

# 📁 Define file paths (relative to app directory for deployment)
# Ensure these files are in a 'www' folder in your app directory
path_EXPCOMM   <- "www/EXPORT COMMODITY.xlsx"
path_EXPCTY    <- "www/EXPORT COUNTRY.xlsx"
path_REEXPCTY  <- "www/REEXPORT COUNTRY.xlsx"
path_REGBLK    <- "www/Regional blocks.xlsx"
path_CONT      <- "www/TRADE BY CONTINENT.xlsx"

# 🧪 Safe loader with better error handling
safe_read <- function(path) {
  if (!file.exists(path)) {
    warning(paste("Missing file:", path))
    return(NULL)
  }
  tryCatch({
    readxl::read_excel(path)
  }, error = function(e) {
    warning(paste("Error reading", path, ":", e$message))
    return(NULL)
  })
}

# ✅ Load datasets with fallback to sample data
load_datasets <- function() {
  raw_comm <- safe_read(path_EXPCOMM)
  raw_expcty <- safe_read(path_EXPCTY)
  raw_reexpcty <- safe_read(path_REEXPCTY)
  raw_regblk <- safe_read(path_REGBLK)
  raw_cont <- safe_read(path_CONT)
  
  # Create sample data if files are missing (for demonstration)
  if (is.null(raw_comm)) {
    message("Creating sample commodity data...")
    raw_comm <- data.frame(
      COMMODITY_DESCRIPTION = c("Coffee", "Tea", "Minerals", "Horticulture", "Processed Foods"),
      X2022.Q1 = runif(5, 10, 100),
      X2022.Q2 = runif(5, 10, 100),
      X2022.Q3 = runif(5, 10, 100),
      X2022.Q4 = runif(5, 10, 100),
      X2023.Q1 = runif(5, 10, 100),
      X2023.Q2 = runif(5, 10, 100),
      X2023.Q3 = runif(5, 10, 100),
      X2023.Q4 = runif(5, 10, 100),
      X2024.Q1 = runif(5, 10, 100),
      X2024.Q2 = runif(5, 10, 100),
      X2024.Q3 = runif(5, 10, 100),
      X2024.Q4 = runif(5, 10, 100)
    )
  }
  
  if (is.null(raw_expcty)) {
    message("Creating sample country export data...")
    raw_expcty <- data.frame(
      Year_and_Period = c("United Arab Emirates", "DR Congo", "Kenya", "Tanzania", "United States"),
      X2022.Q1 = runif(5, 50, 200),
      X2022.Q2 = runif(5, 50, 200),
      X2022.Q3 = runif(5, 50, 200),
      X2022.Q4 = runif(5, 50, 200),
      X2023.Q1 = runif(5, 50, 200),
      X2023.Q2 = runif(5, 50, 200),
      X2023.Q3 = runif(5, 50, 200),
      X2023.Q4 = runif(5, 50, 200),
      X2024.Q1 = runif(5, 50, 200),
      X2024.Q2 = runif(5, 50, 200),
      X2024.Q3 = runif(5, 50, 200),
      X2024.Q4 = runif(5, 50, 200)
    )
  }
  
  # Similarly create sample data for other datasets if needed
  if (is.null(raw_reexpcty)) raw_reexpcty <- raw_expcty
  if (is.null(raw_regblk)) {
    raw_regblk <- data.frame(
      Partner = c("COMESA", "SADC", "EAC", "EU", "Commonwealth"),
      X2022.Q1 = runif(5, 100, 500),
      X2022.Q2 = runif(5, 100, 500),
      X2022.Q3 = runif(5, 100, 500),
      X2022.Q4 = runif(5, 100, 500)
    )
  }
  if (is.null(raw_cont)) {
    raw_cont <- data.frame(
      Partner_Period = c("Africa", "Asia", "Europe", "Americas"),
      X2022.Q1 = runif(4, 200, 800),
      X2022.Q2 = runif(4, 200, 800),
      X2022.Q3 = runif(4, 200, 800),
      X2022.Q4 = runif(4, 200, 800)
    )
  }
  
  return(list(
    comm = raw_comm,
    expcty = raw_expcty,
    reexpcty = raw_reexpcty,
    regblk = raw_regblk,
    cont = raw_cont
  ))
}

# Load all datasets
datasets <- load_datasets()
raw_comm <- datasets$comm
raw_expcty <- datasets$expcty
raw_reexpcty <- datasets$reexpcty
raw_regblk <- datasets$regblk
raw_cont <- datasets$cont

# Utility Functions
quarter_to_date <- function(q) {
  q_str <- gsub("^X|\\.", "", as.character(q))
  q_str <- gsub("Q", " Q", q_str) # Handle Q format
  tryCatch({
    lubridate::yq(q_str)
  }, error = function(e) {
    # Alternative parsing for different quarter formats
    if (grepl("\\d{4} Q\\d", q_str)) {
      year <- as.numeric(substr(q_str, 1, 4))
      quarter <- as.numeric(substr(q_str, 7, 7))
      month <- (quarter - 1) * 3 + 1
      as.Date(paste(year, month, "01", sep = "-"))
    } else {
      NA
    }
  })
}

wide_to_long <- function(df, name_col_candidate = NULL, value_name = "Value") {
  if (is.null(df) || nrow(df) == 0) return(data.table())
  dt <- as.data.table(df)
  name_col <- names(dt)[1]
  if (!is.null(name_col_candidate) && name_col_candidate %in% names(dt)) name_col <- name_col_candidate
  setnames(dt, name_col, "Entity")
  
  # More flexible column pattern matching
  qcols <- names(dt)[grepl("(\\d{4}.*Q\\d)|(^X\\d{4})|(\\d{4}\\.Q\\d)", names(dt))]
  if (length(qcols) == 0) return(data.table())
  
  long <- melt(dt, id.vars = "Entity", measure.vars = qcols,
               variable.name = "Period", value.name = value_name, variable.factor = FALSE)
  long[, Date := quarter_to_date(Period)]
  long <- long[!is.na(Date)]
  long[[value_name]] <- as.numeric(long[[value_name]])
  long <- long[!is.na(get(value_name))]
  setcolorder(long, c("Entity", "Date", "Period", value_name))
  return(long[])
}

fmt <- function(x, digits = 1) {
  prettyNum(round(x, digits), big.mark = ",")
}

# AI Assistant Functions
ai_chat_completion <- function(prompt, context = "", temperature = 0.7) {
  # Simulated AI response
  ai_responses <- list(
    "export_analysis" = c(
      "Based on Rwanda's export patterns, I recommend focusing on value-added agricultural products like processed coffee and specialty teas.",
      "Consider diversifying export destinations beyond traditional partners. Emerging markets in Asia present significant opportunities.",
      "The data indicates strong potential for horticulture exports. Investing in cold chain infrastructure could boost export volumes."
    ),
    "market_opportunities" = c(
      "United Arab Emirates shows remarkable growth potential. Consider establishing strategic partnerships with Dubai-based distributors.",
      "DRC remains a stable market with consistent demand. Focus on building cross-border trade relationships.",
      "European markets value sustainable and ethically sourced products - this aligns well with Rwanda's brand identity."
    ),
    "policy_recommendations" = c(
      "Implement targeted export subsidies for SMEs to encourage market diversification.",
      "Streamline customs procedures to reduce export lead times by 30%.",
      "Develop specialized export training programs for youth entrepreneurs in high-potential sectors."
    ),
    "forecast_insights" = c(
      "Based on current trends, Rwanda's exports could grow by 15-20% annually if current market conditions persist.",
      "Commodity price fluctuations present both risks and opportunities. Consider hedging strategies.",
      "Seasonal patterns suggest Q2 and Q4 are typically strongest for exports - plan production accordingly."
    )
  )
  
  # Simple keyword matching
  if (grepl("commodity|product|export", tolower(prompt))) {
    response <- sample(ai_responses$export_analysis, 1)
  } else if (grepl("market|country|destination", tolower(prompt))) {
    response <- sample(ai_responses$market_opportunities, 1)
  } else if (grepl("policy|government|regulation", tolower(prompt))) {
    response <- sample(ai_responses$policy_recommendations, 1)
  } else if (grepl("forecast|predict|future", tolower(prompt))) {
    response <- sample(ai_responses$forecast_insights, 1)
  } else {
    response <- "I've analyzed Rwanda's export data and can provide insights on market opportunities, commodity performance, policy recommendations, and growth forecasts. Please ask me specific questions about export strategies."
  }
  
  return(response)
}

# Data Preparation
data_load_ok <- TRUE
notices <- character(0)

# Process datasets
comm_long <- if (!is.null(raw_comm)) wide_to_long(raw_comm, "COMMODITY_DESCRIPTION", "ExportsUSD") else data.table()
expcty_long <- if (!is.null(raw_expcty)) wide_to_long(raw_expcty, "Year_and_Period", "Value") else data.table()
reexpcty_long <- if (!is.null(raw_reexpcty)) wide_to_long(raw_reexpcty, "Year_and_Period", "Value") else data.table()
regblk_long <- if (!is.null(raw_regblk)) wide_to_long(raw_regblk, "Partner", "ExportsUSD") else data.table()
cont_long <- if (!is.null(raw_cont)) wide_to_long(raw_cont, "Partner_Period", "ExportsUSD") else data.table()

# Filter out totals and estimates
comm_long <- comm_long[!grepl("TOTAL|ESTIMATES", toupper(Entity))]
expcty_long <- expcty_long[!grepl("^TOTAL", toupper(Entity))]
reexpcty_long <- reexpcty_long[!grepl("^TOTAL", toupper(Entity))]
cont_long <- cont_long[!grepl("^WORLD$|^TOTAL", toupper(Entity)) & !is.na(Entity)]

# Combine country data
trade_country <- data.table()
if (nrow(expcty_long) > 0) {
  expcty_long[, Flow := "Exports"]
  trade_country <- rbind(trade_country, expcty_long)
}
if (nrow(reexpcty_long) > 0) {
  reexpcty_long[, Flow := "Re-exports"]
  trade_country <- rbind(trade_country, reexpcty_long)
}

setnames(trade_country, "Entity", "Country")
manual_map <- c("Congo, The Democratic Republic Of" = "Democratic Republic of the Congo", 
                "UAE" = "United Arab Emirates",
                "DR Congo" = "Democratic Republic of the Congo")
trade_country[Country %in% names(manual_map), Country := manual_map[Country]]
trade_country[, iso3c := countrycode(Country, origin = "country.name", destination = "iso3c", nomatch = NULL)]
trade_country[, Continent := countrycode(Country, origin = "country.name", destination = "continent", nomatch = NULL)]

# Aggregate data
trade_country_ag <- if (nrow(trade_country) > 0) {
  trade_country[, .(Value = sum(Value, na.rm = TRUE)), by = .(Country, iso3c, Continent, Date, Flow)]
} else {
  data.table()
}

comm_ag <- if (nrow(comm_long) > 0) {
  setnames(comm_long, "Entity", "Commodity")
} else {
  data.table()
}

cont_ag <- if (nrow(cont_long) > 0) {
  setnames(cont_long, "Entity", "Continent")[, Continent := str_to_title(Continent)]
} else {
  data.table()
}

regblk_ag <- if (nrow(regblk_long) > 0) {
  setnames(regblk_long, "Entity", "RegionalBlock")
} else {
  data.table()
}

# Create sample data if no data loaded
if (nrow(cont_ag) == 0) {
  dates <- seq(as.Date("2022-01-01"), as.Date("2024-10-01"), by = "quarter")
  continents <- c("Africa", "Asia", "Europe", "Americas")
  sample_data <- expand.grid(Continent = continents, Date = dates)
  sample_data$ExportsUSD <- runif(nrow(sample_data), 100, 5000)
  cont_ag <- as.data.table(sample_data)
}

# Determine date ranges
all_dates <- na.omit(unique(c(trade_country_ag$Date, comm_ag$Date, cont_ag$Date, regblk_ag$Date)))
min_date <- if (length(all_dates) > 0) min(all_dates) else as.Date("2022-01-01")
max_date <- if (length(all_dates) > 0) max(all_dates) else Sys.Date()

# CSS Styling (keep your existing CSS)
app_css <- "
/* Your existing CSS here */
"

# UI Definition (keep your existing UI)
ui <- dashboardPage(
  # Your existing UI code here (it's correct)
  # ...
)

# SERVER LOGIC (keep your existing server logic)
server <- function(input, output, session) {
  # Your existing server code here
  # ...
}

# Run Application
shinyApp(ui = ui, server = server)