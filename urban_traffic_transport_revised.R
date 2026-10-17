# ============================================================
# URBAN TRAFFIC AND PUBLIC TRANSPORT INTELLIGENT SYSTEM
# Complete R Analysis Script (Revised: additional columns)
# ============================================================
#
# Dataset:
# MTA Daily Ridership and Traffic: Beginning 2020
#
# Source:
# New York State Open Data / Metropolitan Transportation Authority
#
# Main prediction target:
# Daily Subway Ridership
#
# ============================================================


# ============================================================
# STEP 1 — INSTALL REQUIRED PACKAGES
# ============================================================

required_packages <- c(
  "tidyverse",
  "lubridate",
  "rmarkdown",
  "knitr"
)

installed_packages <- rownames(
  installed.packages()
)

for (package in required_packages) {
  
  if (!(package %in% installed_packages)) {
    
    install.packages(
      package,
      dependencies = TRUE
    )
    
  }
  
}


# ============================================================
# STEP 2 — LOAD REQUIRED LIBRARIES
# ============================================================

library(tidyverse)
library(lubridate)
library(rmarkdown)
library(knitr)


# ============================================================
# STEP 3 — CREATE PROJECT OUTPUT FOLDERS
# ============================================================

if (!dir.exists("data")) {
  dir.create("data")
}

if (!dir.exists("figures")) {
  dir.create("figures")
}

if (!dir.exists("output")) {
  dir.create("output")
}


# ============================================================
# STEP 4 — DOWNLOAD OFFICIAL DATASET
# ============================================================

url_csv <- "https://data.ny.gov/api/views/sayj-mze2/rows.csv?accessType=DOWNLOAD"

url_soda <- "https://data.ny.gov/resource/sayj-mze2.csv?$limit=500000&$order=date,mode"

local_file <- "data/MTA_transport_data_original.csv"

options(timeout = 300)


# Download one source and standardise it to Date / Mode / Count

read_mta <- function(src) {
  
  tmp <- tempfile(fileext = ".csv")
  
  utils::download.file(
    src,
    tmp,
    mode = "wb",
    quiet = TRUE,
    method = "libcurl",
    headers = c(
      "User-Agent" = "Mozilla/5.0 (R data download)",
      "Accept" = "text/csv"
    )
  )
  
  df <- read.csv(
    tmp,
    stringsAsFactors = FALSE
  )
  
  names(df) <- tools::toTitleCase(
    tolower(names(df))
  )
  
  if (!all(c("Date", "Mode", "Count") %in% names(df))) {
    stop("Downloaded file does not have Date, Mode and Count columns.")
  }
  
  # The SODA endpoint returns ISO dates (2020-03-01T00:00:00.000).
  # Convert them to the original MM/DD/YYYY text so the rest of
  # the script works without any change.
  
  if (any(grepl("^[0-9]{4}-", df$Date))) {
    df$Date <- format(
      as.Date(substr(df$Date, 1, 10)),
      "%m/%d/%Y"
    )
  }
  
  df$Count <- as.numeric(df$Count)
  
  df[, c("Date", "Mode", "Count")]
  
}


cat("\nDownloading MTA transportation dataset...\n")

traffic_data <- NULL

for (src in c(url_csv, url_soda)) {
  
  traffic_data <- tryCatch(
    read_mta(src),
    error = function(e) {
      cat("  Failed:", src, "\n  Reason:", conditionMessage(e), "\n")
      NULL
    },
    warning = function(w) {
      cat("  Failed:", src, "\n  Reason:", conditionMessage(w), "\n")
      NULL
    }
  )
  
  if (!is.null(traffic_data)) {
    cat("Dataset downloaded from:", src, "\n")
    break
  }
  
}


# Last fallback: a copy saved earlier, or a file placed manually

if (is.null(traffic_data) && file.exists(local_file)) {
  
  cat("Using local copy:", local_file, "\n")
  
  traffic_data <- read.csv(
    local_file,
    stringsAsFactors = FALSE
  )
  
}

if (is.null(traffic_data)) {
  
  stop(
    "Could not download the dataset. Download the CSV manually from ",
    "https://data.ny.gov/d/sayj-mze2 (Export > CSV), save it as ",
    local_file,
    " and run the script again."
  )
  
}

cat("Dataset loaded successfully.\n")


# ============================================================
# STEP 5 — SAVE ORIGINAL DATASET
# ============================================================

write.csv(
  traffic_data,
  "data/MTA_transport_data_original.csv",
  row.names = FALSE
)


# ============================================================
# STEP 6 — BASIC DATA INSPECTION
# ============================================================

cat("\n============================================\n")
cat("DATASET PREVIEW\n")
cat("============================================\n")

print(head(traffic_data))


cat("\n============================================\n")
cat("DATASET DIMENSIONS\n")
cat("============================================\n")

print(dim(traffic_data))


cat("\n============================================\n")
cat("COLUMN NAMES\n")
cat("============================================\n")

print(names(traffic_data))


cat("\n============================================\n")
cat("DATA STRUCTURE\n")
cat("============================================\n")

str(traffic_data)


# ============================================================
# STEP 7 — DATASET SUMMARY
# ============================================================

cat("\n============================================\n")
cat("SUMMARY STATISTICS\n")
cat("============================================\n")

print(summary(traffic_data))

# Data Preprocessing:
# Check and handle missing values before analysis.
# ============================================================
# STEP 8 — CHECK MISSING VALUES
# ============================================================

missing_values <- colSums(
  is.na(traffic_data)
)

cat("\n============================================\n")
cat("MISSING VALUES BY COLUMN\n")
cat("============================================\n")

print(missing_values)


# ============================================================
# STEP 9 — CHECK DUPLICATE ROWS
# ============================================================

duplicate_rows <- sum(
  duplicated(traffic_data)
)

cat("\n============================================\n")
cat("DUPLICATE ROWS\n")
cat("============================================\n")

cat(
  "Number of duplicate rows:",
  duplicate_rows,
  "\n"
)


# ============================================================
# STEP 10 — CHECK UNIQUE TRANSPORTATION MODES
# ============================================================

cat("\n============================================\n")
cat("TRANSPORTATION MODES\n")
cat("============================================\n")

print(
  unique(traffic_data$Mode)
)


# ============================================================
# STEP 11 — CHECK DATE RANGE
# ============================================================

cat("\n============================================\n")
cat("DATE INFORMATION\n")
cat("============================================\n")

cat(
  "Minimum date:",
  min(
    as.Date(
      traffic_data$Date,
      format = "%m/%d/%Y"
    ),
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "Maximum date:",
  max(
    as.Date(
      traffic_data$Date,
      format = "%m/%d/%Y"
    ),
    na.rm = TRUE
  ),
  "\n"
)


# ============================================================
# STEP 12 — CHECK NEGATIVE COUNTS
# ============================================================

negative_counts <- sum(
  traffic_data$Count < 0,
  na.rm = TRUE
)

cat("\n============================================\n")
cat("NEGATIVE COUNTS\n")
cat("============================================\n")

cat(
  "Number of negative Count values:",
  negative_counts,
  "\n"
)


# ============================================================
# STEP 13 — CONVERT DATE COLUMN
# ============================================================

traffic_data$Date <- as.Date(
  traffic_data$Date,
  format = "%m/%d/%Y"
)


# ============================================================
# STEP 14 — REMOVE MISSING VALUES
# ============================================================

clean_data <- traffic_data %>%
  filter(
    !is.na(Date),
    !is.na(Mode),
    !is.na(Count)
  )


# ============================================================
# STEP 15 — REMOVE DUPLICATE RECORDS
# ============================================================

clean_data <- clean_data %>%
  distinct()


# ============================================================
# STEP 16 — REMOVE INVALID NEGATIVE COUNTS
# ============================================================

clean_data <- clean_data %>%
  filter(
    Count >= 0
  )


# ============================================================
# STEP 17 — CHECK CLEANED DATASET
# ============================================================

cat("\n============================================\n")
cat("CLEANED DATASET\n")
cat("============================================\n")

cat(
  "Rows before cleaning:",
  nrow(traffic_data),
  "\n"
)

cat(
  "Rows after cleaning:",
  nrow(clean_data),
  "\n"
)

cat(
  "Columns:",
  ncol(clean_data),
  "\n"
)


# ============================================================
# STEP 18 — SAVE CLEANED DATASET
# ============================================================

write.csv(
  clean_data,
  "data/MTA_transport_data_cleaned.csv",
  row.names = FALSE
)


# ============================================================
# STEP 19 — FEATURE ENGINEERING (BASIC)
# ============================================================

clean_data <- clean_data %>%
  mutate(
    Day = weekdays(Date),
    Month = month(
      Date,
      label = TRUE,
      abbr = TRUE
    ),
    Year = year(Date)
  )


# ============================================================
# STEP 19A — ADDITIONAL CALENDAR COLUMNS (NEW)
# ============================================================
#
# New columns:
# Month_Name, Quarter, Week_of_Year, Day_of_Month,
# Day_of_Year, Is_Weekend, Day_Type, Season
#
# ============================================================

clean_data <- clean_data %>%
  mutate(
    Month_Name = as.character(
      month(Date, label = TRUE, abbr = FALSE)
    ),
    Quarter = quarter(Date),
    Week_of_Year = isoweek(Date),
    Day_of_Month = mday(Date),
    Day_of_Year = yday(Date),
    Is_Weekend = ifelse(
      wday(Date) %in% c(1, 7),
      1,
      0
    ),
    Day_Type = ifelse(
      Is_Weekend == 1,
      "Weekend",
      "Weekday"
    ),
    Season = case_when(
      month(Date) %in% c(12, 1, 2) ~ "Winter",
      month(Date) %in% c(3, 4, 5) ~ "Spring",
      month(Date) %in% c(6, 7, 8) ~ "Summer",
      TRUE ~ "Autumn"
    )
  )


# ============================================================
# STEP 19B — US FEDERAL HOLIDAY COLUMNS (NEW)
# ============================================================
#
# New columns: Is_Holiday, Holiday_Name
#
# ============================================================

nth_weekday <- function(year, month, weekday, n) {
  
  first_day <- as.Date(
    sprintf("%d-%02d-01", year, month)
  )
  
  all_days <- seq(
    first_day,
    by = "day",
    length.out = 31
  )
  
  all_days <- all_days[
    month(all_days) == month
  ]
  
  matches <- all_days[
    wday(all_days) == weekday
  ]
  
  if (n == "last") {
    return(tail(matches, 1))
  }
  
  matches[n]
  
}


build_holidays <- function(years) {
  
  out <- lapply(years, function(y) {
    
    data.frame(
      Date = c(
        as.Date(sprintf("%d-01-01", y)),
        nth_weekday(y, 1, 2, 3),
        nth_weekday(y, 2, 2, 3),
        nth_weekday(y, 5, 2, "last"),
        as.Date(sprintf("%d-06-19", y)),
        as.Date(sprintf("%d-07-04", y)),
        nth_weekday(y, 9, 2, 1),
        nth_weekday(y, 10, 2, 2),
        as.Date(sprintf("%d-11-11", y)),
        nth_weekday(y, 11, 5, 4),
        as.Date(sprintf("%d-12-25", y))
      ),
      Holiday_Name = c(
        "New Year's Day",
        "Martin Luther King Jr. Day",
        "Presidents' Day",
        "Memorial Day",
        "Juneteenth",
        "Independence Day",
        "Labor Day",
        "Columbus Day",
        "Veterans Day",
        "Thanksgiving",
        "Christmas Day"
      ),
      stringsAsFactors = FALSE
    )
    
  })
  
  do.call(rbind, out)
  
}


holiday_table <- build_holidays(
  seq(
    min(clean_data$Year),
    max(clean_data$Year)
  )
)


clean_data <- clean_data %>%
  left_join(
    holiday_table,
    by = "Date"
  ) %>%
  mutate(
    Is_Holiday = ifelse(
      is.na(Holiday_Name),
      0,
      1
    ),
    Holiday_Name = ifelse(
      is.na(Holiday_Name),
      "None",
      Holiday_Name
    )
  )


# ============================================================
# STEP 19C — COVID PHASE COLUMN (NEW)
# ============================================================
#
# New column: COVID_Phase
#
# ============================================================

clean_data <- clean_data %>%
  mutate(
    COVID_Phase = case_when(
      Date < as.Date("2020-07-01") ~ "1. Early COVID (to Jun 2020)",
      Date < as.Date("2022-01-01") ~ "2. Recovery (Jul 2020 - Dec 2021)",
      TRUE ~ "3. Post-Recovery (2022 onwards)"
    )
  )


# ============================================================
# STEP 19D — TREND COLUMNS WITHIN EACH MODE (NEW)
# ============================================================
#
# New columns:
# Previous_Day_Count, Daily_Change, Daily_Pct_Change,
# Rolling_7Day_Avg, Same_Day_Last_Week
#
# ============================================================

rolling_mean_7 <- function(x) {
  as.numeric(
    stats::filter(
      x,
      rep(1 / 7, 7),
      sides = 1
    )
  )
}


clean_data <- clean_data %>%
  arrange(Mode, Date) %>%
  group_by(Mode) %>%
  mutate(
    Previous_Day_Count = lag(Count, 1),
    Daily_Change = Count - Previous_Day_Count,
    Daily_Pct_Change = round(
      100 * Daily_Change / Previous_Day_Count,
      2
    ),
    Rolling_7Day_Avg = rolling_mean_7(Count),
    Same_Day_Last_Week = lag(Count, 7)
  ) %>%
  ungroup() %>%
  mutate(
    Daily_Pct_Change = ifelse(
      is.infinite(Daily_Pct_Change),
      NA,
      Daily_Pct_Change
    )
  )


# ============================================================
# STEP 19E — PREVIEW AND SAVE FEATURE DATASET (NEW)
# ============================================================

cat("\n============================================\n")
cat("ENGINEERED COLUMNS\n")
cat("============================================\n")

print(names(clean_data))

print(head(clean_data))


write.csv(
  clean_data,
  "data/MTA_transport_data_features.csv",
  row.names = FALSE
)
# ==========================================
# STAGE 3: EXPLORATORY DATA ANALYSIS
# AND VISUALIZATION
# ==========================================
# Analyze transportation trends over time.
# Compare ridership across transportation modes.
# Examine weekday and monthly patterns.
# Visualize actual versus predicted ridership.

# ============================================================
# STEP 20 — TRANSPORTATION MODE FREQUENCY
# ============================================================

mode_frequency <- clean_data %>%
  count(
    Mode,
    sort = TRUE
  )

cat("\n============================================\n")
cat("NUMBER OF RECORDS BY MODE\n")
cat("============================================\n")

print(mode_frequency)


# ============================================================
# STEP 21 — AVERAGE COUNT BY MODE
# ============================================================

mode_summary <- clean_data %>%
  group_by(Mode) %>%
  summarise(
    Average_Count = mean(
      Count,
      na.rm = TRUE
    ),
    Maximum_Count = max(
      Count,
      na.rm = TRUE
    ),
    Minimum_Count = min(
      Count,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(
    desc(Average_Count)
  )


cat("\n============================================\n")
cat("TRANSPORTATION MODE SUMMARY\n")
cat("============================================\n")

print(mode_summary)


write.csv(
  mode_summary,
  "output/transportation_mode_summary.csv",
  row.names = FALSE
)


# ============================================================
# STEP 21A — MODE SUMMARY: WEEKDAY VS WEEKEND (NEW)
# ============================================================

mode_daytype_summary <- clean_data %>%
  group_by(Mode, Day_Type) %>%
  summarise(
    Average_Count = mean(
      Count,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Day_Type,
    values_from = Average_Count
  ) %>%
  mutate(
    Weekend_to_Weekday_Ratio = round(
      Weekend / Weekday,
      3
    )
  ) %>%
  arrange(
    desc(Weekday)
  )


cat("\n============================================\n")
cat("MODE SUMMARY: WEEKDAY VS WEEKEND\n")
cat("============================================\n")

print(mode_daytype_summary)


write.csv(
  mode_daytype_summary,
  "output/mode_weekday_weekend_summary.csv",
  row.names = FALSE
)


# ============================================================
# STEP 21B — MODE SUMMARY BY COVID PHASE (NEW)
# ============================================================

covid_phase_summary <- clean_data %>%
  group_by(COVID_Phase, Mode) %>%
  summarise(
    Average_Count = mean(
      Count,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = COVID_Phase,
    values_from = Average_Count
  )


cat("\n============================================\n")
cat("AVERAGE COUNT BY MODE AND COVID PHASE\n")
cat("============================================\n")

print(covid_phase_summary)


write.csv(
  covid_phase_summary,
  "output/covid_phase_summary.csv",
  row.names = FALSE
)


# ============================================================
# STEP 22 — DAILY TOTAL TRANSPORTATION COUNT
# ============================================================

daily_total <- clean_data %>%
  group_by(Date) %>%
  summarise(
    Total_Count = sum(
      Count,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# ============================================================
# STEP 23 — DAILY TOTAL TREND
# ============================================================

plot_daily_total <- ggplot(
  daily_total,
  aes(
    x = Date,
    y = Total_Count
  )
) +
  geom_line() +
  labs(
    title = "Daily Total Transportation Demand",
    x = "Date",
    y = "Total Transportation Count"
  ) +
  theme_minimal()

print(plot_daily_total)

ggsave(
  "figures/daily_total_transportation_demand.png",
  plot = plot_daily_total,
  width = 12,
  height = 6
)


# ============================================================
# STEP 23A — DAILY TOTAL WITH 7-DAY MOVING AVERAGE (NEW)
# ============================================================

daily_total_ma <- daily_total %>%
  arrange(Date) %>%
  mutate(
    Rolling_7Day_Avg = rolling_mean_7(Total_Count)
  )


plot_daily_total_ma <- ggplot(
  daily_total_ma,
  aes(
    x = Date
  )
) +
  geom_line(
    aes(
      y = Total_Count,
      linetype = "Daily total"
    ),
    alpha = 0.5
  ) +
  geom_line(
    aes(
      y = Rolling_7Day_Avg,
      linetype = "7-day average"
    ),
    linewidth = 1,
    na.rm = TRUE
  ) +
  labs(
    title = "Daily Total Demand with 7-Day Moving Average",
    x = "Date",
    y = "Total Transportation Count",
    linetype = "Series"
  ) +
  theme_minimal()

print(plot_daily_total_ma)

ggsave(
  "figures/daily_total_with_7day_average.png",
  plot = plot_daily_total_ma,
  width = 12,
  height = 6
)


# ============================================================
# STEP 24 — AVERAGE COUNT BY MODE GRAPH
# ============================================================

plot_mode <- ggplot(
  mode_summary,
  aes(
    x = reorder(
      Mode,
      Average_Count
    ),
    y = Average_Count
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Average Daily Transportation Count by Mode",
    x = "Transportation Mode",
    y = "Average Daily Count"
  ) +
  theme_minimal()

print(plot_mode)

ggsave(
  "figures/average_transportation_by_mode.png",
  plot = plot_mode,
  width = 10,
  height = 6
)


# ============================================================
# STEP 25 — SUBWAY DATASET
# ============================================================

subway_daily <- clean_data %>%
  filter(
    Mode == "Subway"
  ) %>%
  arrange(
    Date
  )


# ============================================================
# STEP 26 — DAILY SUBWAY RIDERSHIP GRAPH
# ============================================================

plot_subway_trend <- ggplot(
  subway_daily,
  aes(
    x = Date,
    y = Count
  )
) +
  geom_line() +
  labs(
    title = "Daily Subway Ridership Trend",
    x = "Date",
    y = "Subway Ridership"
  ) +
  theme_minimal()

print(plot_subway_trend)

ggsave(
  "figures/daily_subway_ridership_trend.png",
  plot = plot_subway_trend,
  width = 12,
  height = 6
)


# ============================================================
# STEP 27 — AVERAGE SUBWAY RIDERSHIP BY DAY
# ============================================================

subway_day <- subway_daily %>%
  mutate(
    Day = weekdays(Date)
  ) %>%
  group_by(Day) %>%
  summarise(
    Average_Ridership = mean(
      Count,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Correct weekday order

subway_day$Day <- factor(
  subway_day$Day,
  levels = c(
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday",
    "Sunday"
  )
)

subway_day <- subway_day %>%
  arrange(
    Day
  )


cat("\n============================================\n")
cat("AVERAGE SUBWAY RIDERSHIP BY DAY\n")
cat("============================================\n")

print(subway_day)


write.csv(
  subway_day,
  "output/subway_day_summary.csv",
  row.names = FALSE
)


# ============================================================
# STEP 27A — SUBWAY WEEKDAY VS WEEKEND (NEW)
# ============================================================

subway_daytype <- subway_daily %>%
  group_by(Day_Type) %>%
  summarise(
    Days = n(),
    Average_Ridership = mean(
      Count,
      na.rm = TRUE
    ),
    Median_Ridership = median(
      Count,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


cat("\n============================================\n")
cat("SUBWAY RIDERSHIP: WEEKDAY VS WEEKEND\n")
cat("============================================\n")

print(subway_daytype)


write.csv(
  subway_daytype,
  "output/subway_weekday_weekend_summary.csv",
  row.names = FALSE
)


# ============================================================
# STEP 27B — SUBWAY RIDERSHIP ON HOLIDAYS (NEW)
# ============================================================
#
# Only weekdays are compared so that the holiday effect is
# not mixed with the weekend effect.
#
# ============================================================

subway_holiday <- subway_daily %>%
  filter(
    Is_Weekend == 0
  ) %>%
  mutate(
    Holiday_Status = ifelse(
      Is_Holiday == 1,
      "Weekday Holiday",
      "Regular Weekday"
    )
  ) %>%
  group_by(Holiday_Status) %>%
  summarise(
    Days = n(),
    Average_Ridership = mean(
      Count,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


cat("\n============================================\n")
cat("SUBWAY RIDERSHIP: HOLIDAYS VS REGULAR WEEKDAYS\n")
cat("============================================\n")

print(subway_holiday)


write.csv(
  subway_holiday,
  "output/subway_holiday_summary.csv",
  row.names = FALSE
)


# ============================================================
# STEP 28 — DAY OF WEEK GRAPH
# ============================================================

plot_day <- ggplot(
  subway_day,
  aes(
    x = Day,
    y = Average_Ridership
  )
) +
  geom_col() +
  labs(
    title = "Average Subway Ridership by Day of Week",
    x = "Day of Week",
    y = "Average Subway Ridership"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

print(plot_day)

ggsave(
  "figures/subway_ridership_by_day.png",
  plot = plot_day,
  width = 10,
  height = 6
)


# ============================================================
# STEP 29 — AVERAGE SUBWAY RIDERSHIP BY MONTH
# ============================================================

subway_month <- subway_daily %>%
  mutate(
    Month = month(
      Date,
      label = TRUE,
      abbr = TRUE
    )
  ) %>%
  group_by(Month) %>%
  summarise(
    Average_Ridership = mean(
      Count,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


cat("\n============================================\n")
cat("AVERAGE SUBWAY RIDERSHIP BY MONTH\n")
cat("============================================\n")

print(subway_month)


write.csv(
  subway_month,
  "output/subway_month_summary.csv",
  row.names = FALSE
)


# ============================================================
# STEP 29A — SUBWAY RIDERSHIP BY QUARTER AND SEASON (NEW)
# ============================================================

subway_quarter <- subway_daily %>%
  group_by(Quarter) %>%
  summarise(
    Average_Ridership = mean(
      Count,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


subway_season <- subway_daily %>%
  group_by(Season) %>%
  summarise(
    Average_Ridership = mean(
      Count,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(
    desc(Average_Ridership)
  )


cat("\n============================================\n")
cat("AVERAGE SUBWAY RIDERSHIP BY QUARTER\n")
cat("============================================\n")

print(subway_quarter)


cat("\n============================================\n")
cat("AVERAGE SUBWAY RIDERSHIP BY SEASON\n")
cat("============================================\n")

print(subway_season)


write.csv(
  subway_quarter,
  "output/subway_quarter_summary.csv",
  row.names = FALSE
)

write.csv(
  subway_season,
  "output/subway_season_summary.csv",
  row.names = FALSE
)


# ============================================================
# STEP 30 — MONTH GRAPH
# ============================================================

plot_month <- ggplot(
  subway_month,
  aes(
    x = Month,
    y = Average_Ridership
  )
) +
  geom_col() +
  labs(
    title = "Average Subway Ridership by Month",
    x = "Month",
    y = "Average Subway Ridership"
  ) +
  theme_minimal()

print(plot_month)

ggsave(
  "figures/subway_ridership_by_month.png",
  plot = plot_month,
  width = 10,
  height = 6
)


# ============================================================
# STEP 31 — CREATE DATA FOR PREDICTION
# ============================================================
#
# We use:
#
# Subway = prediction target
# Bus    = related public transport demand
# BT     = bridge/tunnel road traffic
#
# ============================================================

prediction_data <- clean_data %>%
  filter(
    Mode %in% c(
      "Subway",
      "Bus",
      "BT"
    )
  ) %>%
  group_by(
    Date,
    Mode
  ) %>%
  summarise(
    Count = sum(
      Count,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Mode,
    values_from = Count
  ) %>%
  arrange(
    Date
  )


# ============================================================
# STEP 32 — CHECK PREDICTION DATA
# ============================================================

cat("\n============================================\n")
cat("PREDICTION DATA DIMENSIONS\n")
cat("============================================\n")

print(
  dim(prediction_data)
)


cat("\n============================================\n")
cat("PREDICTION DATA PREVIEW\n")
cat("============================================\n")

print(
  head(prediction_data)
)


# ============================================================
# STEP 33 — CREATE LAGGED SUBWAY VARIABLES
# ============================================================
#
# New in this version:
# Previous_7Day_Avg  = average of the 7 days BEFORE the
#                      prediction day (no data leakage)
# Subway_Last_Week   = subway ridership 7 days earlier
# Is_Holiday         = US federal holiday indicator
# COVID_Phase        = pandemic period
#
# ============================================================

prediction_data <- prediction_data %>%
  arrange(
    Date
  ) %>%
  mutate(
    Previous_Subway = lag(Subway),
    Previous_7Day_Avg = lag(
      rolling_mean_7(Subway)
    ),
    Subway_Last_Week = lag(Subway, 7),
    Day = weekdays(Date),
    Month = month(Date),
    Year = year(Date)
  ) %>%
  left_join(
    holiday_table,
    by = "Date"
  ) %>%
  mutate(
    Is_Holiday = ifelse(
      is.na(Holiday_Name),
      0,
      1
    ),
    Is_Weekend = ifelse(
      wday(Date) %in% c(1, 7),
      1,
      0
    ),
    COVID_Phase = case_when(
      Date < as.Date("2020-07-01") ~ "1. Early COVID (to Jun 2020)",
      Date < as.Date("2022-01-01") ~ "2. Recovery (Jul 2020 - Dec 2021)",
      TRUE ~ "3. Post-Recovery (2022 onwards)"
    )
  ) %>%
  select(
    -Holiday_Name
  )


# ============================================================
# STEP 34 — REMOVE ROWS WITH MISSING PREDICTORS
# ============================================================

prediction_data <- prediction_data %>%
  filter(
    !is.na(Subway),
    !is.na(Bus),
    !is.na(BT),
    !is.na(Previous_Subway),
    !is.na(Previous_7Day_Avg),
    !is.na(Subway_Last_Week)
  )


# ============================================================
# STEP 35 — CONVERT DAY AND COVID PHASE TO FACTORS
# ============================================================

prediction_data$Day <- factor(
  prediction_data$Day,
  levels = c(
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday",
    "Sunday"
  )
)

prediction_data$COVID_Phase <- factor(
  prediction_data$COVID_Phase
)


# ============================================================
# STEP 36 — CHECK FINAL PREDICTION DATA
# ============================================================

cat("\n============================================\n")
cat("FINAL PREDICTION DATA\n")
cat("============================================\n")

print(
  dim(prediction_data)
)

print(
  head(prediction_data)
)


# ============================================================
# STEP 37 — CHRONOLOGICAL TRAIN/TEST SPLIT
# ============================================================
#
# IMPORTANT:
# We do NOT randomly split time-series data.
#
# First 80%  = training data
# Last 20%   = testing data
#
# This prevents future observations from being used
# to train the model.
#
# ============================================================

total_rows <- nrow(
  prediction_data
)

train_size <- floor(
  0.80 * total_rows
)


train_prediction <- prediction_data[
  1:train_size,
]


test_prediction <- prediction_data[
  (train_size + 1):total_rows,
]


cat("\n============================================\n")
cat("TRAIN / TEST SPLIT\n")
cat("============================================\n")

cat(
  "Total observations:",
  total_rows,
  "\n"
)

cat(
  "Training observations:",
  nrow(train_prediction),
  "\n"
)

cat(
  "Testing observations:",
  nrow(test_prediction),
  "\n"
)

# ==========================================
# STAGE 4: LINEAR REGRESSION MODEL
# ==========================================
# Select features for predicting subway ridership.
# Split the data chronologically into training
# and testing datasets.
# Train the linear regression model using lm().
# Generate predictions using predict().
# ============================================================
# STEP 38 — BUILD LINEAR REGRESSION MODEL
# ============================================================

subway_model <- lm(
  Subway ~
    Previous_Subway +
    Previous_7Day_Avg +
    Subway_Last_Week +
    Bus +
    BT +
    Day +
    Month +
    Is_Holiday +
    COVID_Phase,
  data = train_prediction
)


# ============================================================
# STEP 39 — DISPLAY MODEL SUMMARY
# ============================================================

cat("\n============================================\n")
cat("SUBWAY RIDERSHIP MODEL SUMMARY\n")
cat("============================================\n")

print(
  summary(subway_model)
)


# ============================================================
# STEP 40 — PREDICT TEST DATA
# ============================================================

test_prediction$Predicted_Subway <- predict(
  subway_model,
  newdata = test_prediction
)


# ============================================================
# STEP 41 — CREATE RESULTS TABLE
# ============================================================

subway_results <- test_prediction %>%
  select(
    Date,
    Day,
    Is_Holiday,
    Subway,
    Predicted_Subway
  ) %>%
  rename(
    Actual = Subway,
    Predicted = Predicted_Subway
  )


# ============================================================
# STEP 42 — CALCULATE ABSOLUTE ERROR
# ============================================================

subway_results <- subway_results %>%
  mutate(
    Error = Actual - Predicted,
    Absolute_Error = abs(Error),
    Percentage_Error = round(
      100 * Absolute_Error / Actual,
      2
    )
  )


# ============================================================
# STEP 43 — CALCULATE MAE
# ============================================================

MAE <- mean(
  subway_results$Absolute_Error
)


# ============================================================
# STEP 44 — CALCULATE RMSE
# ============================================================

RMSE <- sqrt(
  mean(
    subway_results$Error^2
  )
)


# ============================================================
# STEP 45 — CALCULATE STANDARD PREDICTIVE R-SQUARED
# ============================================================

SSE <- sum(
  subway_results$Error^2
)

SST <- sum(
  (
    subway_results$Actual -
      mean(subway_results$Actual)
  )^2
)

R_squared <- 1 - (
  SSE / SST
)


# ============================================================
# STEP 45A — CALCULATE MAPE (NEW)
# ============================================================

MAPE <- mean(
  subway_results$Percentage_Error,
  na.rm = TRUE
)


# ============================================================
# STEP 46 — FINAL MODEL METRICS
# ============================================================

final_model_metrics <- data.frame(
  Metric = c(
    "MAE",
    "RMSE",
    "MAPE (%)",
    "R-squared"
  ),
  Value = c(
    MAE,
    RMSE,
    MAPE,
    R_squared
  )
)


cat("\n============================================\n")
cat("FINAL TEST MODEL PERFORMANCE\n")
cat("============================================\n")

print(
  final_model_metrics
)


cat(
  "\nMAE =",
  round(MAE, 2),
  "\n"
)

cat(
  "RMSE =",
  round(RMSE, 2),
  "\n"
)

cat(
  "MAPE =",
  round(MAPE, 2),
  "%\n"
)

cat(
  "R-squared =",
  round(R_squared, 4),
  "\n"
)


# ============================================================
# STEP 47 — SAVE FINAL MODEL METRICS
# ============================================================

write.csv(
  final_model_metrics,
  "output/final_model_metrics.csv",
  row.names = FALSE
)


# ============================================================
# STEP 48 — MODEL COEFFICIENTS
# ============================================================

model_coefficients <- as.data.frame(
  summary(subway_model)$coefficients
)

model_coefficients$Variable <- rownames(
  model_coefficients
)

rownames(
  model_coefficients
) <- NULL


names(model_coefficients) <- c(
  "Estimate",
  "Std_Error",
  "t_value",
  "p_value",
  "Variable"
)


model_coefficients <- model_coefficients[
  ,
  c(
    "Variable",
    "Estimate",
    "Std_Error",
    "t_value",
    "p_value"
  )
]


cat("\n============================================\n")
cat("MODEL COEFFICIENTS\n")
cat("============================================\n")

print(
  model_coefficients
)


# ============================================================
# STEP 49 — SIGNIFICANT VARIABLES
# ============================================================

significant_variables <- model_coefficients %>%
  filter(
    p_value < 0.05
  )


cat("\n============================================\n")
cat("STATISTICALLY SIGNIFICANT VARIABLES\n")
cat("p < 0.05\n")
cat("============================================\n")

print(
  significant_variables
)


write.csv(
  model_coefficients,
  "output/final_model_coefficients.csv",
  row.names = FALSE
)


# ============================================================
# STEP 50 — ACTUAL VS PREDICTED GRAPH
# ============================================================

plot_actual_predicted <- ggplot(
  subway_results,
  aes(
    x = Actual,
    y = Predicted
  )
) +
  geom_point() +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Actual vs Predicted Subway Ridership",
    x = "Actual Subway Ridership",
    y = "Predicted Subway Ridership"
  ) +
  theme_minimal()

print(
  plot_actual_predicted
)


ggsave(
  "figures/actual_vs_predicted_subway.png",
  plot = plot_actual_predicted,
  width = 10,
  height = 6
)


# ============================================================
# STEP 51 — ACTUAL VS PREDICTED OVER TIME
# ============================================================

plot_time_prediction <- ggplot(
  subway_results,
  aes(
    x = Date
  )
) +
  geom_line(
    aes(
      y = Actual,
      linetype = "Actual"
    )
  ) +
  geom_line(
    aes(
      y = Predicted,
      linetype = "Predicted"
    )
  ) +
  labs(
    title = "Actual vs Predicted Subway Ridership Over Time",
    x = "Date",
    y = "Subway Ridership",
    linetype = "Series"
  ) +
  theme_minimal()


print(
  plot_time_prediction
)


ggsave(
  "figures/actual_predicted_over_time.png",
  plot = plot_time_prediction,
  width = 12,
  height = 6
)


# ============================================================
# STEP 52 — PREDICTION ERROR GRAPH
# ============================================================

plot_errors <- ggplot(
  subway_results,
  aes(
    x = Predicted,
    y = Error
  )
) +
  geom_point() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Subway Ridership Prediction Error",
    x = "Predicted Subway Ridership",
    y = "Prediction Error"
  ) +
  theme_minimal()


print(
  plot_errors
)


ggsave(
  "figures/subway_prediction_errors.png",
  plot = plot_errors,
  width = 10,
  height = 6
)


# ============================================================
# STEP 52A — PREDICTION ERROR BY DAY AND HOLIDAY (NEW)
# ============================================================

error_by_day <- subway_results %>%
  group_by(Day) %>%
  summarise(
    MAE = mean(Absolute_Error, na.rm = TRUE),
    MAPE = mean(Percentage_Error, na.rm = TRUE),
    .groups = "drop"
  )


error_by_holiday <- subway_results %>%
  mutate(
    Holiday_Status = ifelse(
      Is_Holiday == 1,
      "Holiday",
      "Regular Day"
    )
  ) %>%
  group_by(Holiday_Status) %>%
  summarise(
    Days = n(),
    MAE = mean(Absolute_Error, na.rm = TRUE),
    MAPE = mean(Percentage_Error, na.rm = TRUE),
    .groups = "drop"
  )


cat("\n============================================\n")
cat("PREDICTION ERROR BY DAY OF WEEK\n")
cat("============================================\n")

print(error_by_day)


cat("\n============================================\n")
cat("PREDICTION ERROR: HOLIDAYS VS REGULAR DAYS\n")
cat("============================================\n")

print(error_by_holiday)


write.csv(
  error_by_day,
  "output/prediction_error_by_day.csv",
  row.names = FALSE
)

write.csv(
  error_by_holiday,
  "output/prediction_error_by_holiday.csv",
  row.names = FALSE
)


# ============================================================
# STEP 53 — ACTUAL/PREDICTED RESULT SAMPLE
# ============================================================

cat("\n============================================\n")
cat("FIRST 10 TEST PREDICTIONS\n")
cat("============================================\n")

print(
  head(
    subway_results,
    10
  )
)


# ============================================================
# STEP 54 — SAVE ALL PREDICTIONS
# ============================================================

write.csv(
  subway_results,
  "output/final_subway_predictions.csv",
  row.names = FALSE
)


# ============================================================
# STEP 55 — MODEL PERFORMANCE INTERPRETATION
# ============================================================

cat("\n============================================\n")
cat("MODEL INTERPRETATION\n")
cat("============================================\n")

cat(
  "The model predicts daily Subway ridership using:\n"
)

cat(
  "1. Previous-day Subway ridership\n"
)

cat(
  "2. Previous 7-day average Subway ridership\n"
)

cat(
  "3. Subway ridership on the same day last week\n"
)

cat(
  "4. Bus ridership\n"
)

cat(
  "5. BT traffic count\n"
)

cat(
  "6. Day of week\n"
)

cat(
  "7. Month\n"
)

cat(
  "8. Holiday indicator\n"
)

cat(
  "9. COVID phase\n\n"
)


cat(
  "The model was trained on the first 80% of the\n",
  "chronologically ordered observations and tested\n",
  "on the remaining 20%.\n\n"
)


cat(
  "Test MAE:",
  round(MAE, 2),
  "\n"
)

cat(
  "Test RMSE:",
  round(RMSE, 2),
  "\n"
)

cat(
  "Test MAPE:",
  round(MAPE, 2),
  "%\n"
)

cat(
  "Test R-squared:",
  round(R_squared, 4),
  "\n\n"
)


if (R_squared >= 0.80) {
  
  cat(
    "Interpretation: The model shows strong predictive\n",
    "performance on the unseen test period.\n"
  )
  
} else if (R_squared >= 0.60) {
  
  cat(
    "Interpretation: The model shows moderate predictive\n",
    "performance on the unseen test period.\n"
  )
  
} else {
  
  cat(
    "Interpretation: The model requires further improvement.\n"
  )
  
}


# ============================================================
# STEP 56 — FIND HIGHEST RIDERSHIP DAYS
# ============================================================

highest_subway_days <- subway_daily %>%
  arrange(
    desc(Count)
  ) %>%
  select(
    Date,
    Count
  ) %>%
  head(10)


cat("\n============================================\n")
cat("TOP 10 HIGHEST SUBWAY RIDERSHIP DAYS\n")
cat("============================================\n")

print(
  highest_subway_days
)


write.csv(
  highest_subway_days,
  "output/top_10_subway_ridership_days.csv",
  row.names = FALSE
)


# ============================================================
# STEP 57 — FIND LOWEST RIDERSHIP DAYS
# ============================================================

lowest_subway_days <- subway_daily %>%
  arrange(
    Count
  ) %>%
  select(
    Date,
    Count
  ) %>%
  head(10)


cat("\n============================================\n")
cat("TOP 10 LOWEST SUBWAY RIDERSHIP DAYS\n")
cat("============================================\n")

print(
  lowest_subway_days
)


write.csv(
  lowest_subway_days,
  "output/top_10_lowest_subway_ridership_days.csv",
  row.names = FALSE
)


# ============================================================
# STEP 58 — OVERALL PROJECT SUMMARY
# ============================================================

cat("\n\n")
cat("============================================================\n")
cat(" URBAN TRAFFIC AND PUBLIC TRANSPORT INTELLIGENT SYSTEM\n")
cat(" COMPLETE ANALYSIS FINISHED\n")
cat("============================================================\n\n")


cat(
  "Original dataset rows:",
  nrow(traffic_data),
  "\n"
)

cat(
  "Cleaned dataset rows:",
  nrow(clean_data),
  "\n"
)

cat(
  "Columns after feature engineering:",
  ncol(clean_data),
  "\n"
)

cat(
  "Number of transportation modes:",
  length(
    unique(clean_data$Mode)
  ),
  "\n"
)

cat(
  "Prediction dataset rows:",
  nrow(prediction_data),
  "\n"
)

cat(
  "Training observations:",
  nrow(train_prediction),
  "\n"
)

cat(
  "Testing observations:",
  nrow(test_prediction),
  "\n\n"
)


cat(
  "FINAL PREDICTION MODEL:\n"
)

cat(
  "Subway ~ Previous_Subway + Previous_7Day_Avg + Subway_Last_Week +\n",
  "         Bus + BT + Day + Month + Is_Holiday + COVID_Phase\n\n"
)


cat(
  "FINAL TEST RESULTS:\n"
)

cat(
  "MAE =",
  round(MAE, 2),
  "\n"
)

cat(
  "RMSE =",
  round(RMSE, 2),
  "\n"
)

cat(
  "MAPE =",
  round(MAPE, 2),
  "%\n"
)

cat(
  "R-squared =",
  round(R_squared, 4),
  "\n\n"
)


cat(
  "Output files have been saved in the 'output' folder.\n"
)

cat(
  "Graphs have been saved in the 'figures' folder.\n"
)

cat(
  "Cleaned datasets have been saved in the 'data' folder.\n\n"
)


cat("============================================================\n")
cat(" END OF ANALYSIS\n")
cat("============================================================\n")



