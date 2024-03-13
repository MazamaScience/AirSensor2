# 01_AirMonitor_recipes.R
#
# This script contains small chunks of R code (aka "recipes") that demonstrate
# how to work with the AirMonitor package to access, manipulate and display
# regulatory monitoring data from the USFS maintained archives of pre-processed
# AirNow monitoring data.
#
# This script assumes that you have already installed the following packages:
#  - AirMonitor
#  - AirMonitorPlots

# Check that the AirMonitor package is recent enough
if ( packageVersion("AirMonitor") < "0.4.0" ) {
  stop("VERSION_ERROR:  Please upgrade to AirMonitor 0.4.0 or later.")
}

# Check that the working directory is set properly
if ( !stringr::str_detect(getwd(), "R_workshop$") ) {
  stop("WD_ERROR:  Please set the working directory to 'ASIC-2024/R_workshop/'")
}

# Open reference docs in a web browser
browseURL("http://mazamascience.com/presentations/2022/ASIC_Universal_Data_Structures.pdf")
browseURL("https://mazamascience.github.io/AirMonitor/reference/index.html")

library(AirMonitor)

# ----- Examine 'mts_monitor' object -------------------------------------------

# Recipe to load most recent 10 days of hourly PM2.5 data

# To make a "monitor" object:
monitor <-
  # Step 1) load data
  airnow_loadLatest()

# This is a compact data format. The Environment tab shows it at ~5 MB

# This object is an R list
class(monitor)

# This list contains 2 dataframes: 'meta' and 'data'
names(monitor)

# 'meta' contains N metadata records (device-deployments) with 54 fields
dim(monitor$meta)

View(monitor$meta)

# 'data' contains hourly records for N device-deployments + the 'datetime' field
dim(monitor$data)

# NOTE:  Remember that dplyr shows column names and values. The familiar
# row-column structure of 'data' is seen with head():
head(monitor$data[1:10, 1:4])

# 'meta' rows match 'data' columns
all(monitor$meta$deviceDeploymentID == names(monitor$data[,-1])) # drop 'datetime'

# IMPORTANT:  Compact data format relies on separating data and metadata

# ----- Maps and filtering -----------------------------------------------------

# All sites
# start with the monitor object
monitor %>%
  # Step 1) create a leaflet map
  monitor_leaflet()

# NOTE:  Useful spatial metadata include countryCode, stateCode, countyName

# CONUS
# start with the monitor object
monitor %>%
  # Step 1) filter by state
  monitor_filter(stateCode %in% CONUS) %>%
  # Step 2) create a leaflet map
  monitor_leaflet()

# California
monitor %>%
  monitor_filter(stateCode == "CA") %>%
  monitor_leaflet()

# Which counties have monitors?
monitor %>%
  monitor_filter(stateCode == "CA") %>%          # filter by state
  monitor_pull("countyName") %>%                 # get meta$countyName
  table() %>%
  sort(decreasing = TRUE)

# Riverside County
monitor %>%
  monitor_filter(stateCode == "CA") %>%
  monitor_filter(countyName == "Riverside") %>%
  monitor_leaflet()

# ----- Time series ------------------------------------------------------------

# All monitors in Riverside County

# To make the "riverside" object
riverside <-
  # Step 1) start with the "monitor" object
  monitor %>%
  # Step 2) filter by state
  monitor_filter(stateCode == "CA") %>%
  # Step 3) filter by county
  monitor_filter(countyName == "Riverside")

nrow(riverside$meta)

ncol(riverside$data)

# Riverside time series plot
riverside %>%
  monitor_timeseriesPlot()

# Timeseries plot with extras
riverside %>%
  monitor_timeseriesPlot(
    shadedNight = TRUE,
    addAQI = TRUE
  )

# Select a single monitor
riverside %>%
  monitor_leaflet()

# Click on map to get the deviceDeploymentID

# Single monitor
rubidoux <-
  riverside %>%
  monitor_select("c7cc2b21d9f11f15_840060658001")

nrow(rubidoux$meta)
ncol(rubidoux$data)

head(rubidoux$data)

# ----- Fancy timeseries plot --------------------------------------------------

rubidoux %>%
  monitor_timeseriesPlot(
    shadedNight = TRUE,
    addAQI = TRUE,
    pch = 16,
    col = 'gray70',
    main = "Air Quality at Rubidoux"
  )

# Add NowCast
rubidoux %>%
  monitor_nowcast(
    includeShortTerm = TRUE
  ) %>%
  monitor_timeseriesPlot(
    add = TRUE,
    type = 'l',
    lwd = 2,
    col = "gray10"
  )

# Add a legend
legend(
  "topright",
  legend = c("Hourly PM2.5", "NowCast"),
  col = c("gray70", "gray10"),
  pch = c(16, NA),
  lwd = c(NA, 2)
)

# ----- Updated PM NAAQS -------------------------------------------------------

rubidoux %>%
  monitor_timeseriesPlot(
    shadedNight = TRUE,
    addAQI = TRUE,
    pch = 16,
    col = 'gray70',
    main = "Air Quality at Rubidoux with updated NAAQS",
    NAAQS = "PM2.5_2024"
  )

# Add NowCast
rubidoux %>%
  monitor_nowcast(
    includeShortTerm = TRUE
  ) %>%
  monitor_timeseriesPlot(
    add = TRUE,
    type = 'l',
    lwd = 2,
    col = "gray10"
  )

# Add a legend
legend(
  "topright",
  legend = c("Hourly PM2.5", "NowCast"),
  col = c("gray70", "gray10"),
  pch = c(16, NA),
  lwd = c(NA, 2)
)

# ----- Annual data ------------------------------------------------------------

# Load all of 2020 for all monitors
monitor <-
  airnow_loadAnnual(2020)

nrow(monitor$meta)

# NOTE:  ~160 MB for an entire year!

monitor %>%
  monitor_timeRange()

# Map
monitor %>%
  monitor_leaflet()

# Washington state during fire season
wa <-
  monitor %>%
  monitor_filter(stateCode == "WA") %>%
  monitor_filterDate("2020-07-01", "2020-11-01")

nrow(wa$meta)

# Timeseries
wa %>%
  monitor_timeseriesPlot()

# Narrow down to mid September

# Mid-September
wa <-
  wa %>%
  monitor_filterDate(
    startdate = "2020-09-07",
    enddate = "2020-09-21",
    timezone = "America/Los_Angeles"
  )

wa %>%
  monitor_timeseriesPlot(
    shadedNight = TRUE,
    addAQI = TRUE
  )
addAQILegend()


# ----- Advanced recipes -------------------------------------------------------

# Where was HAZARDOUS encountered?

# Check US_AQI object
names(US_AQI)

US_AQI$names_eng
US_AQI$names_spa
US_AQI$breaks_PM2.5
US_AQI$breaks_PM2.5_2024

# Hazardous starts at 250.5 ug/m3
threshold <- US_AQI$breaks_PM2.5[6]

# Map of locations that experienced HAZARDOUS
wa %>%
  monitor_selectWhere(
    function(x) { any(x >= threshold, na.rm = TRUE) }
  ) %>%
  monitor_leaflet()

# THE ENTIRE STATE!

# Let's look at Omak

omak <-
  wa %>%
  monitor_select("e5d75388f0cfcbf6_530470013")

# Plot time series for Omak
omak %>%
  monitor_timeseriesPlot(
    shadedNight = TRUE,
    addAQI = TRUE,
    type = 'b'
  )
addAQILegend(
  "topright",
  title = "PM2.5 (\u00b5g/m\u00b3)"              # 00b5 = micro, 00b3 = cubed
)

# Hours in each category
omak %>%
  monitor_toAQCTable()

# Days in each category in Chelan & Okanogan counties
wa %>%
  monitor_filter(countyName %in% c("Chelan", "Okanogan")) %>%
  monitor_dailyStatistic(mean) %>%
  monitor_toAQCTable()

# Print daily max for Omak
omak %>%
  monitor_dailyStatistic(
    FUN = max,
    minHours = 18,
    dayBoundary = "LST"
  ) %>%
  monitor_getData() %>%
  print()

# Plot daily averages for Omak
omak %>%
  monitor_dailyBarplot(
    minHours = 18,
    dayBoundary = "LST"
  )
addAQILegend("topright")

# Plot Washington state daily averages
wa %>%
  monitor_collapse(
    FUN = mean
  ) %>%
  monitor_dailyBarplot(
    main = "Washington State daily average PM2.5"
  )
addAQILegend("topright")


