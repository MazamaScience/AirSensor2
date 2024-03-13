# 03_AirSensor2_PAT recipes.R
#
# This script contains small chunks of R code (aka "recipes") that demonstrate
# how to work with the AirSensor2 package to access, manipulate and display
# sensor data available through the PurpleAir API.
#
# This script assumes that you have already installed the following packages:
#  - AirMonitor
#  - AirSensor2

# Check that the AirMonitor package is recent enough
if ( packageVersion("AirMonitor") < "0.4.0" ) {
  stop("VERSION_ERROR:  Please upgrade to AirMonitor 0.4.0 or later.")
}

# Check that the Sensor2 package is recent enough
if ( packageVersion("AirSensor2") < "0.5.0" ) {
  stop("VERSION_ERROR:  Please upgrade to AirSensor2 0.5.0 or later.")
}

# Check that the MazamaSpatialUtils package is recent enough
if ( packageVersion("MazamaSpatialUtils") < "0.8.6" ) {
  browseURL("https://github.com/MazamaScience/ASIC-2024/tree/main/R_workshop#spatial-data")
  stop("VERSION_ERROR:  Please upgrade to MazamaSpatialUtils 0.8.6 or later.")
}

# Check that the working directory is set properly
if ( !stringr::str_detect(getwd(), "R_workshop$") ) {
  stop("WD_ERROR:  Please set the working directory to 'R_workshop/'")
}

# Open reference docs in a web browser
browseURL("https://api.purpleair.com/#api-sensors-get-sensor-history-csv")
browseURL("https://mazamascience.github.io/AirSensor2/reference/index.html")

library(AirMonitor)
library(AirSensor2)

# Set up spatial data from default directories
initializeMazamaSpatialUtils()

# ----- API Keys ---------------------------------------------------------------

# Read in secret PurpleAir_API_READ_KEY
source("global_vars.R")

# Check the key
PurpleAir_checkAPIKey(PurpleAir_API_READ_KEY)

# ----- PurpleAir Synoptic (PAT) -----------------------------------------------

# PAS Metadata only field names
PurpleAir_PAS_METADATA_FIELDS  %>%
  stringr::str_split(",") %>%
  print(width = 75)

# PAT field names for EPA correction
PurpleAir_PAT_EPA_HOURLY_FIELDS  %>%
  stringr::str_split(",") %>%
  print(width = 75)

# PAT field names for detailed QC
PurpleAir_PAT_QC_FIELDS  %>%
  stringr::str_split(",") %>%
  print(width = 75)

# ----- Riverside Hourly PAS ---------------------------------------------------

# Add "confidence" to fields
my_fields <-
  PurpleAir_PAS_METADATA_FIELDS %>%
  stringr::str_split_1(",") %>%
  union("confidence") %>%
  paste0(collapse = ",")

pas <-
  pas_createNew(
    api_key = PurpleAir_API_READ_KEY,
    fields = my_fields,
    countryCodes = "US",
    stateCodes = "CA",
    counties = "Riverside",
    lookbackDays = 1,            # currently working
    location_type = 0            # outdoor only
  )

# How many sensors?
pas %>%
  nrow()

# How many very confident sensors?
pas %>%
  dplyr::filter(confidence == 100) %>%
  nrow()

# Update pas to only use sensors we are confident in
pas <-
  pas %>%
  dplyr::filter(confidence == 100)

# Basic map
pas %>%
  pas_leaflet()

# ----- Riverside Hourly PAT ---------------------------------------------------

# This sensor has beeen up since 2017-09-30!

RIVR_coloc_9_hourly <-
  pat_createHourly(
    api_key = PurpleAir_API_READ_KEY,
    pas = pas,
    sensor_index = 3537,
    startdate = "2024-03-01",
    enddate = "2024-03-09",
    timezone = "America/Los_Angeles",
    fields = PurpleAir_PAT_EPA_HOURLY_FIELDS
  )

# Make a copy and leave original in memory (R is pass-by-copy, not pass-by-reference)
pat <- RIVR_coloc_9_hourly

# What is this thing?
class(pat)
names(pat)
nrow(pat$meta)
names(pat$meta) %>% print(width = 75)
nrow(pat$data)
names(pat$data) %>% print(width = 75)

# Regular time axis
plot(pat$data$datetime, seq_len(nrow(pat$data)))

# Quick review
plot(pat$data)

# ----- Riverside 'monitor' ----------------------------------------------------

monitor <-
  pat %>%
  pat_toMonitor()

class(monitor)

# Plot from 01_AirMonitor_recipes.R

# Hourly dots
monitor %>%
  monitor_timeseriesPlot(
    shadedNight = TRUE,
    addAQI = TRUE,
    NAAQS = "PM2.5_2024",
    pch = 16,
    col = 'gray70',
    main = "RIVR_Co-loc9 Hourly PM2.5 (with EPA correction)"
  )

# Add NowCast
monitor %>%
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

# NOTE:  Negative Values!!! The correction algorithm is not perfect!

# NOTE:  In the words of Spiderman: "With great power comes great responsibility."

# ----- Crestbrool 'monitor' ---------------------------------------------------

# This sensor has beeen up since 2020-10-29

Crestbrool_hourly <-
  pat_createHourly(
    api_key = PurpleAir_API_READ_KEY,
    pas = pas,
    sensor_index = 86879,
    startdate = "2024-03-01",
    enddate = "2024-03-09",
    timezone = "America/Los_Angeles",
    fields = PurpleAir_PAT_EPA_HOURLY_FIELDS
  )

# Make a copy and leave original in memory (R is pass-by-copy, not pass-by-reference)
pat <- Crestbrool_hourly

plot(pat$data)

# Plot from 01_AirMonitor_recipes.R

monitor <-
  pat %>%
  pat_toMonitor()

# Hourly dots
monitor %>%
  monitor_timeseriesPlot(
    shadedNight = TRUE,
    addAQI = TRUE,
    NAAQS = "PM2.5_2024",
    pch = 16,
    col = 'gray70',
    main = "Crestbrool Hourly PM2.5 (with EPA correction)"
  )

# Add NowCast
monitor %>%
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

# ----- Riverside Raw QC -------------------------------------------------------

# Let's take a closer look at the low level data

sensor_index <- "3537"

RIVR_coloc_9_raw <-
  pat_createRaw(
    api_key = PurpleAir_API_READ_KEY,
    pas = pas,
    sensor_index = sensor_index,
    startdate = "2024-03-01",
    enddate = "2024-03-03",
    timezone = "America/Los_Angeles"
  )

# Make a copy and leave original in memory (R is pass-by-copy, not pass-by-reference)
pat <- RIVR_coloc_9_raw

# * Run RMarkdown report -----
params <-
  list(
    pat = pat
  )

# This path is relative to the Rmd/ directory
htmlPath <- sprintf("pat-qc-%s-20240301-20240303.html", sensor_index)

rmarkdown::render(
  input = 'Rmd/pat-qc-report.Rmd',
  params = params,
  output_file = htmlPath
)

browseURL(file.path("Rmd", htmlPath))


# ----- Crestbrool Raw QC ------------------------------------------------------

# Let's take a closer look at the low level data

sensor_index <- "86879"

Crestbrool_raw <-
  pat_createRaw(
    api_key = PurpleAir_API_READ_KEY,
    pas = pas,
    sensor_index = sensor_index,
    startdate = "2024-03-01",
    enddate = "2024-03-03",
    timezone = "America/Los_Angeles"
  )

# Make a copy and leave original in memory (R is pass-by-copy, not pass-by-reference)
pat <- Crestbrool_raw

# * Run RMarkdown report -----
params <-
  list(
    pat = pat
  )

# This path is relative to the Rmd/ directory
htmlPath <- sprintf("pat-qc-%s-20240301-20240303.html", sensor_index)

rmarkdown::render(
  input = 'Rmd/pat-qc-report.Rmd',
  params = params,
  output_file = htmlPath
)

browseURL(file.path("Rmd", htmlPath))


