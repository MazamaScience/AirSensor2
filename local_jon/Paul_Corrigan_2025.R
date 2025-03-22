# Testing access to private sensor data

# Near McCall, Idaho
# https://map.purpleair.com/air-quality-standards-us-epa-aqi?key=JPNU09ZW86YYM0UZ&opt=%2F1%2Flp%2Fa10%2Fp604800%2FcC0&select=218289#11.09/44.9798/-116.2799

# Setup

library(AirSensor2)

initializeMazamaSpatialUtils()

source("global_vars_Corrigan.R") # contains PurpleAir_API_READ_KEY and CORRIGAN_API_KEYS

# Create PurpleAirSynoptic object
pas <-
  pas_createNew(
    api_key = PurpleAir_API_READ_KEY,
    countryCodes = "US",
    stateCodes = "ID",
    lookbackDays = 0, # all historical sensors
    location_type = 0,
    read_keys = CORRIGAN_READ_KEYS
  )

# Get the sensor_index for "camas"
pas %>% pas_filter(name == "camas") %>% pas_leaflet()

# Click on dot to get
sensor_index <- "218289"

# Hourly data with fields for EPA QC
pat_hourly_avg <-
  pat_createHourly(
    api_key <- PurpleAir_API_READ_KEY,
    pas = pas,
    sensor_index = sensor_index,
    read_keys = CORRIGAN_READ_KEYS,
    fields = PurpleAir_PAT_EPA_HOURLY_FIELDS,
    startdate = "2024-07-01",
    enddate = "2024-08-01",
    timezone = "UTC",
    verbose = TRUE
  )

# Raw data with fields for EPA QC
pat_raw_avg <-
  pat_createRaw(
    api_key <- PurpleAir_API_READ_KEY,
    pas = pas,
    sensor_index = sensor_index,
    read_keys = CORRIGAN_READ_KEYS,
    fields = PurpleAir_PAT_QC_FIELDS,
    startdate = "2024-07-01",
    enddate = "2024-07-03",
    timezone = "UTC",
    verbose = TRUE
  )

