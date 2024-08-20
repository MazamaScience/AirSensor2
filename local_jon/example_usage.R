# Example usage of the "pat" files

# Check that the Sensor2 package is recent enough
if ( packageVersion("AirSensor2") < "0.5.1" ) {
  stop("VERSION_ERROR:  Please upgrade to AirSensor2 0.5.1 or later.")
}

library(AirSensor2)

# This PurlpleAirSynoptic object contains all sensors that ever producced data
pas <- get(load("./data/chile_pas.rda"))

# When did sensors produce data
pas %>%
  pas_lifespanPlot(main = "Sensor Reporting Lifespans in Chile")

# To get a 'pas' object for sensors that produced data in 2023 we need to filter

timezone <- "America/Santiago"
start <- MazamaCoreUtils::parseDatetime("2023-01-01 00:00", timezone = timezone)
end <- MazamaCoreUtils::parseDatetime("2024-01-01 00:00", timezone = timezone)

pas_2023 <-
  pas %>%
  pas_filter(date_created < end) %>%
  pas_filter(last_seen > start)

# Identify sensors that produced data in 2023
pas_2023 %>%
  pas_lifespanPlot(
    showSensor = TRUE,
    sensorIdentifier = "sensor_index",
    cex = 0.8,
    lwd = 2,
    moreSpace = 0.1,
    main = "Sensors Reporting in Chile in 2023"
  )

# Where are thes monitors?
pas_2023 %>% pas_leaflet()

# Sensor 158193 produced data throughout the year
pat <- get(load("./data/pat_158193.rda"))

# Learn about this object
class(pat)
names(pat)
dim(pat$meta)
dim(pat$data)
dplyr::glimpse(pat$meta)
dplyr::glimpse(pat$data)

# No plotting is provided in the AirSensor2 package so we use base R plotting
plot(
  pat$data$datetime, pat$data$humidity,
  pch = 15,
  col = adjustcolor("blue", 0.4) # semi-transparent
)

# Months of dropouts (either from the sensor or from the PurpleAir API)
# Let's try another
pat <- get(load("./data/pat_158177.rda"))
plot(
  pat$data$datetime, pat$data$humidity,
  pch = 15,
  col = adjustcolor("blue", 0.4) # semi-transparent
)

# Different months of dropouts
# Try again
pat <- get(load("./data/pat_47259.rda"))
plot(
  pat$data$datetime, pat$data$humidity,
  pch = 15,
  col = adjustcolor("blue", 0.4) # semi-transparent
)

# Different months of dropouts
# Try again
pat <- get(load("./data/pat_132041.rda"))
plot(
  pat$data$datetime, pat$data$humidity,
  pch = 15,
  col = adjustcolor("blue", 0.4) # semi-transparent
)

# Finally! A complete year
# Let's review
layout(matrix(seq(3)))
plot(
  pat$data$datetime, pat$data$temperature,
  pch = 15,
  col = adjustcolor("red", 0.4), # semi-transparent
  main = "Temperature"
)
plot(
  pat$data$datetime, pat$data$humidity,
  pch = 15,
  col = adjustcolor("blue", 0.4),
  main = "Humidity"
)
plot(
  pat$data$datetime, pat$data$pm2.5_cf_1,
  pch = 15,
  col = adjustcolor("black", 0.4),
  main = "PM 2.5"
)
layout(1)

# The pm2.5_cf1_data is clearly bad ... keep looking
pat <- get(load("./data/pat_99747.rda"))

plot(
  pat$data$datetime, pat$data$pm2.5_cf_1,
  pch = 15,
  col = adjustcolor("black", 0.4) # semi-transparent
)

# Now we have something to work with! Let's save it as .csv
readr::write_csv(pat$meta, file = "./data/meta_99747.csv")
readr::write_csv(pat$data, file = "./data/data_99747.csv")

# Where is this?
pas %>%
  pas_filter(sensor_index == "99747") %>%
  pas_leaflet()

# ==============================================================================
# HACK:  Temporary fix for older data. I fixed the code that generates the
# HACK:  data after the data were collected.
class(pat) <- union(class(pat), c("sts"))
# ==============================================================================


# Extract February and look at corrleation pltos as a simple, visual QC
pat %>%
  # TODO:  fix this
  ###pat_filterDate(20230201, 20230301, timezone = timezone) %>%
  pat_getData() %>%
  dplyr::select(humidity, temperature, pm2.5_cf_1_a, pm2.5_cf_1_b) %>%
  plot(pch = '.', col = adjustcolor("black", 0.4))

# ==============================================================================

# Now we'll conveft the 'pat' object to a 'monitor' object for use with the
# AirMonitor package

library(AirMonitor)

monitor <-
  pat %>%
  pat_distinct() %>% # HACK:  Temporary fix for older data
  pat_toMonitor()

# Look at data for February
feb <-
  monitor %>%
  monitor_filterDate(20230201, 20230301, timezone = "America/Santiago")

# Daily barplot
feb %>%
  monitor_dailyBarplot()

# Hourly time series
feb %>%
  monitor_timeseriesPlot(
    shadedNight = TRUE,
    addAQI = TRUE
  )
addAQILegend()

# How many hours in each Air Quality Category
feb %>%
  monitor_toAQCTable()

# Interactive Time series
feb %>%
  monitor_dygraph()



