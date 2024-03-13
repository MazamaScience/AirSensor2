# Demonstration of fast creation of a 'monitor' object

# ----- Setup ------------------------------------------------------------------

# AirSensor2 package
library(AirSensor2)

# Set user's PurpleAir_API_READ_KEY
source('global_vars.R')
setAPIKey("PurpleAir-read", PurpleAir_API_READ_KEY)

# Initialize spatial datasets
library(MazamaSpatialUtils)
setSpatialDataDir("~/Data/Spatial")
initializeMazamaSpatialUtils()

# ----- Monitor object ---------------------------------------------------------

ptm_0 <- proc.time()

mon <-
  PurpleAir_createNewMonitor(
    pas = example_pas,
    sensor_index = "76545",
    startdate = "2023-07-01",
    enddate = "2023-07-15",
    timezone = "UTC",
    verbose = TRUE
  )

ptm <- proc.time()
cat(sprintf(
  "Time to create monitor object: %.2f seconds",ptm[2] - ptm_0[2]
))


AirMonitor::monitor_timeseriesPlot(mon)

# ----- PAT object with parallel download --------------------------------------

ptm_0 <- proc.time()

pat <-
  pat_create(
    pas = example_pas,
    sensor_index = "76545",
    startdate = "2023-01-01",
    enddate = "2023-01-08",
    timezone = "UTC",
    parallel = TRUE,
    verbose = TRUE
  )

ptm <- proc.time()
cat(sprintf(
  "Time to create pat object: %.2f seconds", ptm[2] - ptm_0[2]
))

plot(
  x = pat$data$datetime,
  y = pat$data$pm2.5_atm,
  pch = 15,
  col = adjustcolor("black", alpha = 0.3),
  main = "Uncorrected, 2-minute data"
)

