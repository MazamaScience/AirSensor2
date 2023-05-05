library(AirMonitor)

# Set up logging
logger.setup()
logger.setLevel(INFO)

library(AirSensor2)

# Set up spatial datasetx
initializeMazamaSpatialUtils()

# Get Clarity API key
source("global_vars.R")
api_key <- Clarity_API_READ_KEY

# ===== Functions ==============================================================

comparisonPlot <- function(
    clarity_id = NULL
) {

  clarity_mon <-
    clarity %>%
    monitor_select(clarity_id)

  airnow_mon <-
    airnow %>%
    monitor_select(clarity_mon$meta$airnow_id)

  clarity_mon %>%
    AirMonitor::monitor_timeseriesPlot(col = 'red', shadedNight = TRUE, ylim = c(0, 50), main = "")

  airnow_mon %>%
    AirMonitor::monitor_timeseriesPlot(add = TRUE, col = 'black')

  legend(
    'topright',
    legend = c("Clarity", "AirNow"),
    fill = c("red", "black"),
    border = c"red", "black")
  )

  title(sprintf(
    "Clarity/AirNow pair -- %d meters apart in %s County, %s",
    clarity_mon$meta$airnow_closestDistance,
    clarity_mon$meta$countyName,
    clarity_mon$meta$stateCode
  ))

}

# ===== Synoptic ===============================================================

datestamp <- strftime(lubridate::now(), "%Y%m%d")

synoptic <-
  Clarity_createOpenSynoptic(api_key) %>%
  dplyr::filter(countryCode == "US")

save(synoptic, file = paste0("~/Data/Clarity/Clarity_synoptic_", datestamp, ".rda"))

monitor <-
  monitor_loadLatest() %>%
  monitor_filter(countryCode == "US")

save(monitor, file = paste0("~/Data/Clarity/airnow_latest_", datestamp, ".rda"))

# ===== Timeseries =============================================================

monitorList <- list()

for ( i in seq_len(nrow(synoptic)) ) {

  if ( i %% 20 == 1 )
    logger.info("working on %d / %d ...", i, nrow(synoptic))

  id <- synoptic$datasourceId[i]

  result <- try({

    monitorList[[id]] <-
      Clarity_createOpenMonitor(
        api_key,
        synoptic,
        datasourceId = id,
        format = "USFS",
        parameter = "pm2.5"
      )

  }, silent = TRUE)

  if ( "try-error" %in% class(result) ) {
    err_msg <- geterrmessage()
    logger.warn("Skipping due to: %s", err_message)
    next
  }

}

Clarity_monitor <- monitor_combine(monitorList)

save(Clarity_monitor, file = paste0("~/Data/Clarity/Clarity_monitor_", datestamp, ".rda"))

logger.info("Successfully completed!")

# ===== Explore ================================================================

library(AirMonitor)

clarity <- get(load("~/Data/Clarity/Clarity_monitor_20230504.rda"))
airnow <- get(load("~/Data/Clarity/airnow_latest_20230504.rda"))

# ----- Leaflet map -----

map <- MazamaLocationUtils::table_leaflet(airnow$meta, radius = 4)
MazamaLocationUtils::table_leafletAdd(map, clarity$meta, radius = 4, color = 'red')

# ----- Closest distance -----

clarity$meta$airnow_closestDistance <- as.numeric(NA)
clarity$meta$airnow_id <- as.character(NA)

# Add distance information to clarity$meta
for ( i in seq_len(nrow(clarity$meta)) ) {
  distances <-
    MazamaLocationUtils::table_getDistanceFromTarget(
      airnow$meta,
      clarity$meta$longitude[i],
      clarity$meta$latitude[i]
    )
  minDistIndex <- which.min(distances$distanceFromTarget)
  clarity$meta$airnow_closestDistance[i] <- distances$distanceFromTarget[minDistIndex] # meters

  airnow_id <-
    airnow$meta %>%
    dplyr::filter(locationID == distances$locationID[minDistIndex]) %>%
    dplyr::pull(deviceDeploymentID)

  clarity$meta$airnow_id[i] <- airnow_id
}

# All Clarity sensors within 500m of an AirNow monitor
clarity_close <-
  clarity %>%
  monitor_filter(airnow_closestDistance < 500)


