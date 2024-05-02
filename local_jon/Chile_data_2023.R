library(AirSensor2)

source("global_vars.R")

logger.setup(
  debugLog = "Chile_data_debug.log",
  infoLog = "Chile_data_info.log",
  errorLog = "Chile_data_error.log"
)
logger.setLevel(TRACE)

initializeMazamaSpatialUtils()

chile_pas <-
  pas_createNew(
    api_key = PurpleAir_API_READ_KEY,
    fields = PurpleAir_PAS_METADATA_FIELDS,
    countryCodes = "CL",
    lookbackDays = 1000,
    location_type = 0
  )

save(chile_pas, file = "chile_pas.rda")

dim(chile_pas)

pas_lifespanPlot(chile_pas, main = "Sensor Reporting Lifespans in Chile")

timezone <- "America/Santiago"
start <- MazamaCoreUtils::parseDatetime("2023-01-01 00:00", timezone = timezone)
end <- MazamaCoreUtils::parseDatetime("2024-01-01 00:00", timezone = timezone)

chile_2023_pas <-
  chile_pas %>%
  pas_filter(date_created < end) %>%
  pas_filter(last_seen > start)

dim(chile_2023_pas)

pas_lifespanPlot(chile_2023_pas)

logger.info("===== Downloading 2023 data for %d sensors =====", nrow(chile_2023_pas))

for ( i in seq_len(nrow(chile_2023_pas)) ) {

  meta <- chile_2023_pas[i,]
  sensor_index <- meta$sensor_index


  name = sprintf("pat_%s", sensor_index)
  filePath = sprintf("data/%s.rda", name)

  if ( start > meta$date_created ) {
    sensor_start <- start
  } else {
    sensor_start <- meta$date_created
  }

  if ( end < meta$last_seen ) {
    sensor_end <- end
  } else {
    sensor_end <- meta$last_seen
  }

  logger.info("Working on %s ...", sensor_index)
  logger.trace("start = %s", MazamaCoreUtils::timeStamp(sensor_start, timezone = timezone, style = "clock"))
  logger.trace("end = %s", MazamaCoreUtils::timeStamp(sensor_end, timezone = timezone, style = "clock"))

  result <- try({

    pat <-
      pat_createHourly(
        api_key = PurpleAir_API_READ_KEY,
        pas = chile_2023_pas,
        sensor_index = sensor_index,
        startdate = sensor_start,
        enddate = sensor_end,
        timezone = timezone
      )

  })

  if ( "try-error" %in% class(result) ) {

    err_msg <- geterrmessage()
    logger.error(err_msg)

  } else {

    if ( nrow(pat$data) == 0 ) {
      logger.warn("Sensor %s has no data", sensor_index)
    }

    assign(name, pat)
    save(list = name, file = filePath)

  }

}




