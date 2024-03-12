#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.isInitialized logger.debug
#' @importFrom MazamaCoreUtils getAPIKey
#'
#' @title Convert a PurpleAir Timeseries to a 'mts_monitor' object
#'
#' @param api_key PurpleAir API READ Key. If \code{api_key = NULL}, it
#' will be obtained using \code{getAPIKey("PurpleAir-read")}.
#' @param pat Previously generated \emph{pat} object.
#'
#' @description Download, parse and enhance hourly timeseries data from PurpleAir
#' and create an object of class \code{mts_monitor} for use with the \pkg{AirMonitor}
#' package.
#'
#' @section Correction:
#'
#' Data from PurpleAir is typically corrected to more closely correlate with
#' EPA regulatory monitors.
#'
#' The EPA correction equation described in (see \link{pat_applyCorrection})
#' is used to correct the data from PurpleAir.
#'
#' @note Parallel processing using \code{parallel = TRUE} is not available on
#' Windows machines.
#'
#' @return An AirMonitor package \emph{mts_monitor} object.
#'
#' @references \href{https://www2.purpleair.com}{PurpleAir}
#' @references \href{https://api.purpleair.com}{PurpleAir API}
#' @references \href{https://www2.purpleair.com/policies/terms-of-service}{PurpleAir Terms of service}
#' @references \href{https://www2.purpleair.com/pages/license}{PurpleAir Data license}
#' @references \href{https://www2.purpleair.com/pages/attribution}{PurpleAir Data Attribution}
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' # AirSensor2 package
#' library(AirSensor2)
#'
#' # Set user's PurpleAir_API_READ_KEY
#' source('global_vars.R')
#' setAPIKey("PurpleAir-read", PurpleAir_API_READ_KEY)
#'
#' # Initialize spatial datasets
#' initializeMazamaSpatialUtils()
#'
#' mon <-
#'   pat_toMonitor(
#'     pas = example_pas,
#'     sensor_index = "76545",
#'     startdate = "2023-01-01",
#'     enddate = "2023-01-08",
#'     timezone = "UTC",
#'     verbose = TRUE
#'   )
#'
#' AirMonitor::monitor_timeseriesPlot(mon, shadedNight = TRUE)
#'
#' # Run data download in parallel (faster)
#' mon <-
#'   pat_toMonitor(
#'     pas = example_pas,
#'     sensor_index = "76545",
#'     startdate = "2023-01-01",
#'     enddate = "2023-01-08",
#'     timezone = "UTC",
#'     parallel = TRUE
#'   )
#'
#' AirMonitor::monitor_timeseriesPlot(mon, shadedNight = TRUE)
#'
#' }, silent = FALSE)
#' }

pat_toMonitor <- function(
    api_key = NULL,
    pas = NULL,
    sensor_index = NULL,
    startdate = NULL,
    enddate = NULL,
    timezone = "UTC",
    sleep = 0.5,
    parallel = FALSE,
    verbose = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  if ( is.null(api_key) )
    api_key <- MazamaCoreUtils::getAPIKey("PurpleAir-read")

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(pas)
  MazamaCoreUtils::stopIfNull(sensor_index)
  MazamaCoreUtils::stopIfNull(timezone)
  sleep <- MazamaCoreUtils::setIfNull(sleep, 0.5)
  parallel <- MazamaCoreUtils::setIfNull(parallel, FALSE)
  verbose <- MazamaCoreUtils::setIfNull(verbose, FALSE)

  # Check if MazamaSpatialUtils package has been initialized
  # via initializeMazamaSpatialUtils()
  if ( !spatialIsInitialized() ) {
    stop('`PurpleAir_createSynoptic` requires MazamaSpatialUtils to be initialized:

            initializeMazamaSpatialUtils()

         Please see `?initializeMazamaSpatialUtils for more details.')
  }

  # ----- Load data ------------------------------------------------------------

  pat <-
    pat_createHourly(
      api_key = api_key,
      pas = pas,
      sensor_index = sensor_index,
      startdate = startdate,
      enddate = enddate,
      timezone = timezone,
      parallel = parallel,
      verbose = verbose
    )

  # > dplyr::glimpse(pat$data, width = 75)
  # Rows: 148
  # Columns: 9
  # $ datetime     <dttm> 2022-09-07 07:00:00, 2022-09-07 08:00:00, 2022-09-0…
  # $ humidity     <dbl> 30.000, 30.034, 31.066, 32.000, 32.000, 32.000, 32.0…
  # $ temperature  <dbl> 79.733, 79.000, 79.000, 79.000, 79.000, 78.267, 78.0…
  # $ pm2.5_cf_1   <dbl> 10.3310, 5.6240, 3.1970, 1.3540, 0.9455, 0.8780, 0.7…
  # $ pm2.5_cf_1_a <dbl> 5.959, 5.301, 3.364, 1.446, 1.026, 0.980, 0.852, 1.8…
  # $ pm2.5_cf_1_b <dbl> 14.703, 5.947, 3.030, 1.262, 0.865, 0.776, 0.643, 1.…

  # > dplyr::glimpse(pat$meta)
  # Rows: 1
  # Columns: 26
  # $ deviceDeploymentID <chr> "c22zpemyv9_pa.76545"
  # $ deviceID           <chr> "pa.76545"
  # $ locationID         <chr> "c22zpemyv9"
  # $ locationName       <chr> "Gathard Engineering"
  # $ longitude          <dbl> -122.3576
  # $ latitude           <dbl> 47.65574
  # $ elevation          <dbl> 47
  # $ countryCode        <chr> "US"
  # $ stateCode          <chr> "WA"
  # $ countyName         <chr> "King"
  # $ timezone           <chr> "America/Los_Angeles"
  # $ houseNumber        <chr> NA
  # $ street             <chr> NA
  # $ city               <chr> NA
  # $ postalCode         <chr> NA
  # $ sensor_index       <chr> "76545"
  # $ last_modified      <dttm> 2020-10-08 00:00:23
  # $ date_created       <dttm> 2020-10-02 19:42:37
  # $ privacy            <chr> "public"
  # $ name               <chr> "Gathard Engineering"
  # $ location_type      <chr> "outside"
  # $ model              <chr> "PA-II"
  # $ hardware           <chr> "2.0+BME280+PMSX003-B+PMSX003-A"
  # $ firmware_version   <chr> "7.02"
  # $ firmware_upgrade   <chr> NA
  # $ sensorManufacturer <chr> "PurpleAir"

  # ----- Create meta ----------------------------------------------------------

  monitor_metaNames <-
    c(
      "deviceDeploymentID",
      "deviceID",
      "locationID",
      "locationName",
      "longitude",
      "latitude",
      "elevation",
      "countryCode",
      "stateCode",
      "countyName",
      "timezone",
      "houseNumber",
      "street",
      "city",
      "postalCode",
      "sensor_index",
      "privacy",
      "sensorManufacturer"
    )

  meta <-
    pat$meta %>%
    # Add more metadata used by the AirMonitor package
    dplyr::mutate(
      pollutant = "PM2.5",
      units = "UG/M3",
      address = as.character(NA),
      dataIngestSource = "PurpleAir",
      dataIngestUrl = as.character(NA),
      AQSID = as.character(NA),
      fullAQSID = as.character(NA),
      deploymentType = as.character(NA),
      # Others required by monitor_leaflet
      deviceType = .data$model,
      deviceDescription = as.character(NA),
      deviceExtra = as.character(NA),
      dataIngestURL = as.character(NA),
      dataIngestUnitID = !!sensor_index,
      dataIngestExtra = as.character(NA),
      dataIngestDescription = as.character(NA)
    )

  # TODO:  This step can be removed when AirMonitor gets upgraded to replace
  # TODO:  'zip' with 'postalCode'.
  if ( !"zip" %in% names(meta) ) {
    meta$zip <- meta$postalCode
  }

  if ( nrow(meta) != 1 )
    stop(sprintf("Multiple records in 'pas' match '%s'", sensor_index));

  # Remove "PurpleAir_timeseries" and "synoptic" classes
  attributes(meta)$class <-
    setdiff(attributes(meta)$class, c("PurpleAir_timeseries", "synoptic"))

  # ----- Create data ----------------------------------------------------------

  pat_corrected <- pat_applyCorrection(pat)

  columns <- c("datetime", "pm2.5_corrected")

  data <- pat_corrected$data %>% dplyr::select(dplyr::all_of(columns))

  names(data) <- c("datetime", meta$deviceDeploymentID)

  # ----- Return ---------------------------------------------------------------

  monitor <- list(meta = meta, data = data)

  monitor <- structure(monitor, class = c("mts_monitor", "mts", class(monitor)))

  return(monitor)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  library(AirSensor2)
  initializeMazamaSpatialUtils()

  source("global_vars.R")


  api_key = PurpleAir_API_READ_KEY
  pas = example_pas
  sensor_index = "44939"
  startdate = "2022-09-07"
  enddate = "2022-09-14"
  timezone = "America/Los_Angeles"
  fields = PurpleAir_PAT_EPA_HOURLY_FIELDS
  baseUrl = "https://api.purpleair.com/v1/sensors"
  sleep = 0.5
  parallel = FALSE
  verbose = TRUE


  monitor <-
    pat_toMonitor(
      api_key = api_key,
      pas = pas,
      sensor_index = sensor_index,
      startdate = startdate,
      enddate = enddate,
      timezone = timezone
    )

}
