#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.isInitialized logger.debug
#' @importFrom MazamaCoreUtils getAPIKey
#'
#' @title Create a new PurpleAir 'mts_monitor' object
#'
#' @description Download, parse and enhance hourly timeseries data from PurpleAir
#' and create an object of class \code{mts_monitor} for use with the AirMonitor
#' package.
#'
#' @note This is a very earily implementation that does not include any QC or
#' correction equations.
#'
#' @param api_key PurpleAir API READ Key. If \code{api_key = NULL}, it
#' will be obtained using \code{getAPIKey("PurpleAir-read")}.
#' @param pas Previously generated \emph{pas} object containing \code{sensor_index}.
#' @param sensor_index PurpleAir sensor identifier.
#' @param parameter Parameter to use for data ("pm2.5_atm")
#' @param startdate Desired start time (ISO 8601) or \code{POSIXct}.
#' @param enddate Desired end time (ISO 8601) or \code{POSIXct}.
#' @param timezone Olson timezone used to interpret dates.
#' @param verbose Logical controlling the generation of warning and error messages.
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
#' library(AirSensor2)
#'
#' initializeMazamaSpatialUtils()
#'
#' mon <-
#'   PurpleAir_createMonitor(
#'     api_key = PurpleAir_API_READ_KEY,
#'     pas = MY_PAS,
#'     sensor_index = "76545",
#'     startdate = "2023-01-01",
#'     enddate = "2023-01-08",
#'     timezone = "UTC",
#'     verbose = TRUE
#'   )
#'
#' }, silent = FALSE)
#' }

PurpleAir_createMonitor <- function(
  api_key = NULL,
  pas = NULL,
  sensor_index = NULL,
  parameter = c("pm2.5_atm"),
  startdate = NULL,
  enddate = NULL,
  timezone = "UTC",
  verbose = FALSE
) {

  # ----- Validate parameters --------------------------------------------------

  if ( is.null(api_key) )
    api_key <- MazamaCoreUtils::getAPIKey("PurpleAir-read")

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(pas)
  MazamaCoreUtils::stopIfNull(sensor_index)
  MazamaCoreUtils::stopIfNull(timezone)
  verbose <- MazamaCoreUtils::setIfNull(verbose, FALSE)

  parameter <- match.arg(parameter)

  # Check if MazamaSpatialUtils package has been initialized
  # via initializeMazamaSpatialUtils()
  if ( !spatialIsInitialized() ) {
    stop('`PurpleAir_createSynoptic` requires MazamaSpatialUtils to be initialized:

            initializeMazamaSpatialUtils()

         Please see `?initializeMazamaSpatialUtils for more details.')
  }


  # ----- Load data ------------------------------------------------------------

  pat <- pat_createHourly(
    api_key = api_key,
    pas = pas,
    sensor_index = sensor_index,
    startdate = startdate,
    enddate = enddate,
    timezone = timezone,
    verbose = verbose
  )

  # > dplyr::glimpse(pat$data, width = 75)
  # Rows: 164
  # Columns: 6
  # $ datetime    <dttm> 2023-01-01 08:00:00, 2023-01-01 09:00:00, 2023-01-01…
  # $ humidity    <dbl> 64.466, 64.800, 63.633, 63.567, 64.933, 65.633, 64.26…
  # $ temperature <dbl> 51.733, 52.000, 52.000, 51.733, 51.000, 51.000, 51.00…
  # $ pm2.5_atm   <dbl> 15.2325, 15.9940, 11.8430, 9.2465, 11.6315, 17.4770, …
  # $ pm2.5_atm_a <dbl> 16.258, 17.190, 13.627, 10.943, 14.050, 22.965, 17.73…
  # $ pm2.5_atm_b <dbl> 14.207, 14.798, 10.059, 7.550, 9.213, 11.989, 10.163,…

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
  # $ zip                <chr> NA
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
  # $ sensorManufacturer <chr> "Purple Air"

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
      "zip",
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

  if ( nrow(meta) != 1 )
    stop(sprintf("Multiple records in 'pas' match '%s'", sensor_index));

  # Remove "purple_air_timeseries" class
  attributes(meta)$class <-
    setdiff(attributes(meta)$class, c("purple_air_timeseries", "synoptic"))

  # ----- Create data ----------------------------------------------------------

  columns <- c("datetime", parameter)

  data <- pat$data %>% dplyr::select(dplyr::all_of(columns))

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
  sensor_index = "76545"
  startdate = "2023-01-01"
  enddate = "2023-01-08"
  timezone = "America/Los_Angeles"
  fields = PurpleAir_HISTORY_HOURLY_PM25_FIELDS
  baseUrl = "https://api.purpleair.com/v1/sensors"
  verbose = TRUE


  monitor <-
    PurpleAir_createMonitor(
      api_key = api_key,
      pas = pas,
      sensor_index = sensor_index,
      startdate = startdate,
      enddate = enddate,
      timezone = timezone,
      verbose = verbose
    )

}
