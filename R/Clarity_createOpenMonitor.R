#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.isInitialized logger.debug
#' @importFrom MazamaCoreUtils getAPIKey
#'
#' @title Create a new Clarity 'mts_monitor' object
#'
#' @description Download, parse and enhance hourly timeseries data from Clarity
#' and create an object of class \code{mts_monitor} for use with the AirMonitor
#' package.
#'
#' @param api_key Clarity API READ Key. If \code{api_key = NULL}, it
#' will be obtained using \code{getAPIKey("Clarity-read")}.
#' @param synoptic Previously generated \emph{synoptic} object containing \code{datasourceId}.
#' @param datasourceId Clarity sensor identifier.
#' @param format Customized output format ("USFS2", "USFS").
#' @param parameter Parameter to use for data ("pm2.5" or "nowcast")
#' @param applyQC Logical specifying whether to use the Clarity QCFlag to
#' invalidate data values.
#'
#' @return An AirMonitor package \emph{mts_monitor} object.
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
#' synoptic <-
#'   Clarity_createOpenSynoptic(
#'     api_key = Clarity_API_READ_KEY
#'   )
#'
#' mon <-
#'   Clarity_createOpenMonitor(
#'     api_key = Clarity_API_READ_KEY,
#'     synoptic = synoptic,
#'     datasourceId = "DACHW7097",
#'     parameter = "pm2.5"
#'   )
#'
#' }, silent = FALSE)
#' }

Clarity_createOpenMonitor <- function(
  api_key = NULL,
  synoptic = NULL,
  datasourceId = NULL,
  format = "USFS2",
  parameter = c("pm2.5", "nowcast"),
  applyQC = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  if ( is.null(api_key) )
    api_key <- MazamaCoreUtils::getAPIKey("Clarity-read")

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(synoptic)
  MazamaCoreUtils::stopIfNull(datasourceId)
  MazamaCoreUtils::stopIfNull(format)

  parameter <- match.arg(parameter)

  if ( !datasourceId %in% synoptic$datasourceId ) {
    stop(sprintf("datasourceId '%s' is not found in synoptic", datasourceId))
  }

  # Check if MazamaSpatialUtils package has been initialized
  # via initializeMazamaSpatialUtils()
  if ( !spatialIsInitialized() ) {
    stop('`Clarity_createSynoptic` requires MazamaSpatialUtils to be initialized:

            initializeMazamaSpatialUtils()

         Please see `?initializeMazamaSpatialUtils for more details.')
  }


  # ----- Load data ------------------------------------------------------------

  # Download, parse and enhance synoptic data
  if ( logger.isInitialized() )
    logger.debug("----- Clarity_getOpenHourly() -----")

  # NOTE:  Always returns the last 10 days of hourly data
  tidyDF <- Clarity_getOpenHourly(
    api_key = api_key,
    datasourceId = datasourceId,
    format = format
  )

  # > dplyr::glimpse(tidyDF, width = 75)
  # Rows: 49
  # Columns: 11
  # $ pm2.5_QCFlag        <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
  # $ pm2.5               <dbl> 9.45, 9.33, 8.64, 8.36, 8.37, 8.93, 9.19, 9.0…
  # $ nowcast_QCFlag      <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
  # $ nowcast             <dbl> 8.96, 8.74, 8.54, 8.50, 8.57, 8.76, 8.65, 8.3…
  # $ datetime            <dttm> 2024-08-21 12:00:00, 2024-08-21 11:00:00, 20…
  # $ datasourceId        <chr> "DAABL1560", "DAABL1560", "DAABL1560", "DAABL…
  # $ locationName        <chr> "Gates ES (4096)", "Gates ES (4096)", "Gates …
  # $ longitude           <dbl> -118.2058, -118.2058, -118.2058, -118.2058, -…
  # $ latitude            <dbl> 34.07283, 34.07283, 34.07283, 34.07283, 34.07…
  # $ calibrationId       <chr> "CAHVV2Z9CH", "CAHVV2Z9CH", "CAHVV2Z9CH", "CA…
  # $ calibrationCategory <chr> "global PM2.5 v2", "global PM2.5 v2", "global…

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
      "datasourceId",
      "privacy",
      "sensorManufacturer",
      # Calibration fields
      "calibrationId",
      "calibrationCategory"
    )

  # TODO: use setdiff() to add extra monitor core metadata
  meta <-
    synoptic %>%
    dplyr::filter(datasourceId == !!datasourceId) %>%
    dplyr::select(dplyr::all_of(monitor_metaNames)) %>%
    # Add more metadata used by the AirMonitor package
    dplyr::mutate(
      pollutant = "PM2.5",
      units = "UG/M3",
      address = as.character(NA),
      dataIngestSource = "Clarity",
      dataIngestUrl = as.character(NA),
      AQSID = as.character(NA),
      fullAQSID = as.character(NA),
      deploymentType = as.character(NA),
      # Others required by monitor_leaflet
      deviceType = as.character(NA),
      deviceDescription = as.character(NA),
      deviceExtra = as.character(NA),
      dataIngestURL = as.character(NA),
      dataIngestUnitID = !!datasourceId,
      dataIngestExtra = as.character(NA),
      dataIngestDescription = as.character(NA)
    )

  # TODO:  This step can be removed when AirMonitor gets upgraded to replace
  # TODO:  'zip' with 'postalCode'.
  if ( !"zip" %in% names(meta) ) {
    meta$zip <- meta$postalCode
  }

  if ( nrow(meta) != 1 )
    stop(sprintf("Multiple records in 'synoptic' match '%s'", datasourceId));

  locationName <- unique(tidyDF$locationName)
  if ( length(locationName) > 1 )
    stop(sprintf("Multiple location names found: '%s'", paste0(locationName, collapse = ", ")))

  # NOTE:  locationName is missing from synoptic but available now
  meta$locationName <- unique(tidyDF$locationName)

  # Remove "Clarity_synoptic" class
  attributes(meta)$class <-
    setdiff(attributes(meta)$class, c("Clarity_synoptic", "synoptic"))

  # ----- Create data ----------------------------------------------------------

  columns <- c("datetime", parameter)

  data <- tidyDF %>% dplyr::select(dplyr::all_of(columns))

  names(data) <- c("datetime", meta$deviceDeploymentID)

  QCFlagName <- paste0(parameter, "_QCFlag")

  if ( applyQC ) {
    bad_mask <- is.na(tidyDF[[QCFlagName]]) | tidyDF[[QCFlagName]] == 0
    data[bad_mask,2] <- as.numeric(NA)
  }

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


  api_key <- Clarity_API_READ_KEY
  datasourceId <- "DMRGM1663"
  format <- "USFS2"
  parameter <- "pm2.5"
  applyQC <- TRUE


  synoptic <- Clarity_createOpenSynoptic(api_key)


  monitor <-
    Clarity_createOpenMonitor(
      api_key,
      synoptic,
      datasourceId,
      format = "USFS2",
      parameter
    )

}
