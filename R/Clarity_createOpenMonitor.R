#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.isInitialized logger.debug
#'
#' @title Create a new Clarity 'mts_monitor' object
#'
#' @description Download, parse and enhance hourly timeseries data from Clarity
#' and create an object of class \code{mts_monitor} for use with the AirMonitor
#' package.
#'
#' @param api_key Clarity API READ key.
#' @param syn Previously generated \emph{synoptic} object containing \code{datasosurceId}.
#' @param datasourceId Clarity sensor identifier.
#' @param format Customized output format (currently only "USFS").
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
#' syn <-
#'   Clarity_createOpenSynoptic(
#'     api_key = Clarity_API_READ_KEY
#'   )
#'
#' mon <-
#'   Clarity_createOpenMonitor(
#'     api_key = Clarity_API_READ_KEY,
#'     syn = syn,
#'     parameter = "pm2.5"
#'   )
#'
#' }, silent = FALSE)
#' }

Clarity_createOpenMonitor <- function(
    api_key = NULL,
    syn = NULL,
    datasourceId = NULL,
    format = c("USFS"),
    parameter = c("pm2.5", "nowcast"),
    applyQC = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(syn)
  MazamaCoreUtils::stopIfNull(datasourceId)

  format <- match.arg(format)
  parameter <- match.arg(parameter)

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

  # > dplyr::glimpse(DF, width = 75)
  # Rows: 240
  # Columns: 8
  # $ QCFlag       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
  # $ pm2.5        <dbl> 3.39, 3.04, 2.68, 2.12, 1.65, 1.83, 1.72, 2.00, 2.37…
  # $ nowcast      <dbl> 3.04, 2.63, 2.32, 1.95, 1.78, 1.91, 2.00, 2.28, 2.62…
  # $ datetime     <dttm> 2023-05-04 19:00:00, 2023-05-04 18:00:00, 2023-05-0…
  # $ datasourceId <chr> "DMRGM1663", "DMRGM1663", "DMRGM1663", "DMRGM1663", …
  # $ locationName <chr> "Oxnard ES (5918)", "Oxnard ES (5918)", "Oxnard ES (…
  # $ longitude    <dbl> -118.3687, -118.3687, -118.3687, -118.3687, -118.368…
  # $ latitude     <dbl> 34.1785, 34.1785, 34.1785, 34.1785, 34.1785, 34.1785…

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
      "datasourceId",
      "privacy",
      "sensorManufacturer"
    )

  meta <-
    syn %>%
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
      deploymentType = as.character(NA)
    )

  if ( nrow(meta) != 1 )
    stop(sprintf("Multiple records in 'syn' match '%s'", datasourceId));

  locationName <- unique(tidyDF$locationName)
  if ( length(locationName) > 1 )
    stop(sprintf("Multiple location names found: '%s'", paste0(locationName, collapse = ", ")))

  # NOTE:  locationName is missing from syn but available now
  meta$locationName <- unique(tidyDF$locationName)

  # Remove "Clarity_synoptic" class
  attributes(meta)$class <-
    setdiff(attributes(meta)$class, c("Clarity_synoptic", "synoptic"))

  # ----- Create data ----------------------------------------------------------

  columns <- c("datetime", parameter)

  data <- tidyDF %>% dplyr::select(dplyr::all_of(columns))

  names(data) <- c("datetime", meta$deviceDeploymentID)

  if ( applyQC ) {
    bad_mask <- is.na(tidyDF$QCFlag) | tidyDF$QCFlag == 0
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
  format <- "USFS"
  parameter <- "pm2.5"
  applyQC <- TRUE


  syn <- Clarity_createOpenSynoptic(api_key)


  monitor <-
    Clarity_createOpenMonitor(
      api_key,
      syn,
      datasourceId,
      format = "USFS",
      parameter
    )

}
