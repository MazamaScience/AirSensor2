#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.isInitialized logger.debug logger.trace
#' @importFrom MazamaCoreUtils getAPIKey
#'
#' @title Update a Clarity 'mts_monitor' object
#'
#' @description Download, parse and enhance the last 3 hours of data from all Clarity
#' "open" sensors and append new data to the incoming Clarity \emph{mts_monitor} object
#' package.
#'
#' @note Maintenance of unique device-deployments will be preserved. If any
#' Clarity sensor is reported as having a new location, a new deployment will
#' be created as a separate time series.
#'
#' @param api_key Clarity API READ Key. If \code{api_key = NULL}, it
#' will be obtained using \code{getAPIKey("Clarity-read")}.
#' @param monitor Previously generated \emph{mts_monitor} object.
#' @param format Customized output format ("USFS2", "USFS").
#' @param parameter Parameter to use for data ("pm2.5" or "nowcast")
#' @param applyQC Logical specifying whether to use the Clarity QCFlag to
#' invalidate data values.
#' @param countryCodes Vector of ISO 3166-1 alpha-2 country codes used to filter
#' available data.
#'
#' @return An AirMonitor package \emph{mts_monitor} object.
#'

Clarity_updateAllOpenMonitors <- function(
    api_key = NULL,
    monitor = NULL,
    format = c("USFS2", "USFS"),
    parameter = c("pm2.5", "nowcast"),
    applyQC = TRUE,
    countryCodes = c("CA", "US", "MX")
) {

  # ----- Validate parameters --------------------------------------------------

  if ( is.null(api_key) )
    api_key <- MazamaCoreUtils::getAPIKey("Clarity-read")

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(monitor)

  format <- match.arg(format)
  parameter <- match.arg(parameter)

  # Check if MazamaSpatialUtils package has been initialized
  # via initializeMazamaSpatialUtils()
  if ( !spatialIsInitialized() ) {
    stop('`Clarity_createSynoptic` requires MazamaSpatialUtils to be initialized:

            initializeMazamaSpatialUtils()

         Please see `?initializeMazamaSpatialUtils for more details.')
  }


  # ----- Create current montior -----------------------------------------------

  current_monitor <-
    Clarity_createAllOpenMonitors(
      api_key = api_key,
      format = format,
      parameter = parameter,
      applyQC = applyQC,
      countryCodes = countryCodes
    )

  # ----- Merge with incoming montior ------------------------------------------

  monitor <-
    AirMonitor::monitor_combine(
      monitor,
      current_monitor,
      replaceMeta = TRUE,
      overlapStrategy = "replace all"
    )

  # ----- Return ---------------------------------------------------------------

  return(monitor)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  library(AirSensor2)
  initializeMazamaSpatialUtils()

  source("global_vars.R")

  meta <- get(load(url("https://airfire-data-exports.s3.us-west-2.amazonaws.com/sensors/v3/PM2.5/latest/data/clarity_PM2.5_latest_meta.rda")))
  data <- get(load(url("https://airfire-data-exports.s3.us-west-2.amazonaws.com/sensors/v3/PM2.5/latest/data/clarity_PM2.5_latest_data.rda")))
  monitor <- list(meta = meta, data = data)
  monitor <- structure(monitor, class = c("mts_monitor", "mts", class(monitor)))

  api_key <- Clarity_API_READ_KEY
  format <- "USFS2"
  parameter <- "pm2.5"
  applyQC <- TRUE


  monitor <-
    Clarity_updateAllOpenMonitors(
      api_key,
      monitor,
      format,
      parameter,
      applyQC
    )

}
