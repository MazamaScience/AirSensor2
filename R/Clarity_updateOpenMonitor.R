#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.isInitialized logger.debug logger.trace
#' @importFrom MazamaCoreUtils getAPIKey
#'
#' @title Update a Clarity 'mts_monitor' object
#'
#' @description Download, parse and enhance the last 3 hours of data from Clarity
#' and append new data to the incoming Clarity \emph{mts_monitor} object
#' package.
#'
#' @note Maintenance of unique device-deployments will be preserved. If any
#' Clarity sensor is reported as having a new location, a new deployment will
#' be created as a separate time series.
#'
#' @param api_key Clarity API READ Key. If \code{api_key = NULL}, it
#' will be obtained using \code{getAPIKey("Clarity-read")}.
#' @param monitor Previously generated \emph{mts_monitor} object.
#' @param format Customized output format (currently only "USFS").
#' @param parameter Parameter to use for data ("pm2.5" or "nowcast")
#' @param applyQC Logical specifying whether to use the Clarity QCFlag to
#' invalidate data values.
#'
#' @return An AirMonitor package \emph{mts_monitor} object.
#'

Clarity_updateOpenMonitor <- function(
  api_key = NULL,
  monitor = NULL,
  format = "USFS",
  parameter = c("pm2.5", "nowcast"),
  applyQC = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  if ( is.null(api_key) )
    api_key <- MazamaCoreUtils::getAPIKey("Clarity-read")

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(monitor)
  MazamaCoreUtils::stopIfNull(format)

  parameter <- match.arg(parameter)

  # Check if MazamaSpatialUtils package has been initialized
  # via initializeMazamaSpatialUtils()
  if ( !spatialIsInitialized() ) {
    stop('`Clarity_createSynoptic` requires MazamaSpatialUtils to be initialized:

            initializeMazamaSpatialUtils()

         Please see `?initializeMazamaSpatialUtils for more details.')
  }

  # ----- Download current data ------------------------------------------------

  # Download, parse and enhance synoptic data
  if ( logger.isInitialized() )
    logger.debug("----- Clarity_getAllOpenHourly() -----")

  DFList <-
    Clarity_getAllOpenHourly(
      api_key = api_key,
      format = format,
      baseUrl =
        "https://clarity-data-api.clarity.io/v1/open/all-recent-measurement/pm25/hourly"
    )

  # ----- Create current meta --------------------------------------------------

  # > names(DFList)
  # [1] "synoptic"   "pm2.5_QC"   "pm2.5"      "nowcast_QC" "nowcast"

  if ( logger.isInitialized() )
    logger.trace("----- Clarity_enhanceRawSynopticData() -----")

  synoptic <-
    Clarity_enhanceRawSynopticData(
      DFList$synoptic
    ) %>%
    # Limit to CA-US-MX
    dplyr::filter(.data$countryCode %in% c("CA", "US", "MX"))

  # NOTE:  Below was copied from Clarity_createOpenMonitor.R

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

  # TODO: use setdiff() to add extra monitor core metadata
  meta <-
    synoptic %>%
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
      dataIngestUnitID = .data$datasourceId,
      dataIngestExtra = as.character(NA),
      dataIngestDescription = as.character(NA)
    ) %>%
    # Guarantee unique values of the primary key
    dplyr::distinct(.data$deviceDeploymentID, .keep_all = TRUE)

  # ----- Create data ----------------------------------------------------------

  # Guarantee that qc is ordered the same as data
  if ( parameter == "pm2.5" ) {
    data <- DFList$pm2.5
    qc <- DFList$pm2.5_QC %>% dplyr::select(dplyr::all_of(names(data)))
  } else if ( parameter == "nowcast" ) {
    data <- DFList$nowcast
    qc <- DFList$nowcast_QC %>% dplyr::select(dplyr::all_of(names(data)))
  }

  if ( applyQC ) {
    bad_mask <- is.na(qc) | qc == 0
    data[bad_mask] <- as.numeric(NA)
  }

  # Guarantee that data is ordered the same as meta
  data <-
    data %>%
    dplyr::select(dplyr::all_of(c("datetime", meta$datasourceId)))

  if ( nrow(meta) != (ncol(data) - 1) ) {
    logger.error("%d rows of meta cannot be matched to %d columns of data", nrow(meta), ncol(data))
    stop()
  }

  # Create new column names for data
  names(data) <- c("datetime", meta$deviceDeploymentID)

  current_monitor <- list(meta = meta, data = data)
  current_monitor <- structure(current_monitor, class = c("mts_monitor", "mts", class(current_monitor)))

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
  format <- "USFS"
  parameter <- "pm2.5"
  applyQC <- TRUE


  monitor <-
    Clarity_updateOpenMonitor(
      api_key,
      monitor,
      format,
      parameter,
      applyQC
    )

}
