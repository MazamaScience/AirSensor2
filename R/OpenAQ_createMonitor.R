#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.isInitialized logger.debug
#' @importFrom MazamaCoreUtils getAPIKey
#'
#' @title Create a new OpenAQ 'mts_monitor' object
#'
#' @description Download, parse and enhance hourly timeseries data from OpenAQ
#' and create an object of class `mts_monitor` for use with the AirMonitor
#' package.
#'
#' @param locations Previously generated locations object containing `id`.
#' @param locations_id OpenAQ location identifier.
#' @param parameter Parameter to use for data. Currently only `"pm25"` is supported.
#' @param startdate Start datetime in the requested timezone.
#' @param enddate End datetime in the requested timezone.
#' @param timezone Olson timezone used to interpret `startdate` and `enddate`.
#' @param maxPages Maximum number of pages to request automatically.
#' @param sleepSeconds Number of seconds to pause between additional requests.
#' @param applyQC Logical specifying whether to apply basic QC to invalidate
#' bad data values.
#' @param api_key OpenAQ API key. If `api_key = NULL`, it
#' will be obtained using `getAPIKey("OpenAQ-read")`.
#'
#' @return An AirMonitor package `mts_monitor` object.
#'
OpenAQ_createMonitor <- function(
    locations = NULL,
    locations_id = NULL,
    parameter = c("pm25"),
    startdate = NULL,
    enddate = NULL,
    timezone = "UTC",
    applyQC = TRUE,
    maxPages = 1,
    sleepSeconds = 0.2,
    api_key = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  if ( is.null(api_key) )
    api_key <- MazamaCoreUtils::getAPIKey("OpenAQ-read")

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(locations)
  MazamaCoreUtils::stopIfNull(locations_id)
  MazamaCoreUtils::stopIfNull(startdate)
  MazamaCoreUtils::stopIfNull(enddate)
  MazamaCoreUtils::stopIfNull(timezone)

  parameter <- match.arg(parameter)

  if ( parameter != "pm25" ) {
    stop("Only parameter = 'pm25' is currently supported.")
  }

  if ( !timezone %in% OlsonNames() ) {
    stop(sprintf("timezone '%s' is not found in OlsonNames()", timezone))
  }

  if ( !locations_id %in% locations$id ) {
    stop(sprintf("locations_id '%s' is not found in locations$id", locations_id))
  }

  if ( !spatialIsInitialized() ) {
    stop('`OpenAQ_createMonitor` requires MazamaSpatialUtils to be initialized:

            initializeMazamaSpatialUtils()

         Please see `?initializeMazamaSpatialUtils` for more details.')
  }

  # ----- Identify location ----------------------------------------------------

  meta <-
    locations %>%
    dplyr::filter(.data$id == locations_id)

  if ( nrow(meta) != 1 ) {
    stop(sprintf("Multiple records in 'locations' match '%s'", locations_id))
  }

  # ----- Create requested time window -----------------------------------------

  requestedRange <-
    MazamaCoreUtils::timeRange(
      starttime = startdate,
      endtime = enddate,
      timezone = timezone,
      unit = "hour"
    )

  startdate_utc <- lubridate::with_tz(requestedRange[1], "UTC")
  enddate_utc <- lubridate::with_tz(requestedRange[2], "UTC")

  # ----- Load data ------------------------------------------------------------

  if ( logger.isInitialized() )
    logger.debug("----- OpenAQ_downloadRawData() -----")

  openaq_data <-
    OpenAQ_downloadRawData(
      locations_id = locations_id,
      parameters = parameter,
      data = "hours",
      startdate = startdate_utc,
      enddate = enddate_utc,
      limit = 1000,
      maxPages = maxPages,
      sleepSeconds = sleepSeconds,
      api_key = api_key
    )

  # Guarantee a complete, monotonic, hourly UTC time axis.
  hourly_axis <-
    data.frame(
      datetime = seq(
        from = startdate_utc,
        to = enddate_utc,
        by = "1 hour"
      )
    )

  openaq_data <-
    hourly_axis %>%
    dplyr::left_join(openaq_data, by = "datetime") %>%
    dplyr::arrange(.data$datetime)

  # ----- Create meta ----------------------------------------------------------

  meta <-
    meta %>%
    dplyr::mutate(
      pollutant = "PM2.5",
      units = "UG/M3",
      address = as.character(NA),
      dataIngestSource = "OpenAQ",
      dataIngestUrl = as.character(NA),
      AQSID = as.character(NA),
      fullAQSID = as.character(NA),
      deploymentType = as.character(NA),
      deviceDescription = as.character(NA),
      deviceExtra = as.character(NA),
      dataIngestURL = as.character(NA),
      dataIngestUnitID = as.character(.data$id),
      dataIngestExtra = as.character(NA),
      dataIngestDescription = as.character(NA)
    )

  attributes(meta)$class <-
    setdiff(attributes(meta)$class, c("OpenAQ_locations", "locations"))

  # ----- Create data ----------------------------------------------------------

  columns <- c("datetime", parameter)
  data <- openaq_data %>% dplyr::select(dplyr::all_of(columns))

  names(data) <- c("datetime", meta$deviceDeploymentID)

  if ( applyQC ) {
    bad_mask <- is.na(data[[2]]) | data[[2]] < 0
    data[bad_mask, 2] <- as.numeric(NA)
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

  library(dotenv)
  dotenv::load_dot_env()

  Sys.getenv("OPENAQ_API_KEY")

  OPENAQ_API_KEY <- Sys.getenv("OPENAQ_API_KEY")

  MazamaCoreUtils::setAPIKey("OPENAQ", Sys.getenv("OPENAQ_API_KEY"))

  locations <-
    OpenAQ_createLocations(
      api_key = OPENAQ_API_KEY,
      countryCodes = "US",
      stateCodes = "IL",
      counties = "Cook",
      lookbackDays = 60,
      providers = NULL,
      manufacturers = NULL,
      is_monitor = NULL,
      limit = 1000
    )


  #locations_id <- 3301366  # AirNow
  locations_id <- 6207297  # Clarity
  #locations_id <- 1370216  # AirGradient
  parameter <- "pm25"
  startdate <- MazamaCoreUtils::parseDatetime("2026-04-01 00:00:00", timezone = "UTC")
  enddate <- MazamaCoreUtils::parseDatetime("2026-04-15 00:00:00", timezone = "UTC")
  timezone = "UTC"
  limit <- 1000
  maxPages <- 10
  sleepSeconds <- 0.2
  applyQC <- TRUE
  api_key <- OPENAQ_API_KEY

  monitor <-
    OpenAQ_createMonitor(
      locations = locations,
      locations_id = locations_id,
      parameter = "pm25",
      startdate = startdate,
      enddate = enddate,
      timezone = "UTC",
      applyQC = TRUE,
      maxPages = maxPages,
      sleepSeconds = sleepSeconds,
      api_key = api_key
    )

  monitor %>% AirMonitor::monitor_timeseriesPlot(shadedNight = TRUE, addAQI = TRUE)

}
