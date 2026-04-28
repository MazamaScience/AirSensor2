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
#' will be obtained using `getAPIKey("OPENAQ")`.
#'
#' @return An AirMonitor package `mts_monitor` object.
#'
#' @examples
#' \donttest{
#' try({
#'   if (interactive()) {
#'     initializeMazamaSpatialUtils()
#'
#'     # NOTE:  Read environment vars from .env file with dotenv::load_dot_env()
#'     OPENAQ_API_KEY <- Sys.getenv("OPENAQ_API_KEY")
#'
#'     locations <-
#'       OpenAQ_createLocations(
#'         api_key = OPENAQ_API_KEY,
#'         countryCodes = "US",
#'         stateCodes = "IL",
#'         counties = "Cook",
#'         lookbackDays = 60
#'       )
#'
#'     monitor <-
#'       OpenAQ_createMonitor(
#'         locations = locations,
#'         locations_id = 6207297,
#'         parameter = "pm25",
#'         startdate = "2026-04-01",
#'         enddate = "2026-04-15",
#'         timezone = "America/Chicago",
#'         api_key = OPENAQ_API_KEY
#'       )
#'
#'     monitor %>%
#'       AirMonitor::monitor_timeseriesPlot(shadedNight = TRUE, addAQI = TRUE)
#'
#'   }
#' }, silent = FALSE)
#' }
#'
OpenAQ_createMonitor <- function(
    locations = NULL,
    locations_id = NULL,
    parameter = c("pm25"),
    startdate = NULL,
    enddate = NULL,
    timezone = "UTC",
    applyQC = TRUE,
    maxPages = 10,
    sleepSeconds = 0.2,
    api_key = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  if (is.null(api_key)) {
    api_key <- MazamaCoreUtils::getAPIKey("OPENAQ")
  }

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(locations)
  MazamaCoreUtils::stopIfNull(locations_id)
  MazamaCoreUtils::stopIfNull(startdate)
  MazamaCoreUtils::stopIfNull(enddate)
  MazamaCoreUtils::stopIfNull(timezone)

  parameter <- match.arg(parameter)

  if (parameter != "pm25") {
    stop("Only parameter = 'pm25' is currently supported.")
  }

  if (!timezone %in% OlsonNames()) {
    stop(sprintf("timezone '%s' is not found in OlsonNames()", timezone))
  }

  if (!isTRUE(is.logical(applyQC) && length(applyQC) == 1)) {
    stop("'applyQC' must be TRUE or FALSE.")
  }

  if (!is.numeric(maxPages) || length(maxPages) != 1 || is.na(maxPages) || maxPages < 1) {
    stop("'maxPages' must be a single number >= 1.")
  }

  if (!is.numeric(sleepSeconds) || length(sleepSeconds) != 1 || is.na(sleepSeconds) || sleepSeconds < 0) {
    stop("'sleepSeconds' must be a single number >= 0.")
  }

  locations_id <- as.character(locations_id)
  locations_ids <- as.character(locations$id)

  if (!locations_id %in% locations_ids) {
    stop(sprintf("locations_id '%s' is not found in locations$id", locations_id))
  }

  if (!spatialIsInitialized()) {
    stop(
      "`OpenAQ_createMonitor` requires MazamaSpatialUtils to be initialized:\n\n",
      "  initializeMazamaSpatialUtils()\n\n",
      "Please see `?initializeMazamaSpatialUtils` for more details."
    )
  }

  # ----- Identify location ----------------------------------------------------

  meta <-
    locations %>%
    dplyr::filter(as.character(.data$id) == locations_id)

  if (nrow(meta) != 1) {
    stop(sprintf("Expected exactly one record in 'locations' matching '%s'", locations_id))
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

  # ----- Handle different providers -------------------------------------------

  # TODO:  check on the provider_name
  # TODO:  bail with a warning if other than airgradient, airnow or clarity
  # TODO:  adjust parameters if airgradient

  # ----- Load data ------------------------------------------------------------

  if (logger.isInitialized()) {
    logger.debug("----- OpenAQ_downloadRawData() -----")
  }

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

  # TODO:  If Clarity, perform QC and rename 'corrected_pm25' to 'pm25'

  if (nrow(openaq_data) > 0) {
    openaq_data <-
      openaq_data %>%
      dplyr::select(dplyr::all_of(c("datetime", parameter))) %>%
      dplyr::distinct(.data$datetime, .keep_all = TRUE) %>%
      dplyr::arrange(.data$datetime)
  } else {
    openaq_data <-
      data.frame(
        datetime = as.POSIXct(character()),
        value = numeric(),
        stringsAsFactors = FALSE
      )

    names(openaq_data) <- c("datetime", parameter)
  }

  # Guarantee a complete, monotonic, hourly UTC time axis.
  hourly_axis <-
    data.frame(
      datetime = seq(
        from = startdate_utc,
        to = enddate_utc,
        by = "1 hour"
      )
    )

  data <-
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
      AQSID = as.character(NA),
      fullAQSID = as.character(NA),
      deploymentType = as.character(NA),
      deviceType = ifelse(.data$is_monitor == TRUE, "Monitor", "Sensor"),
      deviceDescription = as.character(NA),
      deviceExtra = as.character(NA),
      dataIngestSource = "OpenAQ",
      dataIngestUnitID = as.character(.data$id),
      dataIngestURL = as.character(NA),
      dataIngestExtra = as.character(NA),
      dataIngestDescription = as.character(NA)
    )

  class(meta) <- setdiff(class(meta), c("OpenAQ_locations", "locations"))

  # ----- Finalize data --------------------------------------------------------

  names(data) <- c("datetime", meta$deviceDeploymentID)

  if (applyQC) {
    bad_mask <- data[[2]] < 0
    bad_mask[is.na(bad_mask)] <- FALSE
    data[bad_mask, 2] <- NA_real_
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

