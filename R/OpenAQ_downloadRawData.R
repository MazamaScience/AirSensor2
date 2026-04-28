#' Download raw OpenAQ measurements
#'
#' Downloads raw measurements from OpenAQ for a single location and returns
#' a wide data frame with one column per requested parameter.
#'
#' For each requested parameter, this function identifies matching sensors at
#' the requested location, downloads all available pages of measurements up to
#' `maxPages`, stacks measurements across sensors, sorts by `datetime`, and
#' deduplicates by keeping the record from the most recently active sensor.
#' The final data frame is created by joining parameter-specific measurement
#' tables by `datetime`.
#'
#' @param locations_id OpenAQ location ID.
#' @param parameters Specific parameters for which to download data.
#' @param data Character string specifying the data interval to return.
#' @param startdate Desired start datetime as POSIXct.
#' @param enddate Desired end datetime as POSIXct.
#' @param limit Maximum number of records to request per page.
#' @param maxPages Maximum number of pages to request automatically.
#' @param sleepSeconds Number of seconds to pause between additional requests.
#' @param api_key OpenAQ API key.
#'
#' @return A data frame with columns `datetime` plus one column for each
#'   requested parameter.
#' @export
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
#'     raw_data <-
#'       OpenAQ_downloadRawData(
#'         locations_id = 1370216,
#'         startdate = "2026-04-01",
#'         enddate = "2026-04-15",
#'         api_key = OPENAQ_API_KEY
#'       )
#'
#'     raw_data %>% plot()
#'
#'   }
#' }, silent = FALSE)
#' }

OpenAQ_downloadRawData <- function(
    locations_id = NULL,
    parameters = c("pm25", "temperature", "relativehumidity"),
    data = c("hours", "measurements"),
    startdate = NULL,
    enddate = NULL,
    limit = 1000,
    maxPages = 10,
    sleepSeconds = 0.2,
    api_key = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  if ( is.null(api_key) ) {
    api_key <- MazamaCoreUtils::getAPIKey("OPENAQ")
  }

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(locations_id)

  startdate <- MazamaCoreUtils::parseDatetime(startdate, timezone = "UTC")
  enddate <- MazamaCoreUtils::parseDatetime(enddate, timezone = "UTC")

  if ( !is.numeric(limit) || length(limit) != 1 || is.na(limit) || limit < 1 ) {
    stop("parameter 'limit' must be a single positive number")
  }

  if ( !is.numeric(maxPages) || length(maxPages) != 1 || is.na(maxPages) || maxPages < 1 ) {
    stop("parameter 'maxPages' must be a single positive number")
  }

  if ( !is.numeric(sleepSeconds) || length(sleepSeconds) != 1 || is.na(sleepSeconds) || sleepSeconds < 0 ) {
    stop("parameter 'sleepSeconds' must be a single non-negative number")
  }

  data <- match.arg(data)

  all_parameters <- OpenAQ_getParameters() %>% dplyr::pull(.data$name)
  parameters <- match.arg(parameters, choices = all_parameters, several.ok = TRUE)

  limit <- as.integer(limit)
  maxPages <- as.integer(maxPages)

  # ----- Helper to return an empty result ------------------------------------

  empty_result <- function(parameters) {
    df <- data.frame(datetime = as.POSIXct(character(), tz = "UTC"))
    for ( parameter in parameters ) {
      df[[parameter]] <- numeric(0)
    }
    return(df)
  }

  # ----- Helper for improved error messages -----------------------------------

  safeOpenAQCall <- function(expr, context) {
    tryCatch(
      expr,
      error = function(e) {
        stop(
          context, "\n",
          "Original error: ", conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }

  # ----- Load sensors ---------------------------------------------------------

  sensors <- safeOpenAQCall(
    openaq::list_location_sensors(
      locations_id = locations_id,
      as_data_frame = TRUE,
      dry_run = FALSE,
      rate_limit = FALSE,
      api_key = api_key
    ),
    paste0(
      "Unable to download sensor metadata for OpenAQ location ",
      locations_id, "."
    )
  )

  sensors <- as.data.frame(sensors, stringsAsFactors = FALSE)

  if ( nrow(sensors) == 0 ) {
    return(empty_result(parameters))
  }

  # ----- Retain only requested parameters -------------------------------------

  parameter_ids <- OpenAQ_parameterToID(parameters)

  parameter_lookup <-
    data.frame(
      parameters_id = parameter_ids,
      parameter_name = parameters,
      stringsAsFactors = FALSE
    )

  sensors <-
    sensors %>%
    # Drop sensors with missing time information
    dplyr::filter(
      !is.na(.data$datetime_first_utc),
      !is.na(.data$datetime_last_utc),
    ) %>%
    dplyr::filter(.data$parameters_id %in% parameter_ids) %>%
    dplyr::left_join(parameter_lookup, by = "parameters_id")

  if ( nrow(sensors) == 0 ) {
    warning(
      "No matching sensors were found for the requested parameters at this location.",
      call. = FALSE
    )
    return(empty_result(parameters))
  }

  # Remove sensors that cannot have data in the requested time range.
  sensors <-
    sensors %>%
    dplyr::mutate(
      datetime_first_utc = as.POSIXct(.data$datetime_first_utc, tz = "UTC"),
      datetime_last_utc  = as.POSIXct(.data$datetime_last_utc,  tz = "UTC")
    ) %>%
    dplyr::filter(
      .data$datetime_last_utc >= startdate,
      .data$datetime_first_utc <= enddate
    )

  if ( nrow(sensors) == 0 ) {
    warning(
      "No matching sensors were found for the requested parameters in the requested date range.",
      call. = FALSE
    )
    return(empty_result(parameters))
  }

  # ----- Download data for each parameter ------------------------------------

  dataList <- list()

  for ( parameter in parameters ) {

    parameter_sensors <-
      sensors %>%
      dplyr::filter(.data$parameter_name == parameter) %>%
      dplyr::arrange(dplyr::desc(.data$datetime_last_utc), dplyr::desc(.data$id))

    if ( nrow(parameter_sensors) == 0 ) {
      next
    }

    sensor_data_list <- list()

    for ( i in seq_len(nrow(parameter_sensors)) ) {

      sensor_id <- parameter_sensors$id[[i]]
      sensor_last_utc <- parameter_sensors$datetime_last_utc[[i]]

      sensor_data <- OpenAQ_downloadPages(
        fetchPageFUN = function(page, limit) {
          safeOpenAQCall(
            openaq::list_sensor_measurements(
              sensors_id = sensor_id,
              data = data,
              datetime_from = startdate,
              datetime_to = enddate,
              limit = limit,
              page = page,
              as_data_frame = TRUE,
              dry_run = FALSE,
              rate_limit = FALSE,
              api_key = api_key
            ),
            paste0(
              "Unable to download OpenAQ measurements for sensor ",
              sensor_id, " (parameter '", parameter, "')."
            )
          )
        },
        limit = limit,
        maxPages = maxPages,
        sleepSeconds = sleepSeconds,
        warnTruncated = FALSE
      )

      sensor_data <- as.data.frame(sensor_data, stringsAsFactors = FALSE)

      if ( nrow(sensor_data) == 0 ) {
        next
      }

      sensor_data <-
        sensor_data %>%
        dplyr::select(
          datetime = .data$datetime_to,
          value = .data$value
        ) %>%
        dplyr::mutate(
          datetime = as.POSIXct(.data$datetime, tz = "UTC"),
          sensor_id = sensor_id,
          sensor_datetime_last_utc = sensor_last_utc
        )

      sensor_data_list[[length(sensor_data_list) + 1]] <- sensor_data

    }

    if ( length(sensor_data_list) == 0 ) {
      next
    }

    # Stack all matching sensors for this parameter, then prefer records from
    # the most recently active sensor when duplicate datetimes occur.
    parameter_data <-
      dplyr::bind_rows(sensor_data_list) %>%
      dplyr::arrange(
        .data$datetime,
        dplyr::desc(.data$sensor_datetime_last_utc),
        dplyr::desc(.data$sensor_id)
      ) %>%
      dplyr::distinct(.data$datetime, .keep_all = TRUE) %>%
      dplyr::select(
        datetime = .data$datetime,
        value = .data$value
      ) %>%
      dplyr::arrange(.data$datetime)

    names(parameter_data)[names(parameter_data) == "value"] <- parameter

    dataList[[parameter]] <- parameter_data

  }

  # ----- Combine parameter data -----------------------------------------------

  if ( length(dataList) == 0 ) {
    warning(
      "No measurements were returned for the requested parameters and date range.",
      call. = FALSE
    )
    return(empty_result(parameters))
  }

  measurements <-
    Reduce(
      f = function(x, y) dplyr::full_join(x, y, by = "datetime"),
      x = dataList
    ) %>%
    dplyr::arrange(.data$datetime)

  # ----- Ensure requested column order ----------------------------------------

  missing_parameters <- setdiff(parameters, names(dataList))

  for ( parameter in missing_parameters ) {
    measurements[[parameter]] <- NA_real_
  }

  measurements <- measurements[, c("datetime", parameters), drop = FALSE]

  return(measurements)

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

  locations_id <- 3301366  # AirNow
  locations_id <- 6207297  # Clarity
  locations_id <- 1370216  # AirGradient
  parameters <- c("pm25", "temperature", "relativehumidity")
  data <- "hours"
  startdate <- MazamaCoreUtils::parseDatetime("2026-04-01 00:00:00", timezone = "UTC")
  enddate <- MazamaCoreUtils::parseDatetime("2026-04-15 00:00:00", timezone = "UTC")
  limit <- 1000
  maxPages <- 10
  sleepSeconds <- 0.2
  api_key <- OPENAQ_API_KEY

  df <-
    OpenAQ_downloadRawData(
      locations_id = locations_id,
      parameters = parameters,
      data = data,
      startdate = startdate,
      enddate = enddate,
      limit = limit,
      maxPages = maxPages,
      sleepSeconds = sleepSeconds,
      api_key = api_key
    )

}
