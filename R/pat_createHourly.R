#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.error logger.debug logger.isInitialized
#' @importFrom MazamaCoreUtils getAPIKey
#'
#' @title Create a new PurpleAir hourly timeseries dataset.
#'
#' @param api_key PurpleAir API Read Key. If \code{api_key = NULL}, it
#' will be obtained using \code{getAPIKey("PurpleAir-read")}.
#' See \code{MazamaCoreUtils::\link[MazamaCoreUtils:setAPIKey]{setAPIKey}}.
#' @param pas Previously generated \emph{pas} object containing \code{sensor_index}.
#' @param sensor_index PurpleAir sensor unique identifier.
#' @param startdate Desired start time (ISO 8601) or \code{POSIXct}.
#' @param enddate Desired end time (ISO 8601) or \code{POSIXct}.
#' @param timezone Olson timezone used to interpret dates.
#' @param fields Character string with PurpleAir field names for the Get Sensor Data API.
#' @param sleep Seconds to sleep between API requests.
#' @param parallel Logical specifying whether to attempt simultaneous downloads
#' using \code{parallel::\link[parallel:mcparallel]{mcparallel}}. (Not available
#' on Windows.)
#' @param baseUrl Base URL for the PurpleAir API.
#' @param verbose Logical controlling the generation of warning and error messages.
#'
#' @return A PurpleAir Timeseries \emph{pat} object.
#'
#' @description Create a \code{pat} object for a specific \code{sensor_index}.
#' This function splits up the requested time range into 2-day intervals (the
#' maximum allowed by the PurpleAir API) and makes repeated calls to
#' \code{pat_downloadParseRawData()}. The \code{sleep} parameter waits a small
#' amount of time between API requests.
#'
#' The PurpleAir API will respond with "rate limiting" errors unless sleep is
#' set appropriately. When \code{parallel = TRUE}, \code{sleep} is ignored.
#'
#' By default, the only variables included in an "hourly pat" are:
#' \code{datetime, humidity, temperature, pm2.5_atm, pm2.5_atm_a, pm2.5_atm_b}.
#' These are sufficient for simple QC using \code{pm2.5_atm_a, pm2.5_atm_b} and
#' simple correction using linear fit models based on
#' \code{pm2.5_atm, temperature, humidity}.
#'
#' @note Parallel processing using \code{parallel = TRUE} is not available on
#' Windows machines.
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
#' pat <-
#'   pat_createHourly(
#'     pas = example_pas,
#'     sensor_index = "76545",
#'     startdate = "2023-01-01",
#'     enddate = "2023-01-08",
#'     timezone = "UTC",
#'     verbose = TRUE
#'   )
#'
#' View(pat$meta[1:100,])
#'
#' }, silent = FALSE)
#' }

pat_createHourly <- function(
    api_key = NULL,
    pas = NULL,
    sensor_index = NULL,
    startdate = NULL,
    enddate = NULL,
    timezone = "UTC",
    fields = PurpleAir_PAT_EPA_HOURLY_FIELDS,
    sleep = 0.5,
    parallel = FALSE,
    baseUrl = "https://api.purpleair.com/v1/sensors",
    verbose = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  if ( is.null(api_key) )
    api_key <- MazamaCoreUtils::getAPIKey("PurpleAir-read")

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(pas)
  MazamaCoreUtils::stopIfNull(sensor_index)
  MazamaCoreUtils::stopIfNull(timezone)
  MazamaCoreUtils::stopIfNull(fields)
  MazamaCoreUtils::stopIfNull(baseUrl)
  sleep <- MazamaCoreUtils::setIfNull(sleep, 0.5)
  parallel <- MazamaCoreUtils::setIfNull(parallel, FALSE)
  verbose <- MazamaCoreUtils::setIfNull(verbose, FALSE)

  # ----- Create hourly pat ----------------------------------------------------

  pat <-
    pat_createNew(
    api_key = api_key,
    pas = pas,
    sensor_index = sensor_index,
    startdate = startdate,
    enddate = enddate,
    timezone = timezone,
    average = 60,
    fields = fields,
    sleep = sleep,
    parallel = parallel,
    baseUrl = baseUrl,
    verbose = verbose
  ) %>%
    pat_distinct()

  # ----- Return ---------------------------------------------------------------

  return(pat)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  api_key = PurpleAir_API_READ_KEY
  pas = example_pas
  sensor_index = "76545"
  startdate = "2023-01-01"
  enddate = "2023-01-09"
  timezone = "America/Los_Angeles"
  fields = PurpleAir_PAT_EPA_HOURLY_FIELDS
  sleep = 0.5
  parallel = FALSE
  baseUrl = "https://api.purpleair.com/v1/sensors"
  verbose = TRUE

  pat <-
    pat_createHourly(
      api_key = api_key,
      pas = pas,
      sensor_index = sensor_index,
      startdate = startdate,
      enddate = enddate,
      timezone = timezone,
      fields = fields,
      sleep = sleep,
      parallel = parallel,
      baseUrl = baseUrl,
      verbose = verbose
    )

}
