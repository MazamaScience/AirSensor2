#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.error logger.debug logger.isInitialized
#' @importFrom MazamaCoreUtils getAPIKey
#'
#' @title Create a raw data PurpleAir timeseries dataset.
#'
#' @param api_key PurpleAir API Read Key. If `api_key = NULL`, it
#' will be obtained using `getAPIKey("PurpleAir-read")`.
#' See [MazamaCoreUtils::setAPIKey()].
#' @param pas Previously generated *pas* object containing `sensor_index`.
#' @param sensor_index PurpleAir sensor unique identifier.
#' @param startdate Desired start time (ISO 8601) or `POSIXct`.
#' @param enddate Desired end time (ISO 8601) or `POSIXct`.
#' @param timezone Olson timezone used to interpret dates.
#' @param fields Character string with PurpleAir field names for the Get Sensor Data API.
#' @param read_keys Optional, comma separated list of sensor read_keys is required
#' for private devices. It is separate from the api_key and each sensor has its own
#' read_key. Submit multiple keys by separating them with a comma (,) character
#' for example: `"key-one,key-two,key-three"`.
#' @param sleep Seconds to sleep between API requests.
#' @param parallel Logical specifying whether to attempt simultaneous downloads
#' using [parallel::mcparallel()]. (Not available
#' on Windows.)
#' @param baseUrl Base URL for the PurpleAir API.
#' @param verbose Logical controlling the generation of warning and error messages.
#'
#' @return A raw PurpleAir Timeseries *pat* object.
#'
#' @description Create a `pat` object for a specific `sensor_index`.
#' This function splits up the requested time range into 2-day intervals (the
#' maximum allowed by the PurpleAir API) and makes repeated calls to
#' `pat_downloadParseRawData()`. The `sleep` parameter waits a small
#' amount of time between API requests.
#'
#' The PurpleAir API will respond with "rate limiting" errors unless sleep is
#' set appropriately. When `parallel = TRUE`, `sleep` is ignored.
#'
#' @note Parallel processing using `parallel = TRUE` is not available on
#' Windows machines.
#'
#' @references [PurpleAir](https://www2.purpleair.com)
#' @references [PurpleAir API](https://api.purpleair.com)
#' @references [PurpleAir Terms of service](https://www2.purpleair.com/policies/terms-of-service)
#' @references [PurpleAir Data license](https://www2.purpleair.com/pages/license)
#' @references [PurpleAir Data Attribution](https://www2.purpleair.com/pages/attribution)
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
#'   pat_createRaw(
#'     pas = example_pas_pm25,
#'     sensor_index = "76545",
#'     startdate = "2023-01-01",
#'     enddate = "2023-01-03",
#'     timezone = "UTC",
#'     verbose = TRUE
#'   )
#'
#' View(pat$data[1:100,])
#'
#' }, silent = FALSE)
#' }

pat_createRaw <- function(
    api_key = NULL,
    pas = NULL,
    sensor_index = NULL,
    startdate = NULL,
    enddate = NULL,
    timezone = "UTC",
    fields = PurpleAir_PAT_QC_FIELDS,
    read_keys = NULL,
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
    pat_create(
      api_key = api_key,
      pas = pas,
      sensor_index = sensor_index,
      startdate = startdate,
      enddate = enddate,
      timezone = timezone,
      average = 0,
      fields = fields,
      read_keys = read_keys,
      sleep = sleep,
      parallel = parallel,
      baseUrl = baseUrl,
      verbose = verbose
    )

  # ----- Return ---------------------------------------------------------------

  return(pat)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  api_key = PurpleAir_API_READ_KEY
  pas = example_pas_pm25
  sensor_index = "76545"
  startdate = "2023-01-01"
  enddate = "2023-01-03"
  timezone = "America/Los_Angeles"
  fields = PurpleAir_PAT_QC_FIELDS
  sleep = 0.5
  parallel = FALSE
  baseUrl = "https://api.purpleair.com/v1/sensors"
  verbose = TRUE

  pat <-
    pat_createRaw(
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
