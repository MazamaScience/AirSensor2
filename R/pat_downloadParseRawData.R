#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.error logger.debug logger.isInitialized
#' @importFrom MazamaCoreUtils getAPIKey
#'
#' @title Download time series data from PurpleAir
#'
#' @param api_key PurpleAir API Read Key. If \code{api_key = NULL}, it
#' will be obtained using \code{getAPIKey("PurpleAir-read")}.
#' See \code{MazamaCoreUtils::\link[MazamaCoreUtils:setAPIKey]{setAPIKey}}.
#' @param sensor_index PurpleAir sensor unique identifier.
#' @param startdate Desired start datetime (ISO 8601).
#' @param enddate Desired end datetime (ISO 8601).
#' @param timezone Olson timezone used to interpret dates.
#' @param average Temporal averaging in minutes performed by PurpleAir. One of:
#' 0 (raw), 10, 30, 60 (hour), 360, 1440 (day).
#' @param fields Character string with PurpleAir field names for the Get Sensor Data API.
#' @param baseUrl Base URL for the PurpleAir API.
#'
#' @return Dataframe of time series PurpleAir data.
#'
#' @description Download and parse time series data for a specific \code{sensor_index}.
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
#' # Hourly (uncorrected) PM2.5
#' pat_raw <-
#'   pat_downloadParseRawData(
#'     api_key = PurpleAir_API_READ_KEY,
#'     sensor_index = "2323",
#'     startdate = "2023-02-01",
#'     enddate = "2023-02-03",
#'     timezone = "UTC",
#'     average = 60,
#'     fields = "pm2.5_atm"
#'   )
#'
#' head(pat_raw)
#'
#' }, silent = FALSE)
#' }

pat_downloadParseRawData <- function(
    api_key = NULL,
    sensor_index = NULL,
    startdate = NULL,
    enddate = NULL,
    timezone = "UTC",
    average = 0,
    fields = PurpleAir_PAT_QC_FIELDS,
    baseUrl = "https://api.purpleair.com/v1/sensors"
) {

  # ----- Validate parameters --------------------------------------------------

  if ( is.null(api_key) )
    api_key <- MazamaCoreUtils::getAPIKey("PurpleAir-read")

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(sensor_index)
  MazamaCoreUtils::stopIfNull(startdate)
  MazamaCoreUtils::stopIfNull(enddate)
  MazamaCoreUtils::stopIfNull(timezone)
  MazamaCoreUtils::stopIfNull(fields)
  MazamaCoreUtils::stopIfNull(baseUrl)

  tRange <- MazamaCoreUtils::dateRange(
    startdate = startdate,
    enddate = enddate,
    timezone = timezone,
    unit = "sec",
    ceilingStart = FALSE,
    ceilingEnd = FALSE
  )

  # ----- Request data ---------------------------------------------------------

  # NOTE:  PA API docs state:
  # NOTE:    Query is executed using data_timestamp >= start_timestamp
  # NOTE:    Query is executed using data_timestamp < end_timestamp.
  # NOTE:
  # NOTE:  However, when requesting hourly data, it looks like PA truncates to
  # NOTE:  the hour so an enddate of 23:59:59 is counted as hour 23 which is
  # NOTE:  then NOT included in the output.
  # NOTE:
  # NOTE:  Here, we add one second to end_timestamp to account for that.

  tbl <-
    PurpleAir_getSensorHistoryCSV(
      api_key = api_key,
      sensor_index = sensor_index,
      start_timestamp = as.numeric(tRange[1]),
      end_timestamp = as.numeric(tRange[2]) + 1,
      average = average,
      fields = fields,
      baseUrl = baseUrl
    )

  return(tbl)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  # Hourly pm2.5_atm (uncorrected)
  api_key <- PurpleAir_API_READ_KEY
  sensor_index <- "131611"
  startdate <- "2023-01-01"
  enddate <- "2023-01-03"
  timezone <- "UTC"
  average <- 0                   # Don't download too much data
  #fields <- "pm2.5_atm"
  fields <- PurpleAir_PAT_QC_FIELDS
  baseUrl <- "https://api.purpleair.com/v1/sensors"

  pat_raw <-
    pat_downloadParseRawData(
      api_key = api_key,
      sensor_index = sensor_index,
      startdate = startdate,
      enddate = enddate,
      timezone = timezone,
      average = average,
      fields = fields,
      baseUrl = baseUrl
    )

}
