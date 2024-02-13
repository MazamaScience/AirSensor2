#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.error logger.debug logger.isInitialized
#' @importFrom MazamaCoreUtils getAPIKey
#'
#' @title Download synoptic data from PurpleAir
#'
#' @param api_key PurpleAir API READ Key. If \code{api_key = NULL}, it
#' will be obtained using \code{getAPIKey("PurpleAir-read")}.
#' See \code{MazamaCoreUtils::\link[MazamaCoreUtils:setAPIKey]{setAPIKey}}.
#' @param fields Character string with PurpleAir field names for the Get Sensor Data API.
#' @param location_type The \code{location_type} of the sensors. Possible values
#' are: 0 = Outside, 1 = Inside or \code{NULL} = both.
#' @param read_keys Optional comma separated list of sensor read_keys is required
#' for private devices. It is separate to the api_key and each sensor has its own
#' read_key. Submit multiple keys by separating them with a comma (,) character
#' for example: key-one,key-two,key-three.
#' @param show_only Optional, comma separated list of sensor_index values. When
#' provided, the results are limited only to the sensors included in this list.
#' @param modified_since The \code{modified_since} parameter generates reults that
#' include only sensors modified after the provided time stamp. Using the
#' time stamp value associated with a previous call (recommended) will limit results to
#' those with new values since the last request. Using a value of 0 will match
#' sensors modified at any time.
#' @param max_age Number of seconds used to filter results to only include sensors
#' modified or updated within the the last \code{max_age} seconds. Using a value of 0 will match all sensors.
#' @param west Longitude of the western edge of the bounding box in which to find sensors.
#' @param east Longitude of the eastern edge of the bounding box in which to find sensors.
#' @param south Latitude of the southern edge of the bounding box in which to find sensors.
#' @param north Latitude of the northern edge of the bounding box in which to find sensors.
#' @param baseUrl Base URL for the PurpleAir API.
#'
#' @return Dataframe of synoptic PurpleAir data.
#'
#' @description Download and parse raw synoptic data from PurpleAir within the
#' specified region.
#'
#' \emph{Synoptic} data provides access to data from many PurpleAir sensors
#' at a moment in time and includes both metadata and recent PM2.5 averages for
#' each sensor. Data returned by this function is typically used to create
#' static or interactive maps.
#'
#' If \code{show_only} is used to request specific sensors, the bounding box
#' information is ignored.
#'
#' \strong{NOTE:} Most users will want to use the \code{pas_createNew} function
#' which enhances raw synoptic data with additional spatial metadata so that it
#' meets the criteria for use as a \emph{pas} object.
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
#' initializeMazamaSpatialUtils()
#'
#' source("global_vars.R") # contains PurpleAir_API_READ_KEY
#'
#' # Download metadata fields only to look for historical data
#' pas_raw <-
#'   pas_downloadParseRawData(
#'     api_key = PurpleAir_API_READ_KEY,
#'     max_age = 3600 * 24 * 365 * 10,
#'     west = -120.5,
#'     east = -120,
#'     south = 48.2,
#'     north = 48.7
#'   )
#'
#' fields <- c("sensor_index", "name", "date_created", "last_seen")
#' View(pas_raw[,fields])
#'
#' # Download metadata, PM2.5 and weather parameter fields
#' pas_raw <-
#'   pas_downloadParseRawData(
#'     api_key = PurpleAir_API_READ_KEY,
#'     fields = PurpleAir_DATA_AVG_PM25_FIELDS,
#'     location_type = 0,
#'     modified_since = NULL,
#'     max_age = 3600 * 24,
#'     west = -125,
#'     east = -117,
#'     south = 42,
#'     north = 49
#'   )
#'
#' View(pas_raw[1:100,])
#'
#' }, silent = FALSE)
#' }

pas_downloadParseRawData <- function(
  api_key = NULL,
  fields = PurpleAir_SENSOR_METADATA_FIELDS,
  location_type = 0,
  read_keys = NULL,
  show_only = NULL,
  modified_since = NULL,
  max_age = 3600 * 24 * 7,
  west = NULL,
  east = NULL,
  south = NULL,
  north = NULL,
  baseUrl = "https://api.purpleair.com/v1/sensors"
) {

  # ----- Validate parameters --------------------------------------------------

  if ( is.null(api_key) )
    api_key <- MazamaCoreUtils::getAPIKey("PurpleAir-read")

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(fields)
  max_age <- MazamaCoreUtils::setIfNull(max_age, 3600 * 24 * 7) # 1 week
  MazamaCoreUtils::stopIfNull(baseUrl)

  if ( !is.null(location_type) ) {
    location_type <- as.numeric(location_type)
    if ( !location_type %in% c(0, 1) ) {
      stop("'location_type' must be one of 0 (outside) or 1 (inside).")
    }
  }

  if ( !is.null(show_only) ) {

    west <- NULL
    north <- NULL
    east <- NULL
    south <- NULL

  } else {

    MazamaCoreUtils::stopIfNull(west)
    MazamaCoreUtils::stopIfNull(east)
    MazamaCoreUtils::stopIfNull(south)
    MazamaCoreUtils::stopIfNull(north)

    # Ensure correct order of longitudes
    if ( west > east ) {
      a <- east
      east <- west
      west <- a
    }

    # Ensure correct order of latitudes
    if ( south > north) {
      a <- north
      north <- south
      south <- a
    }

  }

  # ----- Request data ---------------------------------------------------------

  PAList <-
    PurpleAir_getSensorsData(
      api_key = api_key,
      fields = fields,
      location_type = location_type,
      read_keys = read_keys,
      show_only = show_only,
      modified_since = modified_since,
      max_age = max_age,
      nwlng = west,
      nwlat = north,
      selng = east,
      selat = south,
      baseUrl = baseUrl
    )

  # > str(PAList)
  # List of 11
  # $ api_version             : chr "V1.0.11-0.0.42"
  # $ time_stamp              : POSIXct[1:1], format: "2023-02-18 00:19:22"
  # $ data_time_stamp         : POSIXct[1:1], format: "2023-02-18 00:19:00"
  # $ location_type           : int 0
  # $ max_age                 : int 86400
  # $ firmware_default_version: chr "7.02"
  # $ fields                  : chr [1:49] "sensor_index" "last_modified" "date_created" "last_seen" ...
  # $ location_types          : chr [1:2] "outside" "inside"
  # $ channel_states          : chr [1:4] "No PM" "PM-A" "PM-B" "PM-A+PM-B"
  # $ channel_flags           : chr [1:4] "Normal" "A-Downgraded" "B-Downgraded" "A+B-Downgraded"
  # $ data                    : tibble [2,000 × 49] (S3: tbl_df/tbl/data.frame)

  # ----- Return ---------------------------------------------------------------

  return(PAList$data)

}



# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  api_key = PurpleAir_API_READ_KEY
  fields = PurpleAir_DATA_AVG_PM25_FIELDS
  location_type = 0
  modified_since = NULL
  max_age = 3600 * 24
  west = -125
  east = -117
  south = 42
  north = 49
  baseUrl = "https://api.purpleair.com/v1/sensors"

  pas_raw <-
    pas_downloadParseRawData(
      api_key,
      fields,
      location_type,
      modified_since,
      max_age,
      west,
      east,
      south,
      north,
      baseUrl = "https://api.purpleair.com/v1/sensors"
    )

}
