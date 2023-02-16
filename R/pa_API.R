#' @export
#'
#' @title Checks the validity and returns the key type for the provided \code{api_key}.
#'
#' @param api_key PurpleAir API key.
#' @param baseUrl URL endpoint for the "Check Key" API.
#'
#' @return List containing key type information.
#'
#' @description Sends a request to the PurpleAirAPI API endpoint described at:
#' \url{https://api.purpleair.com/#api-keys-check-api-key}
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(AirSensor2)
#'
#'   pa_checkAPIKey(
#'     api_key = MY_API_READ_KEY
#'   )
#'
#' }, silent = FALSE)
#' }

pa_checkAPIKey <- function(
    api_key = NULL,
    baseUrl = "https://api.purpleair.com/v1/keys"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(baseUrl)

  # ----- Request data ---------------------------------------------------------

  # Strip off any final "/"
  baseUrl <- stringr::str_replace(baseUrl, "/$", "")

  # See: https://api.purpleair.com/#api-keys-check-api-key
  webserviceUrl <- baseUrl

  queryList <-
    list(
    )

  PAList <- PurpleAir_API_Request(
    webserviceUrl = webserviceUrl,
    api_key = api_key,
    queryList = queryList
  )

  return(PAList)

}


#' @export
#'
#' @title Retrieves a list of all members of the specified group.
#'
#' @param api_key PurpleAir API READ key.
#' @param group_id The \code{group_id} of the requested group. This group must
#' be owned by the \code{api_key}.
#' @param baseUrl URL endpoint for the "Get Groups list" API.
#'
#' @return List containing all members of the specified group.
#'
#' @description Sends a request to the PurpleAirAPI API endpoint described at:
#' \url{https://api.purpleair.com/#api-groups-get-group-detail}
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(AirSensor2)
#'
#'   pa_getGroupDetail(
#'     api_key = MY_API_READ_KEY,
#'     group_id = MY_GROUP_ID
#'   )
#'
#' }, silent = FALSE)
#' }

pa_getGroupDetail <- function(
  api_key = NULL,
  group_id = NULL,
  baseUrl = "https://api.purpleair.com/v1/groups"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(group_id)
  MazamaCoreUtils::stopIfNull(baseUrl)

  # ----- Request data ---------------------------------------------------------

  # Strip off any final "/"
  baseUrl <- stringr::str_replace(baseUrl, "/$", "")

  # See: https://api.purpleair.com/#api-groups-get-group-detail
  webserviceUrl <- sprintf("%s/%s", baseUrl, group_id)

  queryList <-
    list(
    )

  PAList <- PurpleAir_API_Request(
    webserviceUrl = webserviceUrl,
    api_key = api_key,
    queryList = queryList
  )

  return(PAList)

}


#' @export
#'
#' @title Retrieves a list of all groups owned by the provided \code{api_key}.
#'
#' @param api_key PurpleAir API READ ey.
#' @param baseUrl URL endpoint for the "get groups list" API.
#'
#' @return List containing all gropus owned by \code{api_key}.
#'
#' @description Sends a request to the PurpleAirAPI API endpoint described at:
#' \url{https://api.purpleair.com/#api-groups-get-groups-list}
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(AirSensor2)
#'
#'   pa_getGroupsList(
#'     api_key = MY_API_READ_KEY
#'   )
#'
#' }, silent = FALSE)
#' }

pa_getGroupsList <- function(
    api_key = NULL,
    baseUrl = "https://api.purpleair.com/v1/groups"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(baseUrl)

  # ----- Request data ---------------------------------------------------------

  # Strip off any final "/"
  baseUrl <- stringr::str_replace(baseUrl, "/$", "")

  # See: https://api.purpleair.com/#api-groups-get-groups-list
  webserviceUrl <- baseUrl

  queryList <-
    list(
    )

  PAList <- PurpleAir_API_Request(
    webserviceUrl = webserviceUrl,
    api_key = api_key,
    queryList = queryList
  )

  return(PAList)

}


#' @export
#'
#' @title Retrieves a list of all sensors in the specified group.
#'
#' @param api_key PurpleAir API READ key.
#' @param group_id The \code{group_id} of the requested group. This group must
#' be owned by the \code{api_key}.
#' @param fields Comma-separated list of 'sensor data fields' to include in the response.
#' @param max_age Filter results to only include sensors modified or updated
#' within the last \code{max_age} seconds. Using a value of 0 will match sensors of any age.
#' @param baseUrl URL endpoint for the "Get Members Data" API.
#'
#' @return List containing all sensors in the specified group.
#'
#' @description Sends a request to the PurpleAirAPI API endpoint described at:
#' \url{https://api.purpleair.com/#api-groups-get-members-data}
#'
#' Retrieves data for all sensors in the specified group.
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(AirSensor2)
#'
#'   pa_getMembersData(
#'     api_key = MY_API_READ_KEY,
#'     group_id = MY_GROUP_ID
#'   )
#'
#' }, silent = FALSE)
#' }

pa_getMembersData <- function(
  api_key = NULL,
  group_id = NULL,
  fields = pas_PM25_FIELDS,
  max_age = 604800,
  baseUrl = "https://api.purpleair.com/v1/groups"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(group_id)
  MazamaCoreUtils::stopIfNull(fields)
  MazamaCoreUtils::stopIfNull(max_age)
  MazamaCoreUtils::stopIfNull(baseUrl)

  # ----- Request data ---------------------------------------------------------

  # Strip off any final "/"
  baseUrl <- stringr::str_replace(baseUrl, "/$", "")

  # See: https://api.purpleair.com/#api-groups-get-members-data
  webserviceUrl <- sprintf("%s/%s/members", baseUrl, group_id)

  queryList <-
    list(
      fields = fields,
      max_age = max_age
    )

  PAList <- PurpleAir_API_Request(
    webserviceUrl = webserviceUrl,
    api_key = api_key,
    queryList = queryList
  )

  # ----- Fix returned data ----------------------------------------------------

  colnames(PAList$data) <- PAList$fields
  tbl <- dplyr::as_tibble(PAList$data)

  # Convert from character to numeric
  for ( name in names(tbl) ) {
    if ( name %in% PurpleAir_Numeric_Fields ) {
      tbl[[name]] <- as.numeric(tbl[[name]])
    } else if ( name %in% PurpleAir_POSIXct_Fields ) {
      tbl[[name]] <- lubridate::as_datetime(as.numeric(tbl[[name]]))
    }
  }

  PAList$data <- tbl

  return(PAList)

}


# ===== Private Functions ======================================================


#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.error logger.debug logger.isInitialized
#'

PurpleAir_API_Request <- function(
    webserviceUrl = NULL,
    api_key = NULL,
    queryList = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(webserviceUrl)
  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(queryList)

  # ----- Request data ---------------------------------------------------------

  # NOTE:  https://httr.r-lib.org/articles/quickstart.html
  r <-
    httr::GET(
      webserviceUrl,
      httr::add_headers("X-API-Key" = api_key),
      query = queryList
    )

  # * Error response -----

  if ( httr::http_error(r) ) {  # web service failed to respond

    content <- httr::content(r)

    err_msg <- sprintf(
      "%s - %s",
      content$error,
      content$description
    )

    if ( logger.isInitialized() ) {
      logger.error("Web service failed to respond: %s", webserviceUrl)
      logger.error(err_msg)
    }

    stop(err_msg)

  }

  # * Success response -----

  content <- httr::content(r, as = "text", encoding = "UTF-8") # don't interpret

  # ----- Parse JSON -----------------------------------------------------------

  # * Convert JSON to an R list -----

  PAList <-
    jsonlite::fromJSON(
      content,
      simplifyVector = TRUE,
      simplifyDataFrame = TRUE,
      simplifyMatrix = TRUE,
      flatten = FALSE
    )

  # * Convert times to POSIXct -----

  if ( "time_stamp" %in% names(PAList) ) {
    PAList$time_stamp <- lubridate::as_datetime(PAList$time_stamp)
  }

  if ( "data_time_stamp" %in% names(PAList) ) {
    PAList$data_time_stamp <- lubridate::as_datetime(PAList$data_time_stamp)
  }

  # returned dataframes with a "created" columns
  for ( name in names(PAList) ) {
    if ( is.data.frame(PAList[[name]]) ) {
      if ( "created" %in% names(PAList[[name]]) ) {
        PAList[[name]]$created <- lubridate::as_datetime(PAList[[name]]$created)
      }
    }
  }

  return(PAList)

}


PurpleAir_Numeric_Fields <- c(
  # Station information and status fields:
  #"name",
  #"icon",
  #"model",
  #"hardware",
  #"location_type",
  #"private",
  "latitude",
  "longitude",
  "altitude",
  #"position_rating",
  "led_brightness",
  #"firmware_version",
  #"firmware_upgrade",
  "rssi",
  "uptime",
  "pa_latency",
  "memory",
  #"last_seen",
  #"last_modified",
  #"date_created",
  #"channel_state",
  #"channel_flags",
  #"channel_flags_manual",
  #"channel_flags_auto",
  "confidence",
  "confidence_manual",
  "confidence_auto",

  # Environmental fields:
  "humidity",
  "humidity_a",
  "humidity_b",
  "temperature",
  "temperature_a",
  "temperature_b",
  "pressure",
  "pressure_a",
  "pressure_b",

  # Miscellaneous fields:
  "voc",
  "voc_a",
  "voc_b",
  "ozone1",
  "analog_input",

  # PM1.0 fields:
  "pm1.0",
  "pm1.0_a",
  "pm1.0_b",
  "pm1.0_atm",
  "pm1.0_atm_a",
  "pm1.0_atm_b",
  "pm1.0_cf_1",
  "pm1.0_cf_1_a",
  "pm1.0_cf_1_b",

  # PM2.5 fields:
  "pm2.5_alt",
  "pm2.5_alt_a",
  "pm2.5_alt_b",
  "pm2.5",
  "pm2.5_a",
  "pm2.5_b",
  "pm2.5_atm",
  "pm2.5_atm_a",
  "pm2.5_atm_b",
  "pm2.5_cf_1",
  "pm2.5_cf_1_a",
  "pm2.5_cf_1_b",

  # PM2.5 pseudo (simple running) average fields:
  "pm2.5_10minute",
  "pm2.5_10minute_a",
  "pm2.5_10minute_b",
  "pm2.5_30minute",
  "pm2.5_30minute_a",
  "pm2.5_30minute_b",
  "pm2.5_60minute",
  "pm2.5_60minute_a",
  "pm2.5_60minute_b",
  "pm2.5_6hour",
  "pm2.5_6hour_a",
  "pm2.5_6hour_b",
  "pm2.5_24hour",
  "pm2.5_24hour_a",
  "pm2.5_24hour_b",
  "pm2.5_1week",
  "pm2.5_1week_a",
  "pm2.5_1week_b",

  # PM10.0 fields:
  "pm10.0",
  "pm10.0_a",
  "pm10.0_b",
  "pm10.0_atm",
  "pm10.0_atm_a",
  "pm10.0_atm_b",
  "pm10.0_cf_1",
  "pm10.0_cf_1_a",
  "pm10.0_cf_1_b",

  # Visibility fields:
  "scattering_coefficient",
  "scattering_coefficient_a",
  "scattering_coefficient_b",
  "deciviews",
  "deciviews_a",
  "deciviews_b",
  "visual_range",
  "visual_range_a",
  "visual_range_b",

  # Particle count fields:
  "0.3_um_count",
  "0.3_um_count_a",
  "0.3_um_count_b",
  "0.5_um_count",
  "0.5_um_count_a",
  "0.5_um_count_b",
  "1.0_um_count",
  "1.0_um_count_a",
  "1.0_um_count_b",
  "2.5_um_count",
  "2.5_um_count_a",
  "2.5_um_count_b",
  "5.0_um_count",
  "5.0_um_count_a",
  "5.0_um_count_b",
  "10.0_um_count",
  "10.0_um_count_a",
  "10.0_um_count_b"

  # ThingSpeak fields, used to retrieve data from api.thingspeak.com:
  #"primary_id_a",
  #"secondary_id_a",
  #"secondary_key_a",
  #"primary_id_b",
  #"primary_key_b",
  #"secondary_id_b",
  #"secondary_key_b"
)


PurpleAir_POSIXct_Fields <- c(
  "last_seen",
  "last_modified",
  "date_created"
)


