#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.error logger.debug logger.isInitialized
#' @importFrom MazamaCoreUtils getAPIKey
#'
#' @title Download synoptic data from PurpleAir
#'
#' @param apiReadKey PurpleAir API Read Key. If \code{apiReadKey = NULL}, it
#' will be obtained using \code{getAPIKey("PurpleAir-read")}.
#' See \code{MazamaCoreUtils::\link[MazamaCoreUtils:setAPIKey]{setAPIKey}}.
#' @param fields Character string with PurpleAir field names for the Get Sensor Data API.
#' @param maxAge Number of seconds used to filter results to only include sensors
#' modified or updated within the \code{maxAge} seconds. Using a value of 0 will match all sensors.
#' @param outsideOnly Logical specifying whether to restrict requests to outside sensors only.
#' @param west Longitude of the western edge of the bounding box in which to find sensors.
#' @param east Longitude of the eastern edge of the bounding box in which to find sensors.
#' @param south Latitude of the southern edge of the bounding box in which to find sensors.
#' @param north Latitude of the northern edge of the bounding box in which to find sensors.
#' @param baseUrl Base URL for the PurpleAir API.
#'
#' @return Dataframe of synoptic PurpleAir data.
#'
#' @description Download and parse synoptic data for PurpleAir within the
#' specified region.
#'
#' The synoptic data provides access to data from many PurpleAir sensors at
#' a moment in time and includes both metadata and recent PM2.5 averages for
#' each sensor.
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
#' pas_raw <-
#'   pas_downloadParseRawData(
#'     apiReadKey = MY_API_READ_KEY,
#'     fields = pas_PM25_FIELDS,
#'     maxAge = 3600 * 24,
#'     outsideOnly = TRUE,
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
  apiReadKey = NULL,
  fields = pas_PM25_FIELDS,
  maxAge = 3600 * 24 * 7,
  outsideOnly = TRUE,
  west = NULL,
  east = NULL,
  south = NULL,
  north = NULL,
  baseUrl = "https://api.purpleair.com/v1/sensors"
) {

  # ----- Validate parameters --------------------------------------------------

  if ( is.null(apiReadKey) )
    apiReadKey <- MazamaCoreUtils::getAPIKey("PurpleAir-read")

  MazamaCoreUtils::stopIfNull(apiReadKey)
  MazamaCoreUtils::stopIfNull(fields)
  maxAge <- MazamaCoreUtils::setIfNull(maxAge, 3600 * 7 * 24)
  MazamaCoreUtils::stopIfNull(outsideOnly)
  MazamaCoreUtils::stopIfNull(west)
  MazamaCoreUtils::stopIfNull(east)
  MazamaCoreUtils::stopIfNull(south)
  MazamaCoreUtils::stopIfNull(north)
  MazamaCoreUtils::stopIfNull(baseUrl)

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

  # ----- Request data ---------------------------------------------------------

  # Strip off any final "/"
  baseUrl <- stringr::str_replace(baseUrl, "/$", "")

  # Placeholder in case things get more complicated
  webserviceUrl <- baseUrl

  queryList <-
    list(
      fields = fields,
      nwlng = west,
      nwlat = north,
      selng = east,
      selat = south
    )

  if ( outsideOnly ) {
    queryList$location_type <- 0
  }

  # NOTE:  https://httr.r-lib.org/articles/quickstart.html
  r <-
    httr::GET(
      webserviceUrl,
      httr::add_headers("X-API-Key" = apiReadKey),
      query = queryList
    )

  # * Error response -----

  if ( httr::http_error(r) ) {  # web service failed to respond

    status_code <- httr::status_code(r)

    err_msg <- sprintf(
      "web service error %s from:\n  %s\n\n TODO",
      status_code,
      webserviceUrl
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

  if ( length(PAList$data) == 0 ) {
    stop("no data were returned")
  }

  # > str(PAList)
  # List of 11
  # $ api_version             : chr "V1.0.10-0.0.17"
  # $ time_stamp              : int 1650933831
  # $ data_time_stamp         : int 1650933778
  # $ location_type           : int 0
  # $ max_age                 : int 604800
  # $ firmware_default_version: chr "6.01"
  # $ fields                  : chr [1:45] "sensor_index" "name" "icon" "model" ...
  # $ location_types          : chr [1:2] "outside" "inside"
  # $ channel_states          : chr [1:4] "No PM" "PM-A" "PM-B" "PM-A+PM-B"
  # $ channel_flags           : chr [1:4] "Normal" "A-Downgraded" "B-Downgraded" "A+B-Downgraded"
  # $ data                    : chr [1:958, 1:45] "131707" "896" "912" "920" ...

  if ( logger.isInitialized() ) {
    logger.debug("api_version = \"%s\"", PAList$api_version)
    logger.debug("time_stamp = \"%s\"", as.POSIXct(PAList$time_stamp, tz = "UTC", origin = lubridate::origin))
    logger.debug("data_time_stamp = \"%s\"", as.POSIXct(PAList$data_time_stamp, tz = "UTC", origin = lubridate::origin))
    logger.debug("max_age = \"%s\"", PAList$max_age) # seconds
    logger.debug("firmware_default_version = \"%s\"", PAList$firmware_default_version)
  }


  # Pull out the tibble of results
  colnames(PAList$data) <- PAList$fields
  tbl <- dplyr::as_tibble(PAList$data)

  # NOTE:  This was generated on 2022-04-25

  # > dplyr::glimpse(tbl, width = 75)
  # Rows: 958
  # Columns: 45
  # $ sensor_index         <chr> "131707", "896", "912", "920", "924", "928",…
  # $ name                 <chr> "Havillah", "Chemainus Elementary", "The Hub…
  # $ icon                 <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0",…
  # $ model                <chr> "PA-II-SD", "PA-II", "PA-II", "PA-II", "PA-I…
  # $ hardware             <chr> "2.0+OPENLOG+31037 MB+DS3231+BME280+PMSX003-…
  # $ location_type        <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0",…
  # $ private              <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0",…
  # $ latitude             <chr> "48.819916", "48.930725", "48.72967", "48.79…
  # $ longitude            <chr> "-119.184746", "-123.73338", "-123.66412", "…
  # $ altitude             <chr> "3650", "160", "185", "190", "215", "386", "…
  # $ position_rating      <chr> "0", "5", "5", "5", "5", "5", "5", "5", "0",…
  # $ led_brightness       <chr> "15", "15", "15", "15", "15", "15", "15", "1…
  # $ firmware_version     <chr> "6.01", "6.01", "3.00", "6.01", "6.01", "6.0…
  # $ firmware_upgrade     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
  # $ rssi                 <chr> "-69", "-67", "-90", "-76", "-60", "-72", "-…
  # $ uptime               <chr> "5769", "30723", "89", "42561", "13208", "92…
  # $ pa_latency           <chr> "379", "226", NA, "281", "208", "222", "242"…
  # $ memory               <chr> "16032", "15088", "29272", "14928", "15424",…
  # $ last_seen            <chr> "1650933807", "1650933854", "1650931185", "1…
  # $ last_modified        <chr> "1645469408", "1506718153", "1648969388", "1…
  # $ date_created         <chr> "1633552393", "1484435197", "1484454581", "1…
  # $ channel_state        <chr> "3", "3", "3", "3", "3", "3", "3", "3", "3",…
  # $ channel_flags        <chr> "0", "0", "0", "0", "0", "0", "0", "0", "2",…
  # $ channel_flags_manual <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0",…
  # $ channel_flags_auto   <chr> "0", "0", "0", "0", "0", "0", "0", "0", "2",…
  # $ confidence           <chr> "100", "96", "100", "100", "100", "100", "10…
  # $ confidence_manual    <chr> "100", "96", "100", "100", "100", "100", "10…
  # $ confidence_auto      <chr> "100", "96", "100", "100", "100", "100", "10…
  # $ humidity             <chr> "31", "31", "31", "30", "27", "38", "44", "3…
  # $ temperature          <chr> "61", "69", "74", "69", "71", "62", "58", "6…
  # $ pressure             <chr> "887", "1012", "1011.5", "1012.1", "1011.2",…
  # $ pm2.5_10minute       <chr> "1.4", "0.5", "0.9", "1.4", "0.4", "1.2", "1…
  # $ pm2.5_30minute       <chr> "1", "0.5", "1.4", "1.1", "0.2", "0.8", "1.2…
  # $ pm2.5_60minute       <chr> "1.1", "0.6", "2.7", "0.9", "0.3", "0.6", "1…
  # $ pm2.5_6hour          <chr> "2.1", "2", "6.3", "2.6", "1.9", "2.4", "1.7…
  # $ pm2.5_24hour         <chr> "1.2", "4", "7.2", "4.4", "2.9", "3.8", "1.9…
  # $ pm2.5_1week          <chr> "2.8", "5.2", "7.6", "5.2", "5.2", "4.3", "1…

  # ----- Return ---------------------------------------------------------------

  return(tbl)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  apiReadKey = MY_API_READ_KEY
  maxAge = 3600 * 24
  outsideOnly = TRUE
  west = -125
  east = -117
  south = 42
  north = 49
  baseUrl = "https://api.purpleair.com/v1/sensors"

  pas_raw <-
    pas_downloadParseRawData(
      apiReadKey,
      maxAge,
      outsideOnly,
      west,
      east,
      south,
      north,
      baseUrl = "https://api.purpleair.com/v1/sensors"
    )





}
