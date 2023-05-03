#
# Wrapper functions for all API endpoints described at:
#   https://api-guide.clarity.io
#


#' @export
#'
#' @title Retrieve current hourly data for all Open Data sensors.
#'
#' @param api_key Clarity API READ key.
#' @param fields Optional parameter specifying sensor data fields to return.
#' @param baseUrl URL endpoint for "All open datasources" returning "Hourly
#' values for last 3 hours" as "1-Hour Mean Nowcast".
#'
#' @return List containing all recent data for a single sensor.
#'
#' @description Sends a request to the Clarity API endpoint for Open Data.
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(AirSensor2)
#'
#'   Clarity_getAllOpenHourly(
#'     api_key = Clarity_API_READ_KEY,
#'     format = "USFS"
#'   )
#'
#' }, silent = FALSE)
#' }

Clarity_getAllOpenHourly <- function(
    api_key = NULL,
    format = c("USFS"),
    baseUrl = "https://clarity-data-api.clarity.io/v1/open/all-recent-measurement/pm25/hourly"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(baseUrl)

  format <- match.arg(format)

  # ----- Request data ---------------------------------------------------------

  # Strip off any final "/"
  baseUrl <- stringr::str_replace(baseUrl, "/$", "")

  # See: https://api-guide.clarity.io/v1/datasources/
  # See: https://docs.google.com/document/d/1b2wvFpbSzCRsjThHASMBOrGON5jIDE7BnIvrL_BrTPE/edit#heading=h.6edc0091kl5u
  webserviceUrl <- sprintf("%s", baseUrl)

  if ( is.null(format) ) {
    queryList <- list()
  } else {
    queryList <-
      list(
        format = format
      )
  }

  responseList <- Clarity_API_GET(
    webserviceUrl = webserviceUrl,
    api_key = api_key,
    queryList = queryList
  )

  # ----- Restructure returned data --------------------------------------------

  # > class(responseList)
  # [1] "data.frame"
  # > dplyr::glimpse(responseList, width = 75)
  # Rows: 605
  # Columns: 4
  # $ datasourceId <chr> "DAABL1560", "DAAZI7074", "DADKD2421", "DAENX0980", …
  # $ lat          <dbl> 34.07283, 37.06706, 42.82760, 34.03556, 43.17658, 37…
  # $ lon          <dbl> -118.20581, -122.05722, 74.58188, -118.36449, 76.897…
  # $ data         <list> <"2023-05-02T23Z", "2023-05-02T22Z", "2023-05-02T21…

  meta <-
    dplyr::as_tibble(responseList[,1:3]) %>%
    dplyr::rename(
      deviceID = "datasourceId",
      longitude = "lon",
      latitude = "lat"
    )

  # TODO:

  return(responseList)

}



# ===== Private Functions ======================================================


# GET and parse a JSON return

Clarity_API_GET <- function(
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
      httr::add_headers("x-api-key" = api_key),
      query = queryList
    )

  # * Error response -----

  if ( httr::http_error(r) ) {  # web service failed to respond

    content <- httr::content(r)

    err_msg <- sprintf(
      "%s - %s",
      content$Code,
      content$Message
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

  responseList <-
    jsonlite::fromJSON(
      content,
      simplifyVector = TRUE,
      simplifyDataFrame = TRUE,
      simplifyMatrix = TRUE,
      flatten = FALSE
    )

  return(responseList)

}


