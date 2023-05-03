#
# Wrapper functions for all API endpoints described at:
#   https://api-guide.clarity.io
#

# ===== All Open Data ==========================================================

#' @export
#'
#' @title Retrieve current hourly data for all Open Data sensors.
#'
#' @param api_key Clarity API READ key.
#' @param format Customized output format (currently only "USFS").
#' @param baseUrl URL endpoint.
#'
#' @return List containing four data frames: \code{meta}, \code{QCFlag},
#' \code{pm25} and \code{nowcast}.
#'
#' @description Sends a request to the Clarity API endpoint for Open Data.
#'
#' \itemize{
#'   \item{\emph{Measurement data from} -- All open datasources}
#'   \item{\emph{Measurements returned} -- Hourly values for last 3 hours}
#'   \item{\emph{PM2.5 Mass Concentration} -- 1-Hour Mean Nowcast}
#' }
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

  responseDF <- Clarity_API_GET(
    webserviceUrl = webserviceUrl,
    api_key = api_key,
    queryList = queryList
  )

  # ----- Restructure returned data --------------------------------------------

  # > class(responseDF)
  # [1] "data.frame"
  # > dplyr::glimpse(responseDF, width = 75)
  # Rows: 605
  # Columns: 4
  # $ datasourceId <chr> "DAABL1560", "DAAZI7074", "DADKD2421", "DAENX0980", …
  # $ lat          <dbl> 34.07283, 37.06706, 42.82760, 34.03556, 43.17658, 37…
  # $ lon          <dbl> -118.20581, -122.05722, 74.58188, -118.36449, 76.897…
  # $ data         <list> <"2023-05-02T23Z", "2023-05-02T22Z", "2023-05-02T21…
  # > class(responseDF$data[1])
  # [1] "list"
  # > length(responseDF$data[1])
  # [1] 1
  # > class(responseDF$data[1][[1]])
  # [1] "matrix" "array"
  # > dim(responseDF$data[1][[1]])
  # [1] 3 4
  # > responseDF$data[1][[1]]
  #      [,1]             [,2] [,3]   [,4]
  # [1,] "2023-05-03T02Z" "1"  "3.21" "3.51"
  # [2,] "2023-05-03T01Z" "1"  "3.44" "3.71"
  # [3,] "2023-05-03T00Z" "1"  "3.45" "3.83"

  # ----- * meta -----
  meta <-
    dplyr::as_tibble(responseDF[,1:3]) %>%
    dplyr::rename(
      longitude = "lon",
      latitude = "lat"
    )

  # All open datasources, hourly values
  # GET /v1/open/all-recent-measurement/pm25/hourly ? format=USFS
  # returns a list of the following example object
  # {
  #   "datasourceId": "DABCX1234",
  #   "lat": 42.194576
  #   "lon": -122.709480
  #   "data": [
  #     ["2023-03-07T14Z", 1, 14.02, 14.43],
  #     ["2023-03-07T13Z", 1, 13.97, 12.78],
  #     ["2023-03-07T12Z", 1, 11.02, 12.09]
  #   ]
  # }
  #
  # Notes
  # Each row of data has the format  [ start of hour (UTC),  QC flag,  1-Hour-Mean,  Nowcast ]
  # Time portion omits minute:second
  # Sorted descending in time

  # ----- * various data -----

  DFList <- list()

  for ( i in seq_len(nrow(responseDF)) ) {

    datasourceId <- responseDF$datasourceId[i]

    matrix <- responseDF$data[i][[1]]
    colnames(matrix) <- c("timestamp", "QCFlag", "pm25", "nowcast")

    # NOTE: Each dataframe will have three hourly records with QCFlag, pm25, nowcast and datasourceId
    DFList[[datasourceId]] <-
      dplyr::as_tibble(matrix) %>%
      dplyr::mutate(datasourceId = !!datasourceId)

  }

  tidyDF <-
    dplyr::bind_rows(DFList) %>%
    dplyr::mutate(
      datetime = MazamaCoreUtils::parseDatetime(.data$timestamp, timezone = "UTC"),
      QCFlag = as.numeric(.data$QCFlag),
      pm25 = as.numeric(.data$pm25),
      nowcast = as.numeric(.data$nowcast)
    )

  # > dplyr::glimpse(tidyDF, width = 75)
  # Rows: 1,811
  # Columns: 5
  # $ timestamp     <dttm> 2023-05-03 02:00:00, 2023-05-03 01:00:00, 2023-05-03 0…
  # $ QCFlag        <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
  # $ pm25          <dbl> 3.21, 3.44, 3.45, 2.88, 3.22, 2.98, 15.50, 14.31, 20.10…
  # $ nowcast       <dbl> 3.51, 3.71, 3.83, 3.00, 3.09, 3.01, 15.52, 15.54, 16.66…
  # $ datasourceId  <chr> "DAABL1560", "DAABL1560", "DAABL1560", "DAAZI7074", "DA…

  QC <-
    tidyDF %>%
    dplyr::select(dplyr::all_of(c("datetime", "QCFlag", "datasourceId"))) %>%
    tidyr::pivot_wider(
      names_from = "datasourceId",
      values_from = "QCFlag"
    ) %>%
    dplyr::arrange(.data$datetime)

  pm25 <-
    tidyDF %>%
    dplyr::select(dplyr::all_of(c("datetime", "pm25", "datasourceId"))) %>%
    tidyr::pivot_wider(
      names_from = "datasourceId",
      values_from = "pm25"
    ) %>%
    dplyr::arrange(.data$datetime)

  nowcast <-
    tidyDF %>%
    dplyr::select(dplyr::all_of(c("datetime", "nowcast", "datasourceId"))) %>%
    tidyr::pivot_wider(
      names_from = "datasourceId",
      values_from = "nowcast"
    ) %>%
    dplyr::arrange(.data$datetime)

  # ----- Return ---------------------------------------------------------------

  # NOTE:  The 'meta' dataframe is the $meta part of an 'mts' object
  # NOTE:  Other dataframes are each the $data part of an 'mts' object.

  returnList <- list(
    meta = meta,
    QC = QC,
    pm25 = pm25,
    nowcast = nowcast
  )

  return(returnList)

}

#' @export
#'
#' @title Retrieve current individual records for all Open Data sensors.
#'
#' @param api_key Clarity API READ key.
#' @param format Customized output format (currently only "USFS").
#' @param baseUrl URL endpoint.
#'
#' @return List containing four data frames: \code{meta}, \code{QCFlag},
#' \code{pm25} and \code{nowcast}.
#'
#' @description Sends a request to the Clarity API endpoint for Open Data.
#'
#' \itemize{
#'   \item{\emph{Measurement data from} -- All open datasources}
#'   \item{\emph{Measurements returned} -- Individual values for last complete
#'   hour (hour-aligned) plus fraction of current hour}
#'   \item{\emph{PM2.5 Mass Concentration} -- Individual sample}
#' }
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(AirSensor2)
#'
#'   Clarity_getAllIndividualOpen(
#'     api_key = Clarity_API_READ_KEY,
#'     format = "USFS"
#'   )
#'
#' }, silent = FALSE)
#' }

Clarity_getAllOpenIndvidual <- function(
    api_key = NULL,
    format = c("USFS"),
    baseUrl = "https://clarity-data-api.clarity.io/v1/open/all-recent-measurement/pm25/individual"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(baseUrl)

  format <- match.arg(format)

  # ----- Request data ---------------------------------------------------------

  # Strip off any final "/"
  baseUrl <- stringr::str_replace(baseUrl, "/$", "")

  # See: https://api-guide.clarity.io/v1/datasources/
  # See: https://docs.google.com/document/d/1b2wvFpbSzCRsjThHASMBOrGON5jIDE7BnIvrL_BrTPE/edit#heading=h.s15gjadxf1eh
  webserviceUrl <- sprintf("%s", baseUrl)

  if ( is.null(format) ) {
    queryList <- list()
  } else {
    queryList <-
      list(
        format = format
      )
  }

  responseDF <- Clarity_API_GET(
    webserviceUrl = webserviceUrl,
    api_key = api_key,
    queryList = queryList
  )

  # ----- Restructure returned data --------------------------------------------

  # ... all the same except the data
  #
  # > dim(responseDF$data[1][[1]])
  # [1] 17  3
  # > head(responseDF$data[1][[1]])
  # [,1]                   [,2] [,3]
  # [1,] "2023-05-03T11:33:46Z" "1"  "4.51"
  # [2,] "2023-05-03T11:28:06Z" "1"  "4.14"
  # [3,] "2023-05-03T11:22:27Z" "1"  "4.24"
  # [4,] "2023-05-03T11:16:47Z" "1"  "4.56"
  # [5,] "2023-05-03T11:11:08Z" "1"  "3.63"
  # [6,] "2023-05-03T11:05:29Z" "1"  "3.43"


  # GET /v1/open/all-recent-measurement/pm25/individual ? format=USFS
  # returns a list of the following example object
  # {
  #   "datasourceId": "DABCX1234",
  #   "lat": 42.194576
  #   "lon": -122.709480
  #   "data": [
  #     ["2023-03-07T14:07:03Z", 1, 14.11],
  #     ["2023-03-07T13:49:11Z", 1, 13.87],
  #     ["2023-03-07T13:31:43Z", 1, 11.54],
  #     ["2023-03-07T13:13:07Z", 1, 11.32]
  #   ]
  # }
  #
  # Notes
  # Each row of data has the format  [ measurement time (UTC),  QC flag,  Mass Concentration ]
  # Sorted descending in time

  returnList <- list()

  for ( i in seq_len(nrow(responseDF)) ) {

    datasourceId <- responseDF$datasourceId[i]
    longitude <- responseDF$lon[i]
    latitude <- responseDF$lat[i]

    matrix <- responseDF$data[i][[1]]
    colnames(matrix) <- c("timestamp", "QCFlag", "pm25")

    # NOTE: Each dataframe will an hours worth of raw records with QCFlag, pm25 and datasourceId
    returnList[[datasourceId]] <-
      dplyr::as_tibble(matrix) %>%
      dplyr::mutate(
        datetime = MazamaCoreUtils::parseDatetime(.data$timestamp, timezone = "UTC"),
        QCFlag = as.numeric(.data$QCFlag),
        pm25 = as.numeric(.data$pm25),
        datasourceId = !!datasourceId,
        longitude = !!longitude,
        latitude = !!latitude
      ) %>%
      dplyr::select(-"timestamp")

  }

  # ----- Return ---------------------------------------------------------------

  # NOTE:  The returnList will contain per sensor-deployment 'sts' objects

  return(returnList)

}

# ===== Single Source Open Data ================================================

#' @export
#'
#' @title Retrieve current hourly data from a single source.
#'
#' @param api_key Clarity API READ key.
#' @param datasourceId Clarity sensor identifier.
#' @param format Customized output format (currently only "USFS").
#' @param baseUrl URL endpoint.
#'
#' @return List containing four data frames: \code{meta}, \code{QCFlag},
#' \code{pm25} and \code{nowcast}.
#'
#' @description Sends a request to the Clarity API endpoint for Open Data.
#'
#' \itemize{
#'   \item{\emph{Measurement data from} -- Drill down on one open datasource}
#'   \item{\emph{Measurements returned} -- Hourly values  for last 10 days}
#'   \item{\emph{PM2.5 Mass Concentration} -- 1-Hour Mean Nowcast}
#' }
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(AirSensor2)
#'
#'   Clarity_getOpenHourly(
#'     api_key = Clarity_API_READ_KEY,
#'     datasourceId = "DAABL1560",
#'     format = "USFS"
#'   )
#'
#' }, silent = FALSE)
#' }

Clarity_getOpenHourly <- function(
    api_key = NULL,
    datasourceId = NULL,
    format = c("USFS"),
    baseUrl = "https://clarity-data-api.clarity.io/v1/open/datasource-measurement"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(datasourceId)
  MazamaCoreUtils::stopIfNull(baseUrl)

  format <- match.arg(format)

  # ----- Request data ---------------------------------------------------------

  # Strip off any final "/"
  baseUrl <- stringr::str_replace(baseUrl, "/$", "")

  # See: https://api-guide.clarity.io/v1/datasources/
  # See: https://docs.google.com/document/d/1b2wvFpbSzCRsjThHASMBOrGON5jIDE7BnIvrL_BrTPE/edit#heading=h.dn2am7gbxgvf
  webserviceUrl <- sprintf("%s/%s/pm25/hourly", baseUrl, datasourceId)

  if ( is.null(format) ) {
    queryList <- list()
  } else {
    queryList <-
      list(
        format = format
      )
  }

  responseDF <- Clarity_API_GET(
    webserviceUrl = webserviceUrl,
    api_key = api_key,
    queryList = queryList
  )

  # ----- Restructure returned data --------------------------------------------

  # GET /v1/open/datasource-measurement/{datasourceId}/pm25/hourly ? format=USFS
  # returns a list of the following example object
  # {
  #   "datasourceId": "DABCX1234",
  #   "name": "Mariette-Lake Intersection",
  #   "lat": 42.194576
  #   "lon": -122.709480
  #   "data": [
  #     ["2023-03-07T14Z", 1, 14.02, 14.43],
  #     ["2023-03-07T13Z", 1, 13.97, 12.78],
  #     ["2023-03-07T12Z", 1, 11.02, 12.09],
  #     ...
  #     going back 10 days
  #   ]
  # }
  #
  # Notes
  # Each row of data has the format  [ start of hour (UTC),  QC flag,  1-Hour-Mean,  Nowcast ]
  # Time portion omits minute:second
  # Sorted descending in time

  DFList <- list()

  # NOTE:  responseDF has only a single record (unless the sensor has moved)

  for ( i in seq_len(nrow(responseDF)) ) {

    datasourceId <- responseDF$datasourceId[i]
    longitude <- responseDF$lon[i]
    latitude <- responseDF$lat[i]

    matrix <- responseDF$data[i][[1]]
    colnames(matrix) <- c("timestamp", "QCFlag", "pm25", "nowcast")

    # NOTE: Each dataframe will an hours worth of raw records with QCFlag, pm25 and datasourceId
    DFList[[datasourceId]] <-
      dplyr::as_tibble(matrix) %>%
      dplyr::mutate(
        datetime = MazamaCoreUtils::parseDatetime(.data$timestamp, timezone = "UTC"),
        QCFlag = as.numeric(.data$QCFlag),
        pm25 = as.numeric(.data$pm25),
        nowcast = as.numeric(.data$nowcast),
        datasourceId = !!datasourceId,
        longitude = !!longitude,
        latitude = !!latitude
      ) %>%
      dplyr::select(-"timestamp")

  }

  # ----- Return ---------------------------------------------------------------

  returnDF <- dplyr::bind_rows(DFList)

  return(returnDF)

}

#' @export
#'
#' @title Retrieve current individual records from a single source.
#'
#' @param api_key Clarity API READ key.
#' @param datasourceId Clarity sensor identifier.
#' @param format Customized output format (currently only "USFS").
#' @param baseUrl URL endpoint.
#'
#' @return List containing four data frames: \code{meta}, \code{QCFlag},
#' \code{pm25} and \code{nowcast}.
#'
#' @description Sends a request to the Clarity API endpoint for Open Data.
#'
#' \itemize{
#'   \item{\emph{Measurement data from} -- Drill down on one open datasource}
#'   \item{\emph{Measurements returned} -- Individual values for last 3 days}
#'   \item{\emph{PM2.5 Mass Concentration} -- Individual sample}
#' }
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(AirSensor2)
#'
#'   Clarity_getOpenIndividual(
#'     api_key = Clarity_API_READ_KEY,
#'     datasourceId = "DAABL1560",
#'     format = "USFS"
#'   )
#'
#' }, silent = FALSE)
#' }

Clarity_getOpenIndividual <- function(
    api_key = NULL,
    datasourceId = NULL,
    format = c("USFS"),
    baseUrl = "https://clarity-data-api.clarity.io/v1/open/datasource-measurement"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(datasourceId)
  MazamaCoreUtils::stopIfNull(baseUrl)

  format <- match.arg(format)

  # ----- Request data ---------------------------------------------------------

  # Strip off any final "/"
  baseUrl <- stringr::str_replace(baseUrl, "/$", "")

  # See: https://api-guide.clarity.io/v1/datasources/
  # See: https://docs.google.com/document/d/1b2wvFpbSzCRsjThHASMBOrGON5jIDE7BnIvrL_BrTPE/edit#heading=h.cp7xp19pucmu
  webserviceUrl <- sprintf("%s/%s/pm25/hourly", baseUrl, datasourceId)

  if ( is.null(format) ) {
    queryList <- list()
  } else {
    queryList <-
      list(
        format = format
      )
  }

  responseDF <- Clarity_API_GET(
    webserviceUrl = webserviceUrl,
    api_key = api_key,
    queryList = queryList
  )

  # ----- Restructure returned data --------------------------------------------

  # GET /v1/open/datasource-measurement/{datasourceId}/pm25/hourly ? format=USFS
  # returns a list of the following example object
  # {
  #   "datasourceId": "DABCX1234",
  #   "name": "Mariette-Lake Intersection",
  #   "lat": 42.194576
  #   "lon": -122.709480
  #   "data": [
  #     ["2023-03-07T14Z", 1, 14.02, 14.43],
  #     ["2023-03-07T13Z", 1, 13.97, 12.78],
  #     ["2023-03-07T12Z", 1, 11.02, 12.09],
  #     ...
  #     going back 10 days
  #   ]
  # }
  #
  # Notes
  # Each row of data has the format  [ start of hour (UTC),  QC flag,  1-Hour-Mean,  Nowcast ]
  # Time portion omits minute:second
  # Sorted descending in time

  DFList <- list()

  # NOTE:  responseDF has only a single record (unless the sensor has moved)

  for ( i in seq_len(nrow(responseDF)) ) {

    datasourceId <- responseDF$datasourceId[i]
    longitude <- responseDF$lon[i]
    latitude <- responseDF$lat[i]

    matrix <- responseDF$data[i][[1]]
    colnames(matrix) <- c("timestamp", "QCFlag", "pm25", "nowcast")

    # NOTE: Each dataframe will an hours worth of raw records with QCFlag, pm25 and datasourceId
    DFList[[datasourceId]] <-
      dplyr::as_tibble(matrix) %>%
      dplyr::mutate(
        datetime = MazamaCoreUtils::parseDatetime(.data$timestamp, timezone = "UTC"),
        QCFlag = as.numeric(.data$QCFlag),
        pm25 = as.numeric(.data$pm25),
        nowcast = as.numeric(.data$nowcast),
        datasourceId = !!datasourceId,
        longitude = !!longitude,
        latitude = !!latitude
      ) %>%
      dplyr::select(-"timestamp")

  }

  # ----- Return ---------------------------------------------------------------

  returnDF <- dplyr::bind_rows(DFList)

  return(returnDF)

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

  responseDF <-
    jsonlite::fromJSON(
      content,
      simplifyVector = TRUE,
      simplifyDataFrame = TRUE,
      simplifyMatrix = TRUE,
      flatten = FALSE
    )

  return(responseDF)

}


