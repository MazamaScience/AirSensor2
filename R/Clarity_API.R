#
# Wrapper functions for all API endpoints described at:
#   https://api-guide.clarity.io
#

# ===== All Open Data ==========================================================

#' @export
#' @importFrom MazamaCoreUtils getAPIKey
#'
#' @title Retrieve current hourly data for all Open Data sensors.
#'
#' @param api_key Clarity API READ Key. If \code{api_key = NULL}, it
#' will be obtained using \code{getAPIKey("Clarity-read")}.
#' @param format Customized output format ("USFS2", "USFS").
#' @param baseUrl URL endpoint.
#'
#' @return List containing five data frames: \code{synoptic}, \code{pm2.5_QCFlag},
#' \code{pm2.5}, \code{nowcast_QCFlag} and \code{nowcast}.
#'
#' @description Sends a request to the Clarity API endpoint for Open Data.
#'
#' When \code{format = "USFS2"}, two additional fields are returned in the
#' "synoptic" dataframe: \code{calibrationId} and \code{calibrationCategory}.
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
#'     format = "USFS2"
#'   )
#'
#' }, silent = FALSE)
#' }

Clarity_getAllOpenHourly <- function(
  api_key = NULL,
  format = c("USFS2", "USFS"),
  baseUrl = "https://clarity-data-api.clarity.io/v1/open/all-recent-measurement/pm25/hourly"
) {

  # ----- Validate parameters --------------------------------------------------

  if ( is.null(api_key) )
    api_key <- MazamaCoreUtils::getAPIKey("Clarity-read")

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
  # Rows: 729
  # Columns: 6
  # $ datasourceId        <chr> "DAABL1560", "DAAUZ8818", "DAAZI7074", "DABNS…
  # $ lat                 <dbl> 34.07283, 15.00045, 37.06706, 37.71934, 34.04…
  # $ lon                 <dbl> -118.20581, 120.75050, -122.05722, -121.87298…
  # $ data                <list> <"2024-08-20T17Z", "2024-08-20T16Z", "2024-0…
  # $ calibrationId       <chr> "CAHVV2Z9CH", "CA43PUF7FW", "CAHVV2Z9CH", "CA…
  # $ calibrationCategory <int> 4, 3, 4, 3, 4, 3, 3, 2, 4, 2, 4, 4, 2, 4, 2, …
  # > class(responseDF$data[1])
  # [1] "list"
  # > length(responseDF$data[1])
  # [1] 1
  # > class(responseDF$data[1][[1]])
  # [1] "matrix" "array"
  # > dim(responseDF$data[1][[1]])
  # [1] 3 5
  # > responseDF$data[1][[1]]
  #      [,1]             [,2] [,3]    [,4] [,5]
  # [1,] "2023-06-08T17Z" "1"  "11.19" "1"  "11.52"
  # [2,] "2023-06-08T16Z" "1"  "11.22" "1"  "11.84"
  # [3,] "2023-06-08T15Z" "1"  "12.37" "1"  "12.46"
  #
  # All open datasources, hourly values
  # GET /v1/open/all-recent-measurement/pm25/hourly ? format=USFS2
  # returns a list of the following example object
  # {
  #   "datasourceId": "DABCX1234",
  #   "lat": 42.194576,
  #   "lon": -122.709480,
  #   "calibrationId": "CA123456",
  #   "calibrationCategory": 4,
  #   "data": [
  #     ["2023-03-07T14Z", 1, 14.02, 1, 14.43],
  #     ["2023-03-07T13Z", 1, 13.97, 0, 12.78],
  #     ["2023-03-07T12Z", 1, 11.02, 1, 12.09]
  #   ]
  # }
  #
  # Notes
  # Each row of data has the format  [ start of hour (UTC),  1-Hour QC flag,  1-Hour-Mean,  Nowcast QC flag, Nowcast ]
  # Time portion omits minute:second
  # Sorted descending in time

  DFList <- list()

  for ( i in seq_len(nrow(responseDF)) ) {

    datasourceId <- responseDF$datasourceId[i]
    longitude <- responseDF$lon[i]
    latitude <- responseDF$lat[i]
    # These fields are only returned by 'format=USFS2' and will be NULL if not present
    calibrationId <- responseDF$calibrationId[i]
    calibrationCategory <- responseDF$calibrationCategory[i]

    matrix <- responseDF$data[i][[1]]
    colnames(matrix) <- c("timestamp", "pm2.5_QCFlag", "pm2.5", "nowcast_QCFlag", "nowcast")

    # NOTE: Each dataframe will have three hourly records with pm2.5_QCFlag, pm2.5, nowcast_QCFlag, nowcast and datasourceId

    if ( format == "USFS" ) {

      DFList[[datasourceId]] <-
        dplyr::as_tibble(matrix) %>%
        dplyr::mutate(
          datasourceId = !!datasourceId,
          longitude = !!longitude,
          latitude = !!latitude,
          # Calibration fields
          calibrationId = as.character(NA),
          calibrationCategory = as.character(NA)
        )

    } else if ( format == "USFS2" ) {

      DFList[[datasourceId]] <-
        dplyr::as_tibble(matrix) %>%
        dplyr::mutate(
          datasourceId = !!datasourceId,
          longitude = !!longitude,
          latitude = !!latitude,
          # Calibration fields
          calibrationId = !!calibrationId,
          calibrationCategory = Clarity_assignCalibrationText(!!calibrationCategory)
        )

    }

  }

  tidyDF <-
    dplyr::bind_rows(DFList) %>%
    dplyr::mutate(
      datetime = MazamaCoreUtils::parseDatetime(.data$timestamp, timezone = "UTC"),
      pm2.5_QCFlag = as.numeric(.data$pm2.5_QCFlag),
      pm2.5 = as.numeric(.data$pm2.5),
      nowcast_QCFlag = as.numeric(.data$nowcast_QCFlag),
      nowcast = as.numeric(.data$nowcast)
    )

  # > dplyr::glimpse(tidyDF, width = 75)
  # Rows: 1,805
  # Columns: 9
  # $ timestamp      <chr> "2023-06-09T00Z", "2023-06-08T23Z", "2023-06-08T22…
  # $ pm2.5_QCFlag   <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
  # $ pm2.5          <dbl> 4.92, 5.45, 5.88, 31.37, 32.62, 45.45, 4.96, 5.03,…
  # $ nowcast_QCFlag <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
  # $ nowcast        <dbl> 5.59, 6.26, 7.07, 44.73, 58.09, 83.56, 4.67, 4.54,…
  # $ datasourceId   <chr> "DAABL1560", "DAABL1560", "DAABL1560", "DAAUZ8818"…
  # $ longitude      <dbl> -118.20581, -118.20581, -118.20581, 120.75026, 120…
  # $ latitude       <dbl> 34.07283, 34.07283, 34.07283, 15.00259, 15.00259, …
  # $ datetime       <dttm> 2023-06-09 00:00:00, 2023-06-08 23:00:00, 2023-06…

  # ----- * synoptic dataframe-----

  last_timestamp <- max(tidyDF$timestamp)

  synoptic <-
    tidyDF %>%
    dplyr::filter(.data$timestamp == !!last_timestamp)

  # ----- * various 'data' dataframes -----

  pm2.5_QC <-
    tidyDF %>%
    dplyr::select(dplyr::all_of(c("datetime", "pm2.5_QCFlag", "datasourceId"))) %>%
    tidyr::pivot_wider(
      names_from = "datasourceId",
      values_from = "pm2.5_QCFlag"
    ) %>%
    dplyr::arrange(.data$datetime)

  pm2.5 <-
    tidyDF %>%
    dplyr::select(dplyr::all_of(c("datetime", "pm2.5", "datasourceId"))) %>%
    tidyr::pivot_wider(
      names_from = "datasourceId",
      values_from = "pm2.5"
    ) %>%
    dplyr::arrange(.data$datetime)

  nowcast_QC <-
    tidyDF %>%
    dplyr::select(dplyr::all_of(c("datetime", "nowcast_QCFlag", "datasourceId"))) %>%
    tidyr::pivot_wider(
      names_from = "datasourceId",
      values_from = "nowcast_QCFlag"
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

  # NOTE:  The 'synoptic' dataframe is ready for ~_enhanceRawSynopticData().
  # NOTE:  Other 'data' dataframes are equivalent the $data part of an 'mts' object.

  returnList <- list(
    synoptic = synoptic,
    pm2.5_QC = pm2.5_QC,
    pm2.5 = pm2.5,
    nowcast_QC = nowcast_QC,
    nowcast = nowcast
  )

  return(returnList)

}

#' @export
#' @importFrom MazamaCoreUtils getAPIKey
#'
#' @title Retrieve current individual records for all Open Data sensors.
#'
#' @param api_key Clarity API READ Key. If \code{api_key = NULL}, it
#' will be obtained using \code{getAPIKey("Clarity-read")}.
#' @param format Customized output format ("USFS2", "USFS").
#' @param baseUrl URL endpoint.
#'
#' @return List containing five data frames: \code{meta}, \code{pm2.5_QCFlag},
#' \code{pm2.5}, \code{nowcast_QCFlag} and \code{nowcast}.
#'
#' @description Sends a request to the Clarity API endpoint for Open Data.
#'
#' When \code{format = "USFS2"}, two additional fields are returned in the
#' "meta" dataframe: \code{calibrationId} and \code{calibrationCategory}.
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
#'     format = "USFS2"
#'   )
#'
#' }, silent = FALSE)
#' }

Clarity_getAllOpenIndividual <- function(
  api_key = NULL,
  format = c("USFS2", "USFS"),
  baseUrl = "https://clarity-data-api.clarity.io/v1/open/all-recent-measurement/pm25/individual"
) {

  # ----- Validate parameters --------------------------------------------------

  if ( is.null(api_key) )
    api_key <- MazamaCoreUtils::getAPIKey("Clarity-read")

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


  # GET /v1/open/all-recent-measurement/pm25/individual ? format=USFS2
  # returns a list of the following example object
  # {
  #   "datasourceId": "DABCX1234",
  #   "lat": 42.194576
  #   "lon": -122.709480
  #   "calibrationId": "CAHW2Z9CH",
  #.  "calibrationCategory": 4,
  #   "data": [
  #     ["2023-03-07T14:07:03Z", 1, 14.11],
  #     ["2023-03-07T13:49:11Z", 1, 13.87],
  #     ["2023-03-07T13:31:43Z", 1, 11.54],
  #     ["2023-03-07T13:13:07Z", 1, 11.32]
  #   ]
  # }
  #
  # Notes
  # Each row of data has the format  [ measurement time (UTC),  PM2.5 QC flag,  Mass Concentration ]
  # Sorted descending in time

  returnList <- list()

  for ( i in seq_len(nrow(responseDF)) ) {

    datasourceId <- responseDF$datasourceId[i]
    longitude <- responseDF$lon[i]
    latitude <- responseDF$lat[i]
    # These fields are only returned by 'format=USFS2' and will be NULL if not present
    calibrationId <- responseDF$calibrationId[i]
    calibrationCategory <- responseDF$calibrationCategory[i]

    matrix <- responseDF$data[i][[1]]
    colnames(matrix) <- c("timestamp", "pm2.5_QCFlag", "pm2.5")

    # NOTE: Each dataframe will have an hours worth of raw records with pm2.5_QCFlag, pm2.5 and datasourceId

    if ( format == "USFS" ) {

      returnList[[datasourceId]] <-
        dplyr::as_tibble(matrix) %>%
        dplyr::mutate(
          datetime = MazamaCoreUtils::parseDatetime(.data$timestamp, timezone = "UTC"),
          pm2.5_QCFlag = as.numeric(.data$pm2.5_QCFlag),
          pm2.5 = as.numeric(.data$pm2.5),
          datasourceId = !!datasourceId,
          longitude = !!longitude,
          latitude = !!latitude,
          # Calibration fields
          calibrationId = as.character(NA),
          calibrationCategory = as.character(NA)
        ) %>%
        dplyr::select(-"timestamp")

    } else if ( format == "USFS2" ) {

      returnList[[datasourceId]] <-
        dplyr::as_tibble(matrix) %>%
        dplyr::mutate(
          datetime = MazamaCoreUtils::parseDatetime(.data$timestamp, timezone = "UTC"),
          pm2.5_QCFlag = as.numeric(.data$pm2.5_QCFlag),
          pm2.5 = as.numeric(.data$pm2.5),
          datasourceId = !!datasourceId,
          longitude = !!longitude,
          latitude = !!latitude,
          # Calibration fields
          calibrationId = !!calibrationId,
          calibrationCategory = Clarity_assignCalibrationText(!!calibrationCategory)
        ) %>%
        dplyr::select(-"timestamp")

    }

  }

  # ----- Return ---------------------------------------------------------------

  # NOTE:  The returnList will contain per sensor-deployment 'sts' objects

  return(returnList)

}

# ===== Single Source Open Data ================================================

#' @export
#' @importFrom MazamaCoreUtils getAPIKey
#'
#' @title Retrieve current hourly data from a single source.
#'
#' @param api_key Clarity API READ Key. If \code{api_key = NULL}, it
#' will be obtained using \code{getAPIKey("Clarity-read")}.
#' @param datasourceId Clarity sensor identifier.
#' @param format Customized output format ("USFS2", "USFS").
#' @param baseUrl URL endpoint.
#'
#' @return List containing five data frames: \code{meta}, \code{pm2.5_QCFlag},
#' \code{pm2.5}, \code{nowcast_QCFlag} and \code{nowcast}.
#'
#' @description Sends a request to the Clarity API endpoint for Open Data.
#'
#' When \code{format = "USFS2"}, two additional fields are returned in the
#' "meta" dataframe: \code{calibrationId} and \code{calibrationCategory}.
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
#'     format = "USFS2"
#'   )
#'
#' }, silent = FALSE)
#' }

Clarity_getOpenHourly <- function(
  api_key = NULL,
  datasourceId = NULL,
  format = "USFS2",
  baseUrl = "https://clarity-data-api.clarity.io/v1/open/datasource-measurement"
) {

  # ----- Validate parameters --------------------------------------------------

  if ( is.null(api_key) )
    api_key <- MazamaCoreUtils::getAPIKey("Clarity-read")

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
  #   "lat": 42.194576,
  #   "lon": -122.709480,
  #   "calibrationId": "CAHVV2Z9CH",
  #   "calibrationCategory": 4,
  #   "data": [
  #     ["2023-03-07T14Z", 1, 14.02, 1, 14.43],
  #     ["2023-03-07T13Z", 1, 13.97, 1, 12.78],
  #     ["2023-03-07T12Z", 1, 11.02, 1, 12.09],
  #     ...
  #     going back 10 days
  #   ]
  # }
  #
  # Notes
  # Each row of data has the format  [ start of hour (UTC),  1-Hour QC flag,  1-Hour-Mean,  Nowcast QC flag, Nowcast ]
  # Time portion omits minute:second
  # Sorted descending in time

  DFList <- list()

  # NOTE:  responseDF has only a single record (unless the sensor has moved)

  for ( i in seq_len(nrow(responseDF)) ) {

    datasourceId <- responseDF$datasourceId[i]
    longitude <- responseDF$lon[i]
    latitude <- responseDF$lat[i]
    locationName <- responseDF$name[i]
    # These fields are only returned by 'format=USFS2' and will be NULL if not present
    calibrationId <- responseDF$calibrationId[i]
    calibrationCategory <- responseDF$calibrationCategory[i]

    matrix <- responseDF$data[i][[1]]
    colnames(matrix) <- c("timestamp", "pm2.5_QCFlag", "pm2.5", "nowcast_QCFlag", "nowcast")

    # NOTE: Each dataframe will an hours worth of raw records with pm2.5_QCFlag, pm2.5, nowcast_QCFlag, nowcast and datasourceId

    if ( format == "USFS" ) {

      DFList[[datasourceId]] <-
        dplyr::as_tibble(matrix) %>%
        dplyr::mutate(
          datetime = MazamaCoreUtils::parseDatetime(.data$timestamp, timezone = "UTC"),
          pm2.5_QCFlag = as.numeric(.data$pm2.5_QCFlag),
          pm2.5 = as.numeric(.data$pm2.5),
          nowcast_QCFlag = as.numeric(.data$nowcast_QCFlag),
          nowcast = as.numeric(.data$nowcast),
          datasourceId = !!datasourceId,
          locationName = !!locationName,
          longitude = !!longitude,
          latitude = !!latitude,
          # Calibration fields
          calibrationId = as.character(NA),
          calibrationCategory = as.character(NA)
        ) %>%
        dplyr::select(-"timestamp")

    } else if ( format == "USFS2" ) {

      DFList[[datasourceId]] <-
        dplyr::as_tibble(matrix) %>%
        dplyr::mutate(
          datetime = MazamaCoreUtils::parseDatetime(.data$timestamp, timezone = "UTC"),
          pm2.5_QCFlag = as.numeric(.data$pm2.5_QCFlag),
          pm2.5 = as.numeric(.data$pm2.5),
          nowcast_QCFlag = as.numeric(.data$nowcast_QCFlag),
          nowcast = as.numeric(.data$nowcast),
          datasourceId = !!datasourceId,
          locationName = !!locationName,
          longitude = !!longitude,
          latitude = !!latitude,
          # Calibration fields
          calibrationId = !!calibrationId,
          calibrationCategory = Clarity_assignCalibrationText(!!calibrationCategory)
        ) %>%
        dplyr::select(-"timestamp")

    }

  }

  # ----- Return ---------------------------------------------------------------

  returnDF <- dplyr::bind_rows(DFList)

  return(returnDF)

}

#' @export
#' @importFrom MazamaCoreUtils getAPIKey
#'
#' @title Retrieve current individual records from a single source.
#'
#' @param api_key Clarity API READ Key. If \code{api_key = NULL}, it
#' will be obtained using \code{getAPIKey("Clarity-read")}.
#' @param datasourceId Clarity sensor identifier.
#' @param format Customized output format ("USFS2", "USFS").
#' @param baseUrl URL endpoint.
#'
#' @return List containing five data frames: \code{meta}, \code{pm2.5_QCFlag},
#' \code{pm2.5}, \code{nowcast_QCFlag} and \code{nowcast}.
#'
#' @description Sends a request to the Clarity API endpoint for Open Data.
#'
#' When \code{format = "USFS2"}, two additional fields are returned in the
#' "meta" dataframe: \code{calibrationId} and \code{calibrationCategory}.
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
#'     format = "USFS2"
#'   )
#'
#' }, silent = FALSE)
#' }

Clarity_getOpenIndividual <- function(
  api_key = NULL,
  datasourceId = NULL,
  format = c("USFS2", "USFS"),
  baseUrl = "https://clarity-data-api.clarity.io/v1/open/datasource-measurement"
) {

  # ----- Validate parameters --------------------------------------------------

  if ( is.null(api_key) )
    api_key <- MazamaCoreUtils::getAPIKey("Clarity-read")

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
  #   "lat": 42.194576,
  #   "lon": -122.709480,
  #   "calibrationId": "CAHVV2Z9CH",
  #   "calibrationCategory": 4,
  #   "data": [
  #     ["2023-03-07T14Z", 1, 14.02, 1, 14.43],
  #     ["2023-03-07T13Z", 1, 13.97, 1, 12.78],
  #     ["2023-03-07T12Z", 1, 11.02, 1, 12.09],
  #     ...
  #     going back 10 days
  #   ]
  # }
  #
  # Notes
  # Each row of data has the format  [ start of hour (UTC),  1-Hour QC flag,  1-Hour-Mean,  Nowcast QC flag, Nowcast ]
  # Time portion omits minute:second
  # Sorted descending in time

  DFList <- list()

  # NOTE:  responseDF has only a single record (unless the sensor has moved)

  for ( i in seq_len(nrow(responseDF)) ) {

    datasourceId <- responseDF$datasourceId[i]
    longitude <- responseDF$lon[i]
    latitude <- responseDF$lat[i]
    locationName <- responseDF$name[i]
    # These fields are only returned by 'format=USFS2' and will be NULL if not present
    calibrationId <- responseDF$calibrationId[i]
    calibrationCategory <- responseDF$calibrationCategory[i]

    matrix <- responseDF$data[i][[1]]
    colnames(matrix) <- c("timestamp", "pm2.5_QCFlag", "pm2.5", "nowcast_QCFlag", "nowcast")

    # NOTE: Each dataframe will contain an hours worth of raw records with pm2.5_QCFlag, pm2.5, nowcast_QCFlag, nowcast and datasourceId

    if ( format == "USFS" ) {

      DFList[[datasourceId]] <-
        dplyr::as_tibble(matrix) %>%
        dplyr::mutate(
          datetime = MazamaCoreUtils::parseDatetime(.data$timestamp, timezone = "UTC"),
          pm2.5_QCFlag = as.numeric(.data$pm2.5_QCFlag),
          pm2.5 = as.numeric(.data$pm2.5),
          nowcast_QCFlag = as.numeric(.data$nowcast_QCFlag),
          nowcast = as.numeric(.data$nowcast),
          datasourceId = !!datasourceId,
          locationName = !!locationName,
          longitude = !!longitude,
          latitude = !!latitude,
          # Calibration fields
          calibrationId = as.character(NA),
          calibrationCategory = as.character(NA)
        ) %>%
        dplyr::select(-"timestamp")

    } else if ( format == "USFS2" ) {

      DFList[[datasourceId]] <-
        dplyr::as_tibble(matrix) %>%
        dplyr::mutate(
          datetime = MazamaCoreUtils::parseDatetime(.data$timestamp, timezone = "UTC"),
          pm2.5_QCFlag = as.numeric(.data$pm2.5_QCFlag),
          pm2.5 = as.numeric(.data$pm2.5),
          nowcast_QCFlag = as.numeric(.data$nowcast_QCFlag),
          nowcast = as.numeric(.data$nowcast),
          datasourceId = !!datasourceId,
          locationName = !!locationName,
          longitude = !!longitude,
          latitude = !!latitude,
          # Calibration fields
          calibrationId = !!calibrationId,
          calibrationCategory = Clarity_assignCalibrationText(!!calibrationCategory)
        ) %>%
        dplyr::select(-"timestamp")

    }

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


# Assign calibration text

Clarity_assignCalibrationText <- function(x) {
  # See: https://docs.google.com/document/d/16jLGz4OYB30bForiPQppCQ9s3jsw8F9OLeezo9XQw6g

  # 0  Uncalibrated
  # 1  Mixed calibrations
  # 2  Custom calibrations
  # 3  Global PM2.5 v1
  # 4  Global PM2.5 v2

  calibrationNames <- c(
    "uncalibrated",
    "mixed_calibrations",
    "custom calibrations",
    "global PM2.5 v1",
    "global PM2.5 v2"
  )

  calibrationText <- calibrationNames[x + 1]

  return(calibrationText)

}

