#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.error logger.debug logger.isInitialized
#'
#' @title Download synoptic data from PurpleAir with older API
#'
#' @param baseUrl Base URL for access to PurpleAir synoptic data. This is the
#' old, 2020 API for accessing synoptic data tha may disappear at any point.
#'
#' @return Dataframe of synoptic PurpleAir data.
#'
#' @description Download and parse synoptic data for the PurpleAir network
#' of particulate sensors.
#'
#' The synoptic data provides a view of the entire PurpleAir network and
#' includes both metadata and recent PM2.5 averages for each deployed sensor.
#'
#' @references \href{https://www.purpleair.com/json?all=true}{json formatted PurpleAir data}
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(AirSensor2)
#'
#' pas_raw <- pas_downloadParseRawData_2020()
#'
#' if ( interactive() ) View(pas_raw[1:100,])
#'
#' }, silent = FALSE)
#' }

pas_downloadParseRawData_2020 <- function(
  baseUrl = "https://www.purpleair.com/json?all=true"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(baseUrl)

  # ----- Request data ---------------------------------------------------------

  # Strip off any final "/"
  baseUrl <- stringr::str_replace(baseUrl, "/$", "")

  # Placeholder in case things get more complicated
  webserviceUrl <- baseUrl

  # NOTE:  using Hadley Wickham style:
  # NOTE:  https://github.com/hadley/httr/blob/master/vignettes/quickstart.Rmd
  r <- httr::GET(webserviceUrl)

  # * Error response -----

  if ( httr::http_error(r) ) {  # web service failed to respond

    # TODO:  Find a package with web service status codes

    # https://digitalocean.com/community/tutorials/how-to-troubleshoot-common-http-error-codes
    if ( httr::status_code(r) == 429 ) {
      err_msg <- paste0("web service error 429 from ", webserviceUrl,
                        ": Too Many Requests")
    } else if ( httr::status_code(r) == 500 ) {
      err_msg <- paste0("web service error 500 from ", webserviceUrl,
                        ": Internal Server Error")
    } else if ( httr::status_code(r) == 502 ) {
      err_msg <- paste0("web service error 502", webserviceUrl,
                        ": Bad Gateway")
    } else if ( httr::status_code(r) == 503 ) {
      err_msg <- paste0("web service error 503", webserviceUrl,
                        ": Service Unavailable")
    } else if ( httr::status_code(r) == 504 ) {
      err_msg <- paste0("web service error 504 from ", webserviceUrl,
                        ": Gateway Timeout from ",
                        webserviceUrl)
    } else {
      err_msg <- paste0("web service error ", httr::status_code(r), " from ",
                        webserviceUrl)
    }

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

  # > names(PAList)
  # [1] "mapVersion"       "baseVersion"      "mapVersionString" "results"

  if ( logger.isInitialized() ) {
    logger.debug("mapVersion = \"%s\"", PAList$mapVersion)
    logger.debug("baseVersion = \"%s\"", PAList$baseVersion)
    logger.debug("mapVersionString = \"%s\"", PAList$mapVersionString)
  }

  # Pull out the dataframe of results
  resultsDF <- PAList$results

  # NOTE:  This was generated on 2022-04-19

  # > dplyr::glimpse(resultsDF, width = 75)
  # Rows: 44,976
  # Columns: 23
  # $ ID                               <int> 14633, 14634, 25999, 26000, 1409…
  # $ Label                            <chr> " Hazelwood canary ", " Hazelwoo…
  # $ DEVICE_LOCATIONTYPE              <chr> "outside", NA, "outside", NA, "o…
  # $ THINGSPEAK_PRIMARY_ID            <chr> "559921", "559923", "694803", "6…
  # $ THINGSPEAK_PRIMARY_ID_READ_KEY   <chr> "CU4BQZZ38WO5UJ4C", "DULWDNCI9M6…
  # $ THINGSPEAK_SECONDARY_ID          <chr> "559922", "559924", "694804", "6…
  # $ THINGSPEAK_SECONDARY_ID_READ_KEY <chr> "D0YNZ1LM59LL49VQ", "EY2CNMYRUZH…
  # $ Lat                              <dbl> 37.27556, 37.27556, 30.05381, 30…
  # $ Lon                              <dbl> -121.96413, -121.96413, -95.4946…
  # $ PM2_5Value                       <chr> "0.22", "0.38", "14.69", "11.15"…
  # $ LastSeen                         <int> 1650407116, 1650407116, 16504071…
  # $ Type                             <chr> "PMS5003+PMS5003+BME280", NA, "P…
  # $ Hidden                           <chr> "false", "false", "false", "fals…
  # $ Flag                             <int> 1, NA, NA, NA, 1, 1, 1, NA, NA, …
  # $ isOwner                          <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
  # $ humidity                         <chr> "24", NA, "47", NA, "28", NA, "4…
  # $ temp_f                           <chr> "82", NA, "80", NA, "76", NA, "7…
  # $ pressure                         <chr> "1009.18", NA, "1013.72", NA, "1…
  # $ AGE                              <int> 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1,…
  # $ Stats                            <chr> "{\"v\":0.22,\"v1\":0.17,\"v2\":…
  # $ ParentID                         <int> NA, 14633, NA, 25999, NA, 14091,…
  # $ A_H                              <chr> NA, NA, NA, NA, NA, NA, NA, NA, …
  # $ Ozone1                           <chr> NA, NA, NA, NA, NA, NA, NA, NA, …

  # * convert results$Stats -----

  # NOTE:  Stats for current, 10 min, 30 min, 1 hr, 6 hr, 1 day, 1 week are
  # NOTE:  stored in df$Stats

  # NOTE:  Some Stats are NA and we need to fill in a blank stats array for those
  emptyStatsList <- list(
    v = -999.9,
    v1 = -999.9,
    v2 = -999.9,
    v3 = -999.9,
    v4 = -999.9,
    v5 = -999.9,
    v6 = -999.9,
    pm = -999.9,
    lastModified = -999.9,
    timeSinceModified = -999 # int
  )

  emptyStatsJSON <-
    jsonlite::toJSON(
      emptyStatsList,
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      pretty = FALSE
    )

  # Add empty JSON string where needed
  missingStatsMask <- is.na(resultsDF$Stats)
  resultsDF$Stats[missingStatsMask] <- emptyStatsJSON

  # ----- Create a unified tibble ----------------------------------------------

  # Convert to tibble and guarantee character data types for IDs
  resultsTbl <-
    dplyr::as_tibble(resultsDF) %>%
    dplyr::mutate(
      ID = as.character(.data$ID),
      THINGSPEAK_PRIMARY_ID = as.character(.data$THINGSPEAK_PRIMARY_ID),
      THINGSPEAK_SECONDARY_ID = as.character(.data$THINGSPEAK_SECONDARY_ID),
      ParentID = as.character(.data$ParentID)
    )

  statsList <- lapply(resultsDF$Stats, function(x) { jsonlite::fromJSON(x) } )

  # NOTE:  At this point we have a statsList where every element is a list.
  # NOTE:  Some Stats are missing 'pm', 'lastModified' and 'timeSinceModified'
  # NOTE:  but bind_rows() will take care of this by filling in those columns
  # NOTE:  with NA.

  statsTbl <- dplyr::bind_rows(statsList)

  # Now convert -999.9 and -999 back to NA
  missingMask <- statsTbl <= -999
  statsTbl[missingMask] <- as.numeric(NA)

  # Now create a new dataframe using the important columns from results and stats

  # > print(sort(names(resultsDF)), width = 75)
  # [1] "A_H"                              "AGE"
  # [3] "DEVICE_LOCATIONTYPE"              "Flag"
  # [5] "Hidden"                           "humidity"
  # [7] "ID"                               "isOwner"
  # [9] "Label"                            "LastSeen"
  # [11] "Lat"                              "Lon"
  # [13] "Ozone1"                           "ParentID"
  # [15] "PM2_5Value"                       "pressure"
  # [17] "Stats"                            "temp_f"
  # [19] "THINGSPEAK_PRIMARY_ID"            "THINGSPEAK_PRIMARY_ID_READ_KEY"
  # [21] "THINGSPEAK_SECONDARY_ID"          "THINGSPEAK_SECONDARY_ID_READ_KEY"
  # [23] "Type"

  # > print(names(statsTbl), width = 75)
  # [1] "v"                 "v1"                "v2"
  # [4] "v3"                "v4"                "v5"
  # [7] "v6"                "pm"                "lastModified"
  # [10] "timeSinceModified"

  tbl <- dplyr::bind_cols(resultsTbl, statsTbl)
  tbl$Stats <- NULL

  # ----- Return ---------------------------------------------------------------

  return(tbl)

}
