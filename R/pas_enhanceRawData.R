#' @export
#' @importFrom rlang .data
#'
#' @title Enhance synoptic data from PurpleAir
#'
#' @description Enhance raw synoptic data from PurpleAir to create an improved
#' dataframe compatible with the \pkg{MazamaLocationUtils} package.
#'
#' Steps include:
#'
#' 1) Replace variable names with more consistent, human readable names.
#'
#' 2) Add spatial metadata for each sensor including:
#' \itemize{
#'   \item{timezone -- Olson timezone}
#'   \item{countryCode -- ISO 3166-1 alpha-2}
#'   \item{stateCode -- ISO 3166-2 alpha-2}
#' }
#'
#' 3) Convert data types from character to \code{POSIXct} and \code{numeric}.
#'
#' 4) Add additional metadata items:
#' \itemize{
#' \item{sensorManufacturer = "Purple Air"}
#' }
#'
#' Limiting spatial searches by country can greatly speed up the process of
#' enhancement. This is performed by providing a vector of ISO country codes to
#' the \code{countryCodes} argument. By default, no subsetting is performed.
#'
#' Users may further improve performance by also specifying \code{stateCodes}
#' when \code{countryCodes} is limited to a single country.
#'
#' When a single US state is specified, named \code{counties} may be specified
#' to further speed up performance.
#'
#' @param pas_raw Dataframe returned by \link{pas_downloadParseRawData}.
#' @param countryCodes ISO 3166-1 alpha-2 country codes used to subset the data.
#' @param stateCodes ISO-3166-2 alpha-2 state codes used to subset the data.
#' @param counties US county names or 5-digit FIPS codes used to subset the data.
#'
#' @return Enhanced dataframe of synoptic PurpleAir data.
#'
#' @seealso \link{pas_downloadParseRawData}
#'
#' @references \href{https://www2.purpleair.com}{PurpleAir}
#' @references \href{https://api.purpleair.com/}{PurpleAir API}
#' @references \href{https://www2.purpleair.com/policies/terms-of-service}{PurpleAir Terms of service}
#' @references \href{https://www2.purpleair.com/pages/license}{PurpleAir Data license}
#' @references \href{https://www2.purpleair.com/pages/attribution}{PurpleAir Data Attribution}
#'

pas_enhanceRawData <- function(
    pas_raw = NULL,
    countryCodes = NULL,
    stateCodes = NULL,
    counties = NULL
) {

  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(pas_raw)

  if ( !is.data.frame(pas_raw) )
    stop("parameter 'pas_raw' parameter is not a dataframe")

  # Guarantee uppercase codes
  countryCodes <- toupper(countryCodes)

  # Validate countryCodes
  if ( any(!(countryCodes %in% countrycode::codelist$iso2c)) )
    stop("parameter 'countryCodes' has values that are not recognized as ISO-2 country codes")

  if ( !is.null(stateCodes) ) {
    if ( is.null(countryCodes) ) {
      stop("'stateCodes' can only be used when also specifying a single country with 'countryCodes'")
    } else if ( length(countryCodes) != 1 ) {
      stop("please limit 'countryCodes' to a single country when using 'stateCodes'")
    }
    if ( !is.null(counties) ) {
      if ( length(stateCodes) != 1 ) {
        stop("please limit 'stateCodes' to a single state when using 'counties'")
      }
    }
  }

  if ( !"longitude" %in% names(pas_raw) ) {
    stop("Parameter 'pas_raw' does not have a 'longitude' column as required by pas_enhanceRawData()")
  }

  if ( !"latitude" %in% names(pas_raw) ) {
    stop("Parameter 'pas_raw' does not have a 'latitude' column as required by pas_enhanceRawData()")
  }

  if ( !"location_type" %in% names(pas_raw) ) {
    stop("Parameter 'pas_raw' does not have a 'location_type' column as required by pas_enhanceRawData()")
  }

  # ----- Harmonize table ------------------------------------------------------

  # > dplyr::glimpse(pas_raw, width = 75)
  # Rows: 1,999
  # Columns: 37
  # $ sensor_index         <chr> "453", "131585", "131611", "131707", "896", …
  # $ last_modified        <dttm> 2019-09-04 21:14:11, 2022-01-29 19:14:34, 2…
  # $ date_created         <dttm> 2016-11-07 18:24:07, 2021-10-05 22:00:29, 2…
  # $ last_seen            <dttm> 2023-02-18 00:47:35, 2023-02-18 00:47:48, 2…
  # $ private              <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0",…
  # $ name                 <chr> "LRAPA-Oakridge City Hall", "1907 ZE Outside…
  # $ icon                 <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0",…
  # $ location_type        <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0",…
  # $ model                <chr> "PA-II", "PA-II-SD", "PA-II", "PA-II-SD", "P…
  # $ hardware             <chr> "2.0+1M+BME280+PMSX003-B+PMSX003-A", "2.0+OP…
  # $ led_brightness       <dbl> 25, 35, 35, 35, 35, 35, 15, 35, 35, 35, 35, …
  # $ firmware_version     <chr> "6.06b", "7.02", "7.02", "7.02", "6.06b", "7…
  # $ firmware_upgrade     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
  # $ rssi                 <dbl> -63, -66, -75, -47, -74, -90, -88, -73, -65,…
  # $ uptime               <dbl> 3112, 68034, 6028, 10397, 3646, 11740, 25, 5…
  # $ pa_latency           <dbl> 221, 256, 458, 514, 219, 308, NA, 250, 234, …
  # $ memory               <dbl> 16136, 16064, 15768, 16320, 15872, 16120, 31…
  # $ position_rating      <dbl> 5, 5, 5, 0, 5, 0, 5, 5, 5, 5, 5, 0, 5, 2, 5,…
  # $ latitude             <dbl> 43.74751, 45.49798, 42.30729, 48.81992, 48.9…
  # $ longitude            <dbl> -122.4567, -122.6099, -122.8396, -119.1847, …
  # $ altitude             <dbl> 1229, 233, 1650, 3650, 160, 663, 185, 190, 2…
  # $ channel_state        <chr> "3", "3", "3", "3", "3", "3", "3", "3", "3",…
  # $ channel_flags        <chr> "1", "0", "0", "0", "0", "0", "0", "0", "0",…
  # $ channel_flags_manual <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0",…
  # $ channel_flags_auto   <chr> "1", "0", "0", "0", "0", "0", "0", "0", "0",…
  # $ confidence           <dbl> 0, 100, 100, 100, 100, 100, 100, 100, 100, 1…
  # $ confidence_auto      <dbl> 0, 100, 100, 100, 100, 100, 100, 100, 100, 1…
  # $ confidence_manual    <dbl> 0, 100, 100, 100, 100, 100, 100, 100, 100, 1…
  # $ humidity             <dbl> 32, 37, 19, 31, 57, 69, 56, 49, 50, 57, 56, …
  # $ temperature          <dbl> 56, 61, 62, 72, 52, 49, 53, 55, 54, 51, 52, …
  # $ pressure             <dbl> 978.10, 1017.26, 961.68, 889.75, 1017.72, 10…
  # $ pm2.5_10minute       <dbl> 0.0, 12.1, 0.0, 1.8, 5.0, 4.8, 6.3, 2.2, 5.4…
  # $ pm2.5_30minute       <dbl> 0.0, 12.0, 0.0, 3.9, 6.0, 4.6, 4.8, 1.6, 3.8…
  # $ pm2.5_60minute       <dbl> 0.0, 12.8, 0.4, 7.2, 6.7, 3.7, 5.8, 1.8, 3.8…
  # $ pm2.5_6hour          <dbl> 0.0, 22.8, 5.4, 8.2, 10.0, 1.6, 9.2, 7.0, 9.…
  # $ pm2.5_24hour         <dbl> 0.0, 25.2, 6.1, 5.9, 10.6, 1.6, 11.8, 10.9, …
  # $ pm2.5_1week          <dbl> 0.0, 16.7, 5.4, 8.6, 10.6, 2.0, 14.9, 12.8, …

  pas <- pas_raw

  # Go through each step and only perform action if that parameter exists

  # * Harmonize basic fields
  # Privacy
  if ( "private" %in% names(pas) ) {
    pas$privacy <- dplyr::if_else(pas$private == "0", "public", "private", as.character(NA))
    pas$private <- NULL
  }

  # sensorManufacturer
  pas$sensorManufacturer <- "PurpleAir"

  # deviceID
  # sensor_index is guaranteed to exist
  pas$deviceID = paste0("pa.", pas$sensor_index)

  # location_type is guaranteed to exist
  pas$location_type = dplyr::if_else(pas$location_type == "0", "outside", "inside", as.character(NA))

  # elevation
  if ( "elevation" %in% names(pas) ) {
    pas$elevation = round(pas$altitude * 0.3048) # convert from feet to meters
  } else {
    pas$elevation = as.numeric(NA)
  }

  # Remove unwanted fields
  if ( "icon" %in% names(pas) ) {
    pas$icon <- NULL
  }

  # * Add core metadata -----
  # NOTE:  precision = 9 results in a precision of ~2 meters
  pas <-
    pas %>%
    MazamaLocationUtils::table_addCoreMetadata(precision = 9) %>%

    # Unique identifier
    dplyr::mutate(
      deviceDeploymentID = paste0(.data$locationID, "_", .data$deviceID),
      locationName = .data$name
    )

  # TODO:  This step can be removed when AirMonitor gets upgraded to replace
  # TODO:  'zip' with 'postalCode'.
  if ( !"zip" %in% names(pas) ) {
    pas$zip <- pas$postalCode
  }
  if ( !"postalCode" %in% names(pas) ) {
    pas$postalCode <- pas$zip
  }

  # Put 'deviceDeploymentID' and 'deviceID' in front
  startingIDs <- c("deviceDeploymentID", "deviceID", "locationID")
  otherColumns <- setdiff(names(pas), startingIDs)
  orderedColumns <- c(startingIDs, otherColumns)
  pas <- pas %>% dplyr::select(dplyr::all_of(orderedColumns))

  # ----- Add spatial metadata -------------------------------------------------

  # * countryCode -----

  pas$countryCode <-
    MazamaSpatialUtils::getCountryCode(
      longitude = pas$longitude,
      latitude = pas$latitude,
      countryCodes = countryCodes,
      allData = FALSE,
      useBuffering = FALSE            # No buffering needed with the EEZ dataset
    )

  # Limit to valid countryCodes
  pas <-
    pas %>%
    dplyr::filter(!is.na(.data$countryCode))

  # * stateCode -----

  # Suppress annoying 'Discarded datum Unknown' messages
  suppressWarnings({
    pas$stateCode <-
      MazamaSpatialUtils::getStateCode(
        longitude = pas$longitude,
        latitude = pas$latitude,
        countryCodes = countryCodes,
        allData = FALSE,
        useBuffering = TRUE
      )
  })

  # Limit to valid stateCodes
  if ( !is.null(stateCodes) ) {
    pas <-
      pas %>%
      dplyr::filter(.data$stateCode %in% stateCodes)
  }

  # * countyName -----

  if ( length(countryCodes) == 1 && countryCodes[1] == "US" ) {

    # Suppress annoying 'Discarded datum Unknown' messages
    suppressWarnings({
      pas$countyName <-
        MazamaSpatialUtils::getUSCounty(
          longitude = pas$longitude,
          latitude = pas$latitude,
          stateCodes = stateCodes,
          allData = FALSE,
          useBuffering = TRUE
        )
    })

    # Limit to valid counties
    if ( !is.null(counties) ) {
      counties <- as.character(counties)
      # Convert from FIPS to countyName
      if ( stringr::str_detect(counties[1], "[0-9]{5}") ) {
        counties <-
          MazamaSpatialUtils::US_countyFIPSToName(
            state = stateCodes,
            countyFIPS = counties
          )
      }
      # Handle input inconsistencies
      counties <-
        stringr::str_to_title(counties) %>%
        stringr::str_replace(" County", "")
      # Limit to valid counties
      pas <-
        pas %>%
        dplyr::filter(.data$countyName %in% counties)
    }

  }

  # * timezone -----

  # Suppress annoying 'Discarded datum Unknown' messages
  suppressWarnings({
    pas$timezone <-
      MazamaSpatialUtils::getTimezone(
        longitude = pas$longitude,
        latitude = pas$latitude,
        countryCodes = countryCodes,
        allData = FALSE,
        useBuffering = TRUE
      )
  })

  # ----- Return ---------------------------------------------------------------

  # Add the "PurpleAir_synoptic" class name
  class(pas) <- union("PurpleAir_synoptic", class(pas))

  return(pas)

}
