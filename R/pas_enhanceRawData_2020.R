#' @export
#' @importFrom MazamaCoreUtils logger.isInitialized logger.trace logger.warn logger.error logger.fatal
#' @importFrom rlang .data
#'
#' @title Enhance synoptic data from PurpleAir
#'
#' @description Enhance raw synoptic data from PurpleAir to create an improved
#' dataframe compatible with the \pkg{MazamaLocationUtils} package.
#'
#' Steps include:
#'
#' 1) Replace variable with more consistent, more human readable names.
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
#' 4) Add distance and monitorID for the two closest PWFSL monitors
#'
#' 5) Add additional metadata items:
#' \itemize{
#' \item{sensorManufacturer = "Purple Air"}
#' \item{targetPollutant = "PM"}
#' }
#'
#' Filtering by country can speed up the process of enhancement and may be
#' performed by providing a vector ISO country codes to the \code{countryCodes}
#' argument. By default, no subsetting is performed.
#'
#' Setting \code{outsideOnly = TRUE} will return only those records marked as
#' 'outside'.
#'
#' @note For data obtained on July 28, 2018 this will result in removal of all
#' 'B' channels, even those whose parent 'A' channel is marked as 'outside'.
#' This is useful if you want a quick, synoptic view of the network, e.g. for a
#' map.
#'
#' @param pas_raw Dataframe returned by \code{pas_downloadParseRawData_2020()}.
#' @param countryCodes ISO country codes used to subset the data.
#'
#' @return Enhanced Dataframe of synoptic PurpleAir data.
#'
#' @seealso \link{pas_downloadParseRawData}
#'
#' @examples
#' \donttest{
#' library(AirSensor)
#'
#' ###initializeMazamaSpatialUtils()
#' MazamaLocationUtils::mazama_initialize()
#'
#' pas <- pas_enhanceData(example_pas_raw, 'US')
#'
#' setdiff(names(pas), names(example_pas_raw))
#' setdiff(names(example_pas_raw), names(pas))
#'
#' if ( interactive() ) {
#'   View(pas[1:100,])
#' }
#' }

pas_enhanceRawData_2020 <- function(
  pas_raw = NULL,
  countryCodes = NULL
) {

  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(pas_raw)
  MazamaCoreUtils::stopIfNull(countryCodes)

  if ( !is.data.frame(pas_raw) )
    stop("parameter 'pas_raw' parameter is not a dataframe")

  # Guarantee uppercase codes
  countryCodes <- toupper(countryCodes)

  # Validate countryCodes
  if ( any(!(countryCodes %in% countrycode::codelist$iso2c)) )
    stop("parameter 'countryCodes' has values that are not recognized as ISO-2 country codes")

  # Set up spatial data before continuing
  MazamaLocationUtils::mazama_initialize()

  # ----- Replicate fields to B channel ----------------------------------------

  # > dplyr::glimpse(pas_raw, width=75)
  # Rows: 44,974
  # Columns: 32
  # $ ID                               <chr> "14633", "14634", "25999", "2600…
  # $ Label                            <chr> " Hazelwood canary ", " Hazelwoo…
  # $ DEVICE_LOCATIONTYPE              <chr> "outside", NA, "outside", NA, "o…
  # $ THINGSPEAK_PRIMARY_ID            <chr> "559921", "559923", "694803", "6…
  # $ THINGSPEAK_PRIMARY_ID_READ_KEY   <chr> "CU4BQZZ38WO5UJ4C", "DULWDNCI9M6…
  # $ THINGSPEAK_SECONDARY_ID          <chr> "559922", "559924", "694804", "6…
  # $ THINGSPEAK_SECONDARY_ID_READ_KEY <chr> "D0YNZ1LM59LL49VQ", "EY2CNMYRUZH…
  # $ Lat                              <dbl> 37.27556, 37.27556, 30.05381, 30…
  # $ Lon                              <dbl> -121.96413, -121.96413, -95.4946…
  # $ PM2_5Value                       <chr> "0.3", "0.0", "7.5", "5.34", "0.…
  # $ LastSeen                         <int> 1650412036, 1650412036, 16504120…
  # $ Type                             <chr> "PMS5003+PMS5003+BME280", NA, "P…
  # $ Hidden                           <chr> "false", "false", "false", "fals…
  # $ Flag                             <int> 1, NA, NA, NA, 1, 1, 1, NA, NA, …
  # $ isOwner                          <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
  # $ humidity                         <chr> "25", NA, "52", NA, "28", NA, "4…
  # $ temp_f                           <chr> "80", NA, "78", NA, "77", NA, "7…
  # $ pressure                         <chr> "1007.75", NA, "1012.95", NA, "1…
  # $ AGE                              <int> 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0,…
  # $ ParentID                         <chr> NA, "14633", NA, "25999", NA, "1…
  # $ A_H                              <chr> NA, NA, NA, NA, NA, NA, NA, NA, …
  # $ Ozone1                           <chr> NA, NA, NA, NA, NA, NA, NA, NA, …
  # $ v                                <dbl> 0.30, 0.00, 7.50, 5.34, 0.00, 0.…
  # $ v1                               <dbl> 0.11, 0.15, 8.37, 6.39, 0.07, 0.…
  # $ v2                               <dbl> 0.12, 0.19, 10.30, 8.09, 0.06, 0…
  # $ v3                               <dbl> 0.14, 0.21, 11.06, 8.78, 0.09, 0…
  # $ v4                               <dbl> 0.46, 0.63, 14.21, 11.20, 0.43, …
  # $ v5                               <dbl> 1.01, 1.28, 16.83, 14.04, 0.83, …
  # $ v6                               <dbl> 2.16, 2.65, 13.66, 11.66, 1.94, …
  # $ pm                               <dbl> 0.30, 0.00, 7.50, 5.34, 0.00, 0.…
  # $ lastModified                     <dbl> 1.650412e+12, 1.650412e+12, 1.65…
  # $ timeSinceModified                <dbl> 119982, 119983, 120009, 120009, …

  # NOTE:  Both the "DEVICE_LOCATIONTYPE" and "Type" fields are present for the
  # NOTE:  A channel but missing from the B channel. This generates confusion
  # NOTE:  when people try to use dplyr to filter for "outside" sensors.

  A <-
    pas_raw %>%
    # Limit to A channel
    dplyr::filter(is.na(.data$ParentID)) %>%
    # Retain columns to be replicated
    dplyr::select(c("ID", "DEVICE_LOCATIONTYPE", "Type"))

  B <-
    pas_raw %>%
    # Limit to B channel
    dplyr::filter(!is.na(.data$ParentID)) %>%
    # Retain only the A channel ID
    dplyr::select(c("ID", "ParentID")) %>%
    # Join, matching ParentID to ID
    dplyr::left_join(A, by = c("ParentID" = "ID")) %>%
    # Remove ParentID
    dplyr::select(-"ParentID")

  # Combine to get all rows
  AB <- dplyr::bind_rows(A, B)

  pas_raw <-
    pas_raw %>%
    # Remove the columns to be replicated
    dplyr::select(-c("DEVICE_LOCATIONTYPE", "Type")) %>%
    # Add the replicated columns found in AB
    dplyr::left_join(AB, by = "ID")

  # ----- Discard unwanted columns ---------------------------------------------

  # Remove "pm" because it is redundant with the value in "v"
  if ( "pm" %in% names(pas_raw) ) {
    pas_raw$pm <- NULL
  }

  # ----- Rename columns -------------------------------------------------------

  # Rename columns to have consistent lowerCamelCase and better human names
  # based on the information in the document "Using PurpleAir Data".

  pas <-
    pas_raw %>%
    dplyr::rename(
      parentID = .data$ParentID,
      locationName = .data$Label,
      latitude = .data$Lat,
      longitude = .data$Lon,
      pm25 = .data$PM2_5Value,
      lastSeenDate = .data$LastSeen,
      sensorType = .data$Type,
      flag_hidden = .data$Hidden,
      flag_highValue = .data$Flag,
      flag_attenuation_hardware = .data$A_H,
      temperature = .data$temp_f,
      age = .data$AGE,
      pm25_current = .data$v,
      pm25_10min = .data$v1,
      pm25_30min = .data$v2,
      pm25_1hr = .data$v3,
      pm25_6hr = .data$v4,
      pm25_1day = .data$v5,
      pm25_1week = .data$v6,
      statsLastModifiedDate = .data$lastModified,
      statsLastModifiedInterval = .data$timeSinceModified
    )

  # ----- Remove invalid locations ---------------------------------------------

    # # Add empty fields for core metadata
    # MazamaLocationUtils::table_addCoreMetadata()

  # # ----- Add spatial metadata -------------------------------------------------
  #
  # pas <- pas_addSpatialMetadata(pas, countryCodes)
  #
  # # ----- Add unique identifiers -----------------------------------------------
  #
  # pas <- pas_addUniqueIDs(pas)
  #
  # # ----- Add an Air district --------------------------------------------------
  #
  # pas <- pas_addAirDistrict(pas)
  #
  # # ----- Convert times to POSIXct ---------------------------------------------
  #
  # pas$lastSeenDate <- as.POSIXct(pas$lastSeenDate,
  #                                tz = "UTC",
  #                                origin = lubridate::origin)
  #
  # pas$statsLastModifiedDate <- as.POSIXct(pas$statsLastModifiedDate / 1000,
  #                                         tz = "UTC",
  #                                         origin = lubridate::origin)
  #
  # # ----- Convert to proper type -----------------------------------------------
  #
  # pas$ID <- as.character(pas$ID)
  # pas$parentID <- as.character(pas$parentID)
  # pas$pm25 <- as.numeric(pas$pm25)
  # pas$pm25_current <- as.numeric(pas$pm25_current)
  # pas$flag_hidden <- ifelse(pas$flag_hidden == 'true', TRUE, FALSE)
  # pas$flag_highValue <- ifelse(pas$flag_highValue == 1, TRUE, FALSE)
  # pas$flag_attenuation_hardware <- ifelse(pas$flag_attenuation_hardware == 'true', TRUE, FALSE)
  # pas$temperature <- as.numeric(pas$temperature)
  # pas$humidity <- as.numeric(pas$humidity)
  # pas$pressure <- as.numeric(pas$pressure)
  #
  # # ----- Convert to internally standard units ---------------------------------
  #
  # pas$statsLastModifiedInterval <- pas$statsLastModifiedInterval / 1000   # seconds
  #
  # # Round values to reflect resolution as specified in:
  # #   https://www.purpleair.com/sensors
  #
  # # TODO:  Figure out why rounding breaks outlier detection
  #
  # # pas$pm25 <- round(pas$pm25)
  # # pas$pm25_current <- round(pas$pm25_current)
  # # pas$temperature <- round(pas$temperature)
  # # pas$humidity <- round(pas$humidity)
  # # pas$pressure <- round(pas$pressure)
  #
  # # ----- Find nearby PWFSL monitors -------------------------------------------
  #
  # # NOTE:  These columns need to exist even if they are all missing
  # pas$pwfsl_closestDistance <- as.numeric(NA)
  # pas$pwfsl_closestMonitorID <- as.character(NA)
  #
  # if ( includePWFSL ) {
  #
  #   if ( logger.isInitialized() ) {
  #     logger.trace("Adding PWFSL monitor metadata")
  #   }
  #   if ( !exists('pwfsl') ) {
  #     pwfsl <- PWFSLSmoke::loadLatest()
  #   }
  #   for ( i in seq_len(nrow(pas)) ) {
  #     distances <- PWFSLSmoke::monitor_distance(pwfsl,
  #                                               pas$longitude[i],
  #                                               pas$latitude[i])
  #     minDistIndex <- which.min(distances)
  #     pas$pwfsl_closestDistance[i] <- distances[minDistIndex] * 1000 # To meters
  #     pas$pwfsl_closestMonitorID[i] <- names(distances[minDistIndex])
  #   }
  #
  # }
  #
  # # ----- Addditional metadata per SCAQMD request ------------------------------
  #
  # pas$sensorManufacturer <- "Purple Air"
  # pas$targetPollutant <- "PM"
  # pas$technologyType <- "consumer-grade"
  #
  # # ----- Add communityRegion --------------------------------------------------
  #
  # pas <- pas_addCommunityRegion(pas)
  #
  # # ----- Return ---------------------------------------------------------------

  # Guarantee the class name still exists
  class(pas) <- union('pa_synoptic', class(pas))

  return(pas)

}
