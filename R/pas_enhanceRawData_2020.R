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
#' 1) Replace variable with more consistent, human readable names.
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
#' Filtering by country can speed up the process of enhancement and may be
#' performed by providing a vector ISO country codes to the \code{countryCodes}
#' argument. By default, no subsetting is performed.
#'
#' Uses may also limit results by specifying \code{stateCodes} when
#' \code{countryCodes} is limited to a single country.
#'
#' @param pas_raw Dataframe returned by \code{pas_downloadParseRawData_2020()}.
#' @param countryCodes ISO 3166-1 alpha-2 country codes used to subset the data.
#' @param stateCodes ISO-3166-2 alpha-2 state codes used to subset the data.
#'
#' @return Enhanced Dataframe of synoptic PurpleAir data.
#'
#' @seealso \link{pas_downloadParseRawData_2020}
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
#' pas_raw <- pas_downloadParseRawData_2020()
#'
#' pas_AU <-
#'   pas_enhanceRawData_2020(
#'     pas_raw,
#'     countryCodes = "AU"
#'   ) %>%
#'   dplyr::filter(
#'     DEVICE_LOCATIONTYPE == "outside"
#'   )
#'
#' if ( interactive() )
#'   MazamaLocationUtils::table_leaflet(pas_AU)
#'
#' pas_WA <-
#'   pas_enhanceRawData_2020(
#'     pas_raw,
#'     countryCodes = "US",
#'     stateCodes = "WA"
#'   ) %>%
#'   dplyr::filter(
#'     DEVICE_LOCATIONTYPE == "outside"
#'   )
#'
#' if ( interactive() )
#'   MazamaLocationUtils::table_leaflet(pas_WA)
#'
#' }, silent = FALSE)
#' }

pas_enhanceRawData_2020 <- function(
  pas_raw = NULL,
  countryCodes = NULL,
  stateCodes = NULL
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
  }

  # ----- Harmonize table ------------------------------------------------------

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

  pas <-
    pas_raw %>%

    # * Rename columns -----
  dplyr::rename(
    parentID = .data$ParentID,
    label = .data$Label,
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
  ) %>%

    # * Remove unwanted columns -----
  dplyr::select(-c(
    "Ozone1",        # "Ozone1" has never had any values
    "pm"             # "pm" is redundant with the value in "v"
  )) %>%

    # * Add new columns -----
  dplyr::mutate(
    deviceID = .data$ID,
    sensorManufacturer = "Purple Air"
  ) %>%

    # * Remove invalid locations -----
  dplyr::filter( !is.na(.data$longitude) & !is.na(.data$latitude) ) %>%
    dplyr::filter( .data$longitude >= -180 & .data$longitude <= 180 ) %>%
    dplyr::filter( .data$latitude >= -90 & .data$latitude <= 90 ) %>%

    # * Add core metadata -----
  MazamaLocationUtils::table_addCoreMetadata()

  # ----- Limit to country bounding box ----------------------------------------

  # NOTE:  Most PurpleAir sensors are in the the US (in California).

  if ( !is.null(countryCodes) ) {

    if ( !is.null(stateCodes) && exists("NaturalEarthAdm1") ) {
      SPDF <- get("NaturalEarthAdm1") # To pass R CMD check
      mask <-
        (SPDF@data$countryCode %in% countryCodes) &
        (SPDF@data$stateCode %in% stateCodes)
      bbox <-
        subset(SPDF, mask ) %>%
        sp::bbox()
    } else {
      mask <- MazamaSpatialUtils::SimpleCountriesEEZ@data$countryCode %in% countryCodes
      bbox <-
        subset(MazamaSpatialUtils::SimpleCountriesEEZ, mask) %>%
        sp::bbox()
    }

    pas <-
      pas %>%
      dplyr::filter( .data$longitude >= bbox[1,1] & .data$longitude <= bbox[1,2] ) %>%
      dplyr::filter( .data$latitude >= bbox[2,1] & .data$latitude <= bbox[2,2] )

  }

  # ----- Convert to proper types ----------------------------------------------

  pas$ID <- as.character(pas$ID)
  pas$parentID <- as.character(pas$parentID)
  pas$pm25 <- as.numeric(pas$pm25)
  pas$pm25_current <- as.numeric(pas$pm25_current)
  pas$flag_hidden <- ifelse(pas$flag_hidden == 'true', TRUE, FALSE)
  pas$flag_highValue <- ifelse(pas$flag_highValue == 1, TRUE, FALSE)
  pas$flag_attenuation_hardware <- ifelse(pas$flag_attenuation_hardware == 'true', TRUE, FALSE)
  pas$temperature <- as.numeric(pas$temperature)
  pas$humidity <- as.numeric(pas$humidity)
  pas$pressure <- as.numeric(pas$pressure)

  pas$statsLastModifiedInterval <- pas$statsLastModifiedInterval / 1000   # seconds

  # ----- Convert times to POSIXct ---------------------------------------------

  pas$lastSeenDate <-
    as.POSIXct(
      pas$lastSeenDate,
      tz = "UTC",
      origin = lubridate::origin
    )

  pas$statsLastModifiedDate <-
    as.POSIXct(
      pas$statsLastModifiedDate / 1000,
      tz = "UTC",
      origin = lubridate::origin
    )

  # ----- Replicate fields to B channel ----------------------------------------

  # NOTE:  Both the "DEVICE_LOCATIONTYPE" and "type" fields are present for the
  # NOTE:  A channel but missing from the B channel. This generates confusion
  # NOTE:  when people try to use dplyr to filter for "outside" sensors.
  # NOTE:
  # NOTE:  We also want to have a unique "deviceID" associated with each device.
  # NOTE:  We will replicate the A channel "deviceID" for this purpose.

  A <-
    pas %>%
    # Limit to A channel
    dplyr::filter(is.na(.data$parentID)) %>%
    # Retain columns to be replicated
    dplyr::select(c("ID", "deviceID", "DEVICE_LOCATIONTYPE", "sensorType", "label")) %>%
    # Create locationName
    dplyr::mutate(
      locationName = .data$label
    ) %>%
    dplyr::select(-c("label"))

  B <-
    pas %>%
    # Limit to B channel
    dplyr::filter(!is.na(.data$parentID)) %>%
    # Retain only the A channel ID
    dplyr::select(c("ID", "parentID")) %>%
    # Join, matching parentID to ID
    dplyr::left_join(A, by = c("parentID" = "ID")) %>%
    # Remove parentID
    dplyr::select(-"parentID")

  # Combine to get all rows
  AB <- dplyr::bind_rows(A, B)

  pas <-
    pas %>%
    # Remove the columns to be replicated
    dplyr::select(-c("deviceID", "DEVICE_LOCATIONTYPE", "sensorType", "locationName")) %>%
    # Add the replicated columns found in AB
    dplyr::left_join(AB, by = "ID") %>%

    # Add deviceDeploymentID after deviceID has been sorted out
    dplyr::mutate(
      deviceDeploymentID = paste0(.data$locationID, "_", .data$deviceID)
    )

  # ----- Add spatial metadata -------------------------------------------------

  # Get the unique locations
  uniqueLocations <-
    pas %>%
    dplyr::select(c("locationID", "longitude", "latitude")) %>%
    dplyr::distinct(.data$locationID, .keep_all = TRUE)

  # * countryCode -----
  uniqueLocations$countryCode <-
    MazamaSpatialUtils::getCountryCode(
      longitude = uniqueLocations$longitude,
      latitude = uniqueLocations$latitude,
      countryCodes = countryCodes,
      allData = FALSE,
      useBuffering = FALSE            # No buffering needed with the EEZ dataset
    )

  # Limit to valid countryCodes
  uniqueLocations <-
    uniqueLocations %>%
    dplyr::filter(!is.na(.data$countryCode))

  # * stateCode -----
  # Suppress annoying 'Discarded datum Unknown' messages
  suppressWarnings({
    uniqueLocations$stateCode <-
      MazamaSpatialUtils::getStateCode(
        longitude = uniqueLocations$longitude,
        latitude = uniqueLocations$latitude,
        countryCodes = countryCodes,
        allData = FALSE,
        useBuffering = TRUE
      )
  })

  # Limit to valid stateCodes
  if ( !is.null(stateCodes) ) {
    uniqueLocations <-
      uniqueLocations %>%
      dplyr::filter(.data$stateCode %in% stateCodes)
  }

  # * timezone -----
  # Suppress annoying 'Discarded datum Unknown' messages
  suppressWarnings({
    uniqueLocations$timezone <-
      MazamaSpatialUtils::getTimezone(
        longitude = uniqueLocations$longitude,
        latitude = uniqueLocations$latitude,
        countryCodes = countryCodes,
        allData = FALSE,
        useBuffering = TRUE
      )
  })

  # Limit uniqueLocations to 'locationID' and new variables
  uniqueLocations <-
    uniqueLocations %>%
    dplyr::select(-c("longitude", "latitude"))

  # Add spatial data to 'pas'
  pas <-
    pas %>%
    # Remove empty fields that will be replaced
    dplyr::select(-c("countryCode", "stateCode", "timezone")) %>%
    # Add spatial data
    dplyr::left_join(uniqueLocations, by = "locationID") %>%
    # Limit to requested countries
    dplyr::filter(!is.na(.data$countryCode))

  # ----- Return ---------------------------------------------------------------

  # Guarantee the class name still exists
  class(pas) <- union('pa_synoptic', class(pas))

  return(pas)

}
