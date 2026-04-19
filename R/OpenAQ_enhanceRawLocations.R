#' @export
#' @importFrom rlang .data
#'
#' @title Enhance locations data from OpenAQ
#'
#' @description Enhance raw location data from OpenAQ to create an
#' improved dataframe compatible with the \pkg{MazamaLocationUtils} package and
#' AirSensor2 conventions.
#'
#' Steps include:
#'
#' 1) Replace variable names with more consistent, human readable names.
#'
#' 2) Add spatial metadata for each location including:
#' \itemize{
#'   \item{timezone -- Olson timezone}
#'   \item{countryCode -- ISO 3166-1 alpha-2}
#'   \item{stateCode -- ISO 3166-2 alpha-2 when available}
#'   \item{countyName -- US county name when appropriate}
#' }
#'
#' 3) Add additional metadata items:
#' \itemize{
#'   \item{sensorManufacturer}
#'   \item{deviceID}
#'   \item{deviceDeploymentID}
#'   \item{locationName}
#' }
#'
#' 4) Filter by countryCodes, stateCodes, and counties when requested.
#'
#' @param openaq_raw Dataframe returned by [openaq::list_locations()].
#' @param countryCodes ISO 3166-1 alpha-2 country codes used to subset the data.
#' @param stateCodes ISO-3166-2 alpha-2 state codes used to subset the data.
#' @param counties US county names or 5-digit FIPS codes used to subset the data.
#'
#' @return Enhanced dataframe of location OpenAQ data.
#'

OpenAQ_enhanceRawLocations <- function(
    openaq_raw = NULL,
    countryCodes = NULL,
    stateCodes = NULL,
    counties = NULL
) {

  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(openaq_raw)

  if ( !is.data.frame(openaq_raw) ) {
    stop("parameter 'openaq_raw' is not a dataframe")
  }

  # Drop any special classes for easier debugging / consistency
  openaq <- as.data.frame(openaq_raw, stringsAsFactors = FALSE)

  if ( !"longitude" %in% names(openaq) ) {
    stop("Parameter 'openaq_raw' does not have a 'longitude' column as required by OpenAQ_enhanceRawData()")
  }

  if ( !"latitude" %in% names(openaq) ) {
    stop("Parameter 'openaq_raw' does not have a 'latitude' column as required by OpenAQ_enhanceRawData()")
  }

  if ( !"id" %in% names(openaq) ) {
    stop("Parameter 'openaq_raw' does not have an 'id' column as required by OpenAQ_enhanceRawData()")
  }

  if ( !"name" %in% names(openaq) ) {
    stop("Parameter 'openaq_raw' does not have a 'name' column as required by OpenAQ_enhanceRawData()")
  }

  # Guarantee uppercase country/state codes
  if ( !is.null(countryCodes) ) {
    countryCodes <- toupper(countryCodes)

    if ( any(!(countryCodes %in% countrycode::codelist$iso2c)) ) {
      stop("parameter 'countryCodes' has values that are not recognized as ISO-2 country codes")
    }
  }

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

  # ----- Harmonize table ------------------------------------------------------

  # Normalize common fields / types
  openaq <-
    openaq %>%
    dplyr::mutate(

      # Human-readable / AirSensor2-style names
      locationName = .data$name,

      # OpenAQ location id as a stable device identifier
      deviceID = paste0("openaq.", .data$id),

      # Consistent manufacturer/source field
      ###sensorManufacturer = "OpenAQ", # TODO: could figure it out with table queries

      # Harmonize booleans to more readable strings where useful
      ###mobile = dplyr::if_else(.data$is_mobile, "mobile", "stationary", missing = NA_character_),
      deviceType = dplyr::if_else(.data$is_monitor, "Monitor", "Sensor", missing = NA_character_),

      # Prefer character vectors over factors
      timezone = as.character(.data$timezone),
      countryCode = as.character(.data$country_iso),
      provider_name = as.character(.data$provider_name),
      owner_name = as.character(.data$owner_name),

      # Keep timestamps as POSIXct
      datetime_first = as.POSIXct(.data$datetime_first, tz = "UTC"),
      datetime_last  = as.POSIXct(.data$datetime_last,  tz = "UTC")
    )

  # ----- Add core metadata ----------------------------------------------------

  # NOTE: precision = 9 results in a precision of ~2 meters
  openaq <-
    openaq %>%
    MazamaLocationUtils::table_addCoreMetadata(precision = 9) %>%
    dplyr::mutate(
      deviceDeploymentID = paste0(.data$locationID, "_", .data$deviceID)
    )

  # Put identifiers in front
  startingIDs <- c("deviceDeploymentID", "deviceID", "locationID")
  otherColumns <- setdiff(names(openaq), startingIDs)
  orderedColumns <- c(startingIDs, otherColumns)
  openaq <- openaq %>% dplyr::select(dplyr::all_of(orderedColumns))

  # ----- Apply requested country filter --------------------------------------

  # If OpenAQ already provides country_iso, use it first
  if ( !is.null(countryCodes) ) {
    openaq <-
      openaq %>%
      dplyr::filter(.data$countryCode %in% countryCodes)
  }

  # ----- Add / refine spatial metadata ---------------------------------------

  # stateCode
  suppressWarnings({
    openaq$stateCode <-
      MazamaSpatialUtils::getStateCode(
        longitude = openaq$longitude,
        latitude = openaq$latitude,
        countryCodes = countryCodes,
        allData = FALSE,
        useBuffering = TRUE
      )
  })

  if ( !is.null(stateCodes) ) {
    openaq <-
      openaq %>%
      dplyr::filter(.data$stateCode %in% stateCodes)
  }

  # countyName (US only)
  if ( !is.null(countryCodes) && length(countryCodes) == 1 && countryCodes[1] == "US" ) {

    suppressWarnings({
      openaq$countyName <-
        MazamaSpatialUtils::getUSCounty(
          longitude = openaq$longitude,
          latitude = openaq$latitude,
          stateCodes = stateCodes,
          allData = FALSE,
          useBuffering = TRUE
        )
    })

    if ( !is.null(counties) ) {
      counties <- as.character(counties)

      # Convert from FIPS to county name if needed
      if ( stringr::str_detect(counties[1], "^[0-9]{5}$") ) {
        counties <-
          MazamaSpatialUtils::US_countyFIPSToName(
            state = stateCodes,
            countyFIPS = counties
          )
      }

      counties <-
        counties %>%
        stringr::str_to_title() %>%
        stringr::str_replace(" County$", "")

      openaq <-
        openaq %>%
        dplyr::filter(.data$countyName %in% counties)
    }

  }

  # timezone
  # OpenAQ already provides timezone, but fill any missing values spatially
  if ( !"timezone" %in% names(openaq) ) {
    openaq$timezone <- NA_character_
  }

  missingTimezone <- is.na(openaq$timezone) | openaq$timezone == ""

  if ( any(missingTimezone) ) {
    suppressWarnings({
      openaq$timezone[missingTimezone] <-
        MazamaSpatialUtils::getTimezone(
          longitude = openaq$longitude[missingTimezone],
          latitude = openaq$latitude[missingTimezone],
          countryCodes = countryCodes,
          allData = FALSE,
          useBuffering = TRUE
        )
    })
  }

  # countryCode
  # If any are missing, derive spatially
  if ( !"countryCode" %in% names(openaq) ) {
    openaq$countryCode <- NA_character_
  }

  missingCountry <- is.na(openaq$countryCode) | openaq$countryCode == ""

  if ( any(missingCountry) ) {
    openaq$countryCode[missingCountry] <-
      MazamaSpatialUtils::getCountryCode(
        longitude = openaq$longitude[missingCountry],
        latitude = openaq$latitude[missingCountry],
        countryCodes = countryCodes,
        allData = FALSE,
        useBuffering = FALSE
      )
  }

  # Limit to valid countryCodes after fill, if requested
  if ( !is.null(countryCodes) ) {
    openaq <-
      openaq %>%
      dplyr::filter(!is.na(.data$countryCode)) %>%
      dplyr::filter(.data$countryCode %in% countryCodes)
  }

  # ----- Extras ---------------------------------------------------------------

  # Add start and end date strings
  openaq <-
    openaq %>%
      dplyr::mutate(
        start = format(.data$datetime_first, "%b %d, %Y"),
        end   = format(.data$datetime_last,  "%b %d, %Y")
      )

  # ----- Return ---------------------------------------------------------------

  return(openaq)

}
