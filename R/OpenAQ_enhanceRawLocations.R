#' Enhance OpenAQ locations data
#'
#' Enhances raw location data returned by [openaq::list_locations()] to create
#' a standardized data frame compatible with \pkg{MazamaLocationUtils} and
#' \pkg{AirSensor2} conventions.
#'
#' This function renames variables for clarity, adds spatial and device metadata,
#' and optionally filters the data by country, state, or county. The resulting
#' data frame includes consistent identifiers and metadata fields suitable for
#' downstream analysis and mapping.
#'
#' @param locations_raw Data frame returned by [openaq::list_locations()].
#' @param countryCodes Optional ISO 3166-1 alpha-2 country codes used to subset
#'   the data.
#' @param stateCodes Optional ISO 3166-2 alpha-2 state codes used to subset
#'   the data.
#' @param counties Optional U.S. county names or 5-digit FIPS codes used to
#'   subset the data.
#'
#' @return A data frame of enhanced OpenAQ location data with standardized
#'   variable names, spatial metadata (e.g., timezone, countryCode, stateCode,
#'   countyName), and device metadata (e.g., sensorManufacturer, deviceID,
#'   deviceDeploymentID, locationName).
#'
#' @export
#' @importFrom rlang .data
OpenAQ_enhanceRawLocations <- function(
    locations_raw = NULL,
    countryCodes = NULL,
    stateCodes = NULL,
    counties = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(locations_raw)

  if ( !is.data.frame(locations_raw) ) {
    stop("parameter 'locations_raw' is not a dataframe")
  }

  # Drop any special classes for easier debugging / consistency
  locations <- as.data.frame(locations_raw, stringsAsFactors = FALSE)

  if ( !"longitude" %in% names(locations) ) {
    stop(
      "Parameter 'locations_raw' does not have a 'longitude' column as ",
      "required by OpenAQ_enhanceRawLocations()"
    )
  }

  if ( !"latitude" %in% names(locations) ) {
    stop(
      "Parameter 'locations_raw' does not have a 'latitude' column as ",
      "required by OpenAQ_enhanceRawLocations()"
    )
  }

  if ( !"id" %in% names(locations) ) {
    stop(
      "Parameter 'locations_raw' does not have an 'id' column as ",
      "required by OpenAQ_enhanceRawLocations()"
    )
  }

  if ( !"name" %in% names(locations) ) {
    stop(
      "Parameter 'locations_raw' does not have a 'name' column as ",
      "required by OpenAQ_enhanceRawLocations()"
    )
  }

  # Guarantee uppercase country/state codes
  if ( !is.null(countryCodes) ) {
    countryCodes <- toupper(countryCodes)

    if ( any(!(countryCodes %in% countrycode::codelist$iso2c)) ) {
      stop(
        "parameter 'countryCodes' has values that are not recognized as ",
        "ISO-2 country codes"
      )
    }
  }

  if ( !is.null(stateCodes) ) {
    if ( is.null(countryCodes) ) {
      stop(
        "'stateCodes' can only be used when also specifying a single country ",
        "with 'countryCodes'"
      )
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

  # > dplyr::glimpse(locations, width = 75)
  # Rows: 326
  # Columns: 15
  # $ id             <int> 424, 1153, 1250, 1255, 1657, 1658, 1660, 1856, 196…
  # $ name           <chr> "CHI_COM", "ALSIP", "LEMONT", "LISLE", "CHI_SP", "…
  # $ is_mobile      <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, F…
  # $ is_monitor     <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TR…
  # $ timezone       <fct> America/Chicago, America/Chicago, America/Chicago,…
  # $ countries_id   <dbl> 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, …
  # $ country_name   <chr> "United States", "United States", "United States",…
  # $ country_iso    <fct> US, US, US, US, US, US, US, US, US, US, US, US, US…
  # $ latitude       <dbl> 41.75470, 41.67080, 41.66810, 41.81310, 41.91360, …
  # $ longitude      <dbl> -87.71360, -87.73250, -87.99060, -88.07280, -87.72…
  # $ datetime_first <dttm> 2016-03-06 20:00:00, 2016-03-06 20:00:00, 2016-03…
  # $ datetime_last  <dttm> 2026-04-20 15:00:00, 2026-04-20 15:00:00, 2026-04…
  # $ owner_name     <fct> Unknown Governmental Organization, Unknown Governm…
  # $ providers_id   <dbl> 119, 119, 119, 119, 119, 119, 119, 119, 119, 119, …
  # $ provider_name  <chr> "AirNow", "AirNow", "AirNow", "AirNow", "AirNow", …

  # Normalize common fields / types to prepare for 'monitor' creation
  locations <-
    locations %>%
    dplyr::mutate(

      # Human-readable / AirSensor2-style names
      locationName = .data$name,

      # TODO:  Verify that locations_id can be used as a device identifier

      # OpenAQ location id as a stable device identifier
      deviceID = paste0("openaq.", .data$id),

      # Harmonize booleans to more readable strings where useful
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

  # NOTE: precision = 10 results in a precision of ~1 meters
  locations <-
    locations %>%
    MazamaLocationUtils::table_addCoreMetadata(precision = 10) %>%
    dplyr::mutate(
      deviceDeploymentID = paste0(.data$locationID, "_", .data$deviceID)
    )

  # TODO:  Remove this when AirMonitor and AirMonitorPlots no longer require zip
  locations$zip <- as.character(NA)

  # Put identifiers in front
  startingIDs <- c("deviceDeploymentID", "deviceID", "locationID")
  otherColumns <- setdiff(names(locations), startingIDs)
  orderedColumns <- c(startingIDs, otherColumns)
  locations <- locations %>% dplyr::select(dplyr::all_of(orderedColumns))

  # ----- Apply requested country filter --------------------------------------

  # If we already have countryCode, use it first
  if ( !is.null(countryCodes) ) {
    locations <-
      locations %>%
      dplyr::filter(.data$countryCode %in% countryCodes)
  }

  # ----- Add / refine spatial metadata ---------------------------------------

  # stateCode
  suppressWarnings({
    locations$stateCode <-
      MazamaSpatialUtils::getStateCode(
        longitude = locations$longitude,
        latitude = locations$latitude,
        countryCodes = countryCodes,
        allData = FALSE,
        useBuffering = TRUE
      )
  })

  if ( !is.null(stateCodes) ) {
    locations <-
      locations %>%
      dplyr::filter(.data$stateCode %in% stateCodes)
  }

  # countyName (US only)
  if ( !is.null(countryCodes) && length(countryCodes) == 1 && countryCodes[1] == "US" ) {

    suppressWarnings({
      locations$countyName <-
        MazamaSpatialUtils::getUSCounty(
          longitude = locations$longitude,
          latitude = locations$latitude,
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

      locations <-
        locations %>%
        dplyr::filter(.data$countyName %in% counties)
    }

  }

  # timezone
  # OpenAQ already provides timezone, but fill any missing values spatially
  if ( !"timezone" %in% names(locations) ) {
    locations$timezone <- NA_character_
  }

  missingTimezone <- is.na(locations$timezone) | locations$timezone == ""

  if ( any(missingTimezone) ) {
    suppressWarnings({
      locations$timezone[missingTimezone] <-
        MazamaSpatialUtils::getTimezone(
          longitude = locations$longitude[missingTimezone],
          latitude = locations$latitude[missingTimezone],
          countryCodes = countryCodes,
          allData = FALSE,
          useBuffering = TRUE
        )
    })
  }

  # countryCode
  # If any are missing, derive spatially
  if ( !"countryCode" %in% names(locations) ) {
    locations$countryCode <- NA_character_
  }

  missingCountry <- is.na(locations$countryCode) | locations$countryCode == ""

  if ( any(missingCountry) ) {
    locations$countryCode[missingCountry] <-
      MazamaSpatialUtils::getCountryCode(
        longitude = locations$longitude[missingCountry],
        latitude = locations$latitude[missingCountry],
        countryCodes = countryCodes,
        allData = FALSE,
        useBuffering = FALSE
      )
  }

  # Limit to valid countryCodes after fill, if requested
  if ( !is.null(countryCodes) ) {
    locations <-
      locations %>%
      dplyr::filter(!is.na(.data$countryCode)) %>%
      dplyr::filter(.data$countryCode %in% countryCodes)
  }

  # ----- Extras ---------------------------------------------------------------

  # Add start and end date strings for use with MazamaLocationUtils::table_leaflet()
  locations <-
    locations %>%
    dplyr::mutate(
      start = format(.data$datetime_first, "%b %d, %Y"),
      end   = format(.data$datetime_last,  "%b %d, %Y")
    )

  # ----- Return ---------------------------------------------------------------

  return(locations)

}

