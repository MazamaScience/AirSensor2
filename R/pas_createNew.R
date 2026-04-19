#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.isInitialized logger.debug
#'
#' @title Create a new PurpleAir synoptic dataset
#'
#' @description Download, parse and enhance synoptic data from PurpleAir and
#' return the results as a useful tibble with class `PurpleAir_synoptic`.
#'
#' Steps include:
#'
#' 1) Download and parse synoptic data
#'
#' 2) Replace variable names with more consistent, more human readable names.
#'
#' 3) Add spatial metadata for each sensor including:
#' \itemize{
#'   \item{timezone -- olson timezone}
#'   \item{countryCode -- ISO 3166-1 alpha-2}
#'   \item{stateCode -- ISO 3166-2 alpha-2}
#' }
#'
#' 4) Convert data types from character to `POSIXct` and `numeric`.
#'
#' Data requests are made with a bounding box determined from the required
#' `countryCodes` parameter. If a single country is specified and additional
#' `stateCodes` are specified, the bounding box will be limited to those
#' states. Withing a single state, `counties` may be used to further limit
#' the sensors requested.
#'
#' If `show_only` is used to request specific sensors, the `countryCodes`
#' and `stateCodes` information is ignored when requesting data. But these
#' are still used to help speed up the assignment of enhanced metadata..
#'
#' @param api_key PurpleAir API Read Key.
#' @param fields Character string with PurpleAir field names for the Get Sensor Data API.
#' @param countryCodes ISO 3166-1 alpha-2 country codes used to subset the data.
#' At least one countryCode must be specified.
#' @param stateCodes ISO-3166-2 alpha-2 state codes used to subset the data.
#' Specifying stateCodes is optional.
#' @param counties US county names or 5-digit FIPS codes used to subset the data.
#' Specifying counties is optional.
#' @param lookbackDays Number of days to "look back" for valid data. Data are
#' filtered to only include sensors with data more recent than `lookbackDays` ago.
#' Use `lookbackDays = 0` to get all historical sensors.
#' @param location_type The `location_type` of the sensors. Possible values
#' are: 0 = Outside, 1 = Inside or `NULL` = both.
#' @param read_keys Optional, comma separated list of sensor read_keys is required
#' for private devices. It is separate from the api_key and each sensor has its own
#' read_key. Submit multiple keys by separating them with a comma (,) character
#' for example: `"key-one,key-two,key-three"`.
#' @param show_only Optional, comma separated list of sensor_index values. When
#' provided, results are requested only for the specified sensors.
#' @param baseUrl Base URL for the PurpleAir API.
#'
#' @return A PurpleAir Synoptic *pas* object.
#'
#' @note The `fields` parameter allows users to dial in which fields they
#' are interested in depending on their needs. However, the following fields
#' will be added if not specified in order to guarantee compatibility with
#' `pas_enhanceRawData()`:
#' `"longitude,latitude,name,location_type,date_created,last_seen"`.
#'
#' Pregenerated fields for use in this function include:
#' \itemize{
#'   \item{[PurpleAir_PAS_MINIMAL_FIELDS]} -- minimal set of fields
#'   \item{[PurpleAir_PAS_METADATA_FIELDS]} -- instrument-only fields
#'   \item{[PurpleAir_PAS_AVG_PM25_FIELDS]} -- includes measurements
#' }
#'
#' @seealso [pas_downloadParseRawData()]
#' @seealso [pas_enhanceRawData()]
#'
#' @references [PurpleAir](https://www2.purpleair.com)
#' @references [PurpleAir API](https://api.purpleair.com/)
#' @references [PurpleAir Terms of service](https://www2.purpleair.com/policies/terms-of-service)
#' @references [PurpleAir Data license](https://www2.purpleair.com/pages/license)
#' @references [PurpleAir Data Attribution](https://www2.purpleair.com/pages/attribution)
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
#' source("global_vars.R") # contains PurpleAir_API_READ_KEY
#'
#' pas <-
#'   pas_createNew(
#'     api_key = PurpleAir_API_READ_KEY,
#'     countryCodes = "US",
#'     stateCodes = "WA",
#'     counties = c("Okanogan"),
#'     lookbackDays = 0, # all historical sensors
#'     location_type = 0
#'   )
#'
#' }, silent = FALSE)
#' }

pas_createNew <- function(
    api_key = NULL,
    fields = PurpleAir_PAS_MINIMAL_FIELDS,
    countryCodes = NULL,
    stateCodes = NULL,
    counties = NULL,
    lookbackDays = 1,
    location_type = 0,
    read_keys = NULL,
    show_only = NULL,
    baseUrl = "https://api.purpleair.com/v1/sensors"
) {

  # ----- Validate parameters --------------------------------------------------

  lookbackDays <- MazamaCoreUtils::setIfNull(lookbackDays, 1)
  MazamaCoreUtils::stopIfNull(baseUrl)

  # NOTE:  if show_only is used, countryCodes are optional and only used to speed
  # NOTE:  up assignment of spatial metadata.

  if ( is.null(show_only) ) {
    MazamaCoreUtils::stopIfNull(countryCodes)
  }

  if ( !is.null(countryCodes) ) {

    # Guarantee uppercase codes
    MazamaCoreUtils::stopIfNull(countryCodes)
    countryCodes <- toupper(countryCodes)

    # NOTE:  stateCodes are optional

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

    # Check if MazamaSpatialUtils package has been initialized
    # via initializeMazamaSpatialUtils()
    if ( !spatialIsInitialized() ) {
      stop('`pas_createNew` requires MazamaSpatialUtils to be initialized:

            initializeMazamaSpatialUtils()

         Please see `?initializeMazamaSpatialUtils for more details.')
    }

  }

  # Guarantee fields needed for enhancement
  fields <-
    fields %>%
    stringr::str_split_1(",") %>%
    union(PurpleAir_PAS_MINIMAL_FIELDS) %>%
    paste0(collapse = ",")

  # ----- Get country/state bounding box ---------------------------------------

  if ( !is.null(show_only) ) {

    west <- NULL
    north <- NULL
    east <- NULL
    south <- NULL

  } else {

    if ( logger.isInitialized() )
      logger.debug("----- create bounding box -----")

    # NOTE:  Most PurpleAir sensors are in the the US (in California).

    if ( !is.null(stateCodes) && exists("NaturalEarthAdm1") ) {

      if ( !is.null(counties) && exists("USCensusCounties") ) {

        counties <- as.character(counties)
        isFIPS <- stringr::str_detect(counties[1], "[0-9]{5}")

        if ( isFIPS ) {
          SFDF <-
            get("USCensusCounties") %>%  # To pass R CMD check
            dplyr::filter(.data$stateCode %in% stateCodes) %>%
            dplyr::filter(.data$countyFIPS %in% counties)
        } else {
          # Handle input inconsistencies
          counties <-
            stringr::str_to_title(counties) %>%
            stringr::str_replace(" County", "")
          SFDF <-
            get("USCensusCounties") %>%  # To pass R CMD check
            dplyr::filter(.data$stateCode %in% stateCodes) %>%
            dplyr::filter(.data$countyName %in% counties)
        }

      } else {

        # Use state but not counties
        SFDF <-
          get("NaturalEarthAdm1") %>% # To pass R CMD check
          dplyr::filter(.data$countryCode %in% countryCodes) %>%
          dplyr::filter(.data$stateCode %in% stateCodes)

      }

    } else {

      # Neither state nor county is specified
      SFDF <-
        MazamaSpatialUtils::SimpleCountriesEEZ %>%
        dplyr::filter(.data$countryCode %in% countryCodes)

    }

    bbox <- sf::st_bbox(SFDF)

    west <- as.numeric(bbox$xmin)
    east <- as.numeric(bbox$xmax)
    south <- as.numeric(bbox$ymin)
    north <- as.numeric(bbox$ymax)

  }

  # ----- Load data ------------------------------------------------------------

  # Download, parse and enhance synoptic data
  if ( logger.isInitialized() )
    logger.debug("----- pas_downloadParseRawData() -----")

  pas_raw <-
    pas_downloadParseRawData(
      api_key = api_key,
      fields = fields,
      location_type = location_type,
      read_keys = read_keys,
      show_only = show_only,
      modified_since = NULL, # get all data more recent than max_age
      max_age = lookbackDays * 24 * 3600,
      west = west,
      east = east,
      south = south,
      north = north,
      baseUrl = baseUrl
    )

  if ( logger.isInitialized() )
    logger.debug("----- pas_enhanceRawData() -----")

  pas <-
    pas_enhanceRawData(
      pas_raw,
      countryCodes = countryCodes,
      stateCodes = stateCodes,
      counties = counties
    )

  if ( logger.isInitialized() )
    logger.debug("----- finished enhancing -----")

  # ----- Return ---------------------------------------------------------------

  # Add a class name
  class(pas) <- union("PurpleAir_synoptic", class(pas))

  return(pas)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  countryCodes = "US"
  stateCodes = c("WA")
  counties <- NULL
  lookbackDays = 1
  baseUrl = "https://api.purpleair.com/v1/sensors"


  pas_raw <- example_pas_raw

}
