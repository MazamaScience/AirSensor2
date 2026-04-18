#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.isInitialized logger.debug
#' @importFrom MazamaCoreUtils getAPIKey
#'
#' @title Create a new OpenAQ locations dataset
#'
#' @description Download, parse and enhance locations data from OpenAQ and
#' return the results as a plain tibble.
#'
#' Steps include:
#'
#' 1) Download and parse locations data
#'
#' 2) Add spatial metadata for each location including:
#' \itemize{
#'   \item{timezone -- olson timezone}
#'   \item{countryCode -- ISO 3166-1 alpha-2}
#'   \item{stateCode -- ISO 3166-2 alpha-2}
#' }
#'
#' @note
#' Valid values for `countryCodes`, `providers` and `manufacturers` can be obtained with:
#'
#' \preformatted{
#' OpenAQ_getCountries()
#' OpenAQ_getProviders()
#' OpenAQ_getManufacturers()
#' }
#'
#' These functions return reference tables containing the available values.
#' Matching is case-insensitive. Unmatched values are ignored.
#'
#' @param api_key OpenAQ API READ Key. If `api_key = NULL`, it
#' will be obtained using `getAPIKey("OPENAQ")`.
#' @param countryCodes ISO 3166-1 alpha-2 country codes used to subset the data.
#' At least one countryCode must be specified. (Country names are also acceptible.)
#' @param stateCodes ISO-3166-2 alpha-2 state codes used to subset the data.
#' Specifying stateCodes is optional.
#' @param counties US county names or 5-digit FIPS codes used to subset the data.
#' Specifying counties is optional.
#' @param lookbackDays Number of days to "look back" for locations with valid data. Locations are
#' filtered to only include those with data more recent than `lookbackDays` ago.
#' Use `lookbackDays = 0` to get all historical locations.
#' @param providers Character vector of provider names or export prefixes used to subset the data. See note.
#' @param manufacturers Character vector of manufacturer names used to subset the data. See note.
#' @param monitor A logical to filter results to regulatory monitors (TRUE) or air sensors (FALSE), both are included if NULL.
#' @param limit An integer specifying the maximum number of results to return, max is 1000.
#'
#' @return A tibble of location metadata.
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
#' locations <-
#'   OpenAQ_createLocations(
#'     countryCodes = "US",
#'     stateCodes = "IL",
#'     counties = "Cook",
#'     api_key = OPENAQ_API_KEY
#'   ) %>%
#'   dplyr::mutate(
#'     start = format(.data$datetime_first, "%b %-d, %Y"),
#'     end   = format(.data$datetime_last,  "%b %-d, %Y")
#'   )
#'
#' clarity <- locations %>% dplyr::filter(provider_name == "Clarity")
#' map <-
#'   MazamaLocationUtils::table_leaflet(
#'     clarity,
#'     extraVars = c("deviceDeploymentID", "start", "end", "owner_name", "provider_name"),
#'     radius = 5, fillColor = "blue"
#'   )
#'
#' airnow <- locations %>% dplyr::filter(provider_name == "AirNow")
#' map <- MazamaLocationUtils::table_leafletAdd(
#'     map,
#'     airnow,
#'     extraVars = c("deviceDeploymentID", "start", "end", "owner_name", "provider_name"),
#'     radius = 10, color = "black", fillColor = "black"
#'   )
#'
#' airgradient <- locations %>% dplyr::filter(provider_name == "AirGradient")
#' map <- MazamaLocationUtils::table_leafletAdd(
#'     map,
#'     airgradient,
#'     extraVars = c("deviceDeploymentID", "start", "end", "owner_name", "provider_name"),
#'     radius = 5, color = "orange", fillColor = "orange"
#'   )
#'
#' print(map)
#'
#' }, silent = FALSE)
#' }

OpenAQ_createLocations <- function(
    api_key = NULL,
    countryCodes = NULL,
    stateCodes = NULL,
    counties = NULL,
    lookbackDays = 1,
    providers = NULL,
    manufacturers = NULL,
    monitor = NULL,
    limit = 1000
) {

  # ----- Validate parameters --------------------------------------------------

  if ( is.null(api_key) )
    api_key <- MazamaCoreUtils::getAPIKey("OpenAQ")

  lookbackDays <- MazamaCoreUtils::setIfNull(lookbackDays, 1)
  limit <- MazamaCoreUtils::setIfNull(limit, 1000)
  if ( limit > 1000 ) {
    message("Limit cannot be greater than 1000. Resetting to 1000.")
    limit <- 1000
  }

  if ( !is.null(countryCodes) ) {

    # Guarantee uppercase codes
    countryCodes <- toupper(countryCodes)

    # NOTE:  stateCodes are optional

    # Validate countryCodes
    if ( any(!(countryCodes %in% countrycode::codelist$iso2c)) )
      stop("parameter 'countryCodes' has values that are not recognized as ISO-2 country codes")

    if ( !is.null(stateCodes) ) {
      if ( length(countryCodes) != 1 ) {
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

  # ----- countries_id ---------------------------------------------------------

  countries_id <- NULL

  if ( !is.null(countryCodes) ) {
    countries_id <- OpenAQ_countryToID(countryCodes)
    countries_id <- unique(countries_id[!is.na(countries_id)])
  }

  # ----- bbox -----------------------------------------------------------------

  bbox <- NULL

  if ( !is.null(stateCodes) ) {

    if ( logger.isInitialized() )
      logger.debug("----- create bounding box -----")

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

    }

    bbox <- sf::st_bbox(SFDF)

  }

  # ----- providers_id ---------------------------------------------------------

  providers_id <- NULL
  if ( !is.null(providers) ) {
    providers_id <- OpenAQ_providerToID(providers)
    providers_id <- unique(providers_id[!is.na(providers_id)])
  }

  # ----- manufacturers_id -----------------------------------------------------

  manufacturers_id <- NULL
  if ( !is.null(manufacturers) ) {
    manufacturers_id <- OpenAQ_manufacturerToID(manufacturers)
    manufacturers_id <- unique(manufacturers_id[!is.na(manufacturers_id)])
  }

  # ----- Load data ------------------------------------------------------------

  openaq_raw <-
    OpenAQ_downloadRawLocations(
      api_key = api_key,
      bbox = bbox,
      providers_id = providers_id,
      manufacturers_id = manufacturers_id,
      monitor = monitor,
      mobile = FALSE,
      countries_id = countries_id,
      limit = limit,
      maxPages = 10,
      sleepSeconds = 0.5
    )

  # ----- Filter ---------------------------------------------------------------

  if ( !is.null(lookbackDays) && lookbackDays > 0 ) {

    cutoff <- lubridate::now("UTC") - lubridate::days(lookbackDays)

    openaq_raw <-
      openaq_raw %>%
      dplyr::filter(.data$datetime_last >= cutoff)

  }

  # ----- Enhance --------------------------------------------------------------

  locations <-
    OpenAQ_enhanceRawLocations(
      openaq_raw,
      countryCodes = countryCodes,
      stateCodes = stateCodes,
      counties = counties
    )

  # ----- Return ---------------------------------------------------------------

  return(locations)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  library(AirSensor2)
  initializeMazamaSpatialUtils()

  library(dotenv)
  dotenv::load_dot_env()

  Sys.getenv("OPENAQ_API_KEY")

  OPENAQ_API_KEY <- Sys.getenv("OPENAQ_API_KEY")

  MazamaCoreUtils::setAPIKey("OpenAQ", Sys.getenv("OPENAQ_API_KEY"))


  api_key = NULL
  countryCodes = c("US")
  stateCodes = "CA"
  counties = NULL
  lookbackDays = 1
  providers = NULL
  manufacturers = NULL
  monitor = NULL
  limit = 1000


  df <-
    OpenAQ_createLocations(
      api_key = NULL,
      countryCodes = countryCodes,
      stateCodes = stateCodes,
      counties = counties,
      lookbackDays = lookbackDays,
      providers = NULL,
      manufacturers = NULL,
      monitor = NULL,
      limit = 1000
    )


}
