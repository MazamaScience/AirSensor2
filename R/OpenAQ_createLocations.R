#' Create an OpenAQ locations dataset
#'
#' Downloads location metadata from OpenAQ and returns an enhanced data frame
#' suitable for analysis and mapping with \pkg{MazamaLocationUtils}.
#'
#' This function can filter locations by country, state, county, provider,
#' manufacturer, and monitor type. It can also limit results to locations with
#' recent data using `lookbackDays`. Returned data are enhanced with standardized
#' identifiers and spatial metadata such as timezone, countryCode, and stateCode.
#'
#' Valid values for `countryCodes`, `providers`, and `manufacturers` can be
#' explored with [OpenAQ_getCountries()], [OpenAQ_getProviders()], and
#' [OpenAQ_getManufacturers()].
#'
#' @param countryCodes Optional ISO 3166-1 alpha-2 country codes used to subset
#'   the data.
#' @param stateCodes Optional ISO 3166-2 alpha-2 state codes used to subset
#'   the data.
#' @param counties Optional U.S. county names or 5-digit FIPS codes used to
#'   subset the data.
#' @param lookbackDays Number of days to look back for locations with recent
#'   data. Use `lookbackDays = 0` to include all historical locations.
#' @param providers Optional character vector of OpenAQ provider names or export
#'   prefixes used to subset the data.
#' @param manufacturers Optional character vector of manufacturer names used to
#'   subset the data.
#' @param is_monitor Optional logical used to filter results to regulatory
#'   monitors (`TRUE`) or air sensors (`FALSE`). If `NULL`, both are included.
#' @param limit Maximum number of locations to request per API call. Values
#'   greater than 1000 are reset to 1000.
#' @param api_key OpenAQ API read key. If `NULL`, the key is obtained with
#'   `MazamaCoreUtils::getAPIKey("OPENAQ")`.
#'
#' @return A data frame of enhanced OpenAQ location metadata.
#'
#' @examples
#' \donttest{
#' try({
#'   if (interactive()) {
#'     initializeMazamaSpatialUtils()
#'
#'     # NOTE:  Read environment vars from .env file with dotenv::load_dot_env()
#'     OPENAQ_API_KEY <- Sys.getenv("OPENAQ_API_KEY")
#'
#'     locations <-
#'       OpenAQ_createLocations(
#'         countryCodes = "US",
#'         stateCodes = "IL",
#'         counties = "Cook",
#'         api_key = OPENAQ_API_KEY
#'       )
#'
#'     table(locations$provider_name)
#'
#'     clarity <- locations %>% dplyr::filter(provider_name == "Clarity")
#'     airnow <- locations %>% dplyr::filter(provider_name == "AirNow")
#'     airgradient <- locations %>% dplyr::filter(provider_name == "AirGradient")
#'
#'     map <-
#'       MazamaLocationUtils::table_leaflet(
#'         clarity,
#'         extraVars = c("deviceDeploymentID", "start", "end", "owner_name", "provider_name"),
#'         radius = 5, fillColor = "blue"
#'       )
#'
#'     map <- MazamaLocationUtils::table_leafletAdd(
#'       map,
#'       airnow,
#'       extraVars = c("deviceDeploymentID", "start", "end", "owner_name", "provider_name"),
#'       radius = 10, fillColor = "black"
#'     )
#'
#'     map <- MazamaLocationUtils::table_leafletAdd(
#'       map,
#'       airgradient,
#'       extraVars = c("deviceDeploymentID", "start", "end", "owner_name", "provider_name"),
#'       radius = 5, fillColor = "red"
#'     )
#'
#'     print(map)
#'   }
#' }, silent = FALSE)
#' }
#'
#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.isInitialized logger.debug
#' @importFrom MazamaCoreUtils getAPIKey
OpenAQ_createLocations <- function(
    countryCodes = NULL,
    stateCodes = NULL,
    counties = NULL,
    lookbackDays = 1,
    providers = NULL,
    manufacturers = NULL,
    is_monitor = NULL,
    limit = 1000,
    api_key = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  if ( is.null(api_key) )
    api_key <- MazamaCoreUtils::getAPIKey("OPENAQ")

  lookbackDays <- MazamaCoreUtils::setIfNull(lookbackDays, 1)
  limit <- MazamaCoreUtils::setIfNull(limit, 1000)

  if ( limit > 1000 ) {
    message("Limit cannot be greater than 1000. Resetting to 1000.")
    limit <- 1000
  }

  if ( !is.null(countryCodes) ) {

    # Guarantee uppercase codes
    countryCodes <- toupper(countryCodes)

    # Validate countryCodes
    if ( any(!(countryCodes %in% countrycode::codelist$iso2c)) ) {
      stop("parameter 'countryCodes' has values that are not recognized as ISO-2 country codes")
    }

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
    if ( !spatialIsInitialized() ) {
      stop(
        "`OpenAQ_createLocations()` requires MazamaSpatialUtils to be initialized:\n\n",
        "  initializeMazamaSpatialUtils()\n\n",
        "Please see `?initializeMazamaSpatialUtils` for more details."
      )
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
          counties <-
            stringr::str_to_title(counties) %>%
            stringr::str_replace(" County", "")

          SFDF <-
            get("USCensusCounties") %>%  # To pass R CMD check
            dplyr::filter(.data$stateCode %in% stateCodes) %>%
            dplyr::filter(.data$countyName %in% counties)
        }

      } else {

        SFDF <-
          get("NaturalEarthAdm1") %>%  # To pass R CMD check
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

  locations_raw <-
    OpenAQ_downloadRawLocations(
      api_key = api_key,
      bbox = bbox,
      providers_id = providers_id,
      manufacturers_id = manufacturers_id,
      monitor = is_monitor,
      mobile = FALSE,
      countries_id = countries_id,
      limit = limit,
      maxPages = 10,
      sleepSeconds = 0.5
    )

  # ----- Filter ---------------------------------------------------------------

  if ( !is.null(lookbackDays) && lookbackDays > 0 ) {

    cutoff <- lubridate::now("UTC") - lubridate::days(lookbackDays)

    locations_raw <-
      locations_raw %>%
      dplyr::filter(.data$datetime_last >= cutoff)

  }

  # ----- Enhance --------------------------------------------------------------

  locations <-
    OpenAQ_enhanceRawLocations(
      locations_raw,
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

  MazamaCoreUtils::setAPIKey("OPENAQ", Sys.getenv("OPENAQ_API_KEY"))

  api_key <- NULL
  countryCodes <- c("US")
  stateCodes <- "IL"
  counties <- "Cook"
  lookbackDays <- 1
  providers <- NULL
  manufacturers <- NULL
  is_monitor <- NULL
  limit <- 1000

  locations <-
    OpenAQ_createLocations(
      api_key = NULL,
      countryCodes = countryCodes,
      stateCodes = stateCodes,
      counties = counties,
      lookbackDays = lookbackDays,
      providers = NULL,
      manufacturers = NULL,
      is_monitor = NULL,
      limit = 1000
    )

}
