#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.isInitialized logger.debug
#' @importFrom MazamaCoreUtils getAPIKey
#'
#' @title Create a new OpenAQ synoptic dataset
#'
#' @description Download, parse and enhance synoptic data from OpenAQ and
#' return the results as a useful tibble with class `openaq_synoptic`.
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
#' @param api_key OpenAQ API READ Key. If `api_key = NULL`, it
#' will be obtained using `getAPIKey("OpenAQ-read")`.
#' @param countryCodes ISO 3166-1 alpha-2 country codes used to subset the data.
#' At least one countryCode must be specified.
#' @param stateCodes ISO-3166-2 alpha-2 state codes used to subset the data.
#' Specifying stateCodes is optional.
#' @param counties US county names or 5-digit FIPS codes used to subset the data.
#' Specifying counties is optional.
#' @param lookbackDays Number of days to "look back" for valid data. Data are
#' filtered to only include sensors with data more recent than `lookbackDays` ago.
#' Use `lookbackDays = 0` to get all historical sensors.
#' @param providers_id Numeric vector containing 1 or more provider ID(s) to use for filtering results.
#' @param manufacturers_id TODO Numeric vector containing 1 or more manufacturer ID(s) to use for filtering results.
#' @param monitor A logical to filter results to regulatory monitors (TRUE) or air sensors (FALSE), both are included if NULL.
#' @param limit An integer specifying the maximum number of results to return, max is 1000.
#'
#' @return An OpenAQ  *synoptic* object.
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
#' synoptic <-
#'   OpenAQ_createOpenSynoptic(
#'     api_key = OPENAQ_API_KEY
#'   )
#'
#' synoptic %>% synoptic_leaflet()
#'
#' }, silent = FALSE)
#' }

OpenAQ_createSynoptic <- function(
  api_key = NULL,
  countryCodes = NULL,
  stateCodes = NULL,
  counties = NULL,
  lookbackDays = 1,
  providers_id = NULL,
  manufacturers_id = NULL,
  monitor = NULL,
  limit = 1000
) {

  # ----- Validate parameters --------------------------------------------------

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

    if ( !exists("OpenAQ_countries") ) {
      OpenAQ_countries <<- openaq::list_countries(limit = 1000)
    }
    countries_id <-
      OpenAQ_countries %>%
      dplyr::filter(code %in% countryCodes) %>%
      dplyr::pull(id)

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

  # ----- manufacturers_id -----------------------------------------------------

  manufacturers_id <- NULL

  # ----- Load data ------------------------------------------------------------

  message("countries_id = ", paste0(countries_id, collapse = ","))
  message("bbox = ", paste0(bbox, collapse = ","))

  message("Done!")

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  OpenAQ_createSynoptic(
    api_key = NULL,
    countryCodes = c("US"),
    stateCodes = "IL",
    counties = "Cook",
    lookbackDays = 1,
    providers_id = NULL,
    manufacturers_id = NULL,
    monitor = NULL,
    limit = 1000
  )


}
