#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.isInitialized logger.debug
#'
#' @title Create a new PurpleAir synoptic dataset
#'
#' @description Download, parse and enhance synoptic data from PurpleAir and
#' return the results as a useful tibble with class \code{pa_synoptic}.
#'
#' Steps include:
#'
#' 1) Download and parse synoptic data
#'
#' 2) Replace variable with more consistent, more human readable names.
#'
#' 3) Add spatial metadata for each sensor including:
#' \itemize{
#'   \item{timezone -- olson timezone}
#'   \item{countryCode -- ISO 3166-1 alpha-2}
#'   \item{stateCode -- ISO 3166-2 alpha-2}
#' }
#'
#' 4) Convert data types from character to \code{POSIXct} and \code{numeric}.
#'
#' Data requests are made with a bounding box determined from the required
#' \code{countryCodes} parameter. If a single country is specified and additional
#' \code{stateCodes} are specified, the bounding box will be limited to those
#' states.
#'
#' @param apiReadKey PurpleAir API Read Key.
#' @param countryCodes ISO 3166-1 alpha-2 country codes used to subset the data.
#' At least one countryCode must be specified.
#' @param stateCodes ISO-3166-2 alpha-2 state codes used to subset the data.
#' Specifying stateCodes is optional.
#' @param lookbackDays Number of days to "look back" for valid data. Data are
#' filtered to only include sensors with data more recent than \code{lookbackDays} ago.
#' @param outsideOnly Logical specifying whether to restrict requests to outside sensors only.
#' @param baseUrl Base URL for synoptic data.
#'
#' @return A PurpleAir Synoptic \emph{pas} object.
#'
#' @seealso \link{pas_downloadParseRawData}
#' @seealso \link{pas_enhanceRawData}
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
#' pas <-
#'   pas_createNew(
#'     apiReadKey = API_READ_KEY,
#'     countryCodes = "US",
#'     stateCodes = "WA",
#'     lookbackDays = 1,
#'     outsideOnly = TRUE
#'   )
#'
#' pas %>% pas_leaflet()
#'
#' }, silent = FALSE)
#' }

pas_createNew <- function(
  apiReadKey = NULL,
  countryCodes = NULL,
  stateCodes = NULL,
  lookbackDays = 1,
  outsideOnly = TRUE,
  baseUrl = "https://api.purpleair.com/v1/sensors"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(countryCodes)
  # NOTE:  stateCodes are optional
  lookbackDays <- MazamaCoreUtils::setIfNull(lookbackDays, 1)
  outsideOnly <- MazamaCoreUtils::setIfNull(outsideOnly, TRUE)
  MazamaCoreUtils::stopIfNull(baseUrl)

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

  # Check if MazamaSpatialUtils package has been initialized
  # via initializeMazamaSpatialUtils()
  if ( !spatialIsInitialized() ) {
    stop('`pas_createNew` requires MazamaSpatialUtils to be initialized:

            library(MazamaSpatialUtils)
            initializeMazamaSpatialUtils()

         Please see `?initializeMazamaSpatialUtils for more details.')
  }

  # ----- Get country/state bounding box ---------------------------------------

  # NOTE:  Most PurpleAir sensors are in the the US (in California).

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

  west <- bbox[1,1]
  east <- bbox[1,2]
  south <- bbox[2,1]
  north <- bbox[2,2]

  # ----- Load data ------------------------------------------------------------

  # Download, parse and enhance synoptic data
  if ( logger.isInitialized() )
    logger.debug("----- pas_downloadParseRawData() -----")

  pas_raw <-
    pas_downloadParseRawData(
      apiReadKey = apiReadKey,
      maxAge = lookbackDays * 24 * 3600,
      outsideOnly = outsideOnly,
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
      countryCodes,
      stateCodes
    )

  # # Filter for age
  # starttime <- lubridate::now(tzone = "UTC") - lubridate::ddays(lookbackDays)
  # pas <- dplyr::filter(pas, .data$lastSeenDate >= starttime)

  # ----- Return ---------------------------------------------------------------

  # Add a class name
  class(pas) <- union("pa_synoptic", class(pas))

  return(pas)

}

# ===== DEBUGGING ============================================================

if ( FALSE ) {

  countryCodes = "US"
  stateCodes = "WA"
  lookbackDays = 1
  outsideOnly = TRUE
  baseUrl = "https://api.purpleair.com/v1/sensors"

}