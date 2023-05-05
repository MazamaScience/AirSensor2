#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.isInitialized logger.debug
#' @importFrom MazamaCoreUtils getAPIKey
#'
#' @title Create a new Clarity synoptic dataset
#'
#' @description Download, parse and enhance synoptic data from Clarity and
#' return the results as a useful tibble with class \code{clarity_synoptic}.
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
#' 4) Convert data types from character to \code{POSIXct} and \code{numeric}.
#'
#' @param api_key Clarity API READ Key. If \code{api_key = NULL}, it
#' will be obtained using \code{getAPIKey("Clarity-read")}.
#' @param format Customized output format (currently only "USFS").
#' @param baseUrl Base URL for the PurpleAir API.
#'
#' @return A Clarity Synoptic \emph{pas} object.
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
#'   Clarity_createOpenSynoptic(
#'     api_key = Clarity_API_READ_KEY,
#'     format = "USFS"
#'   )
#'
#' pas %>% pas_leaflet()
#'
#' }, silent = FALSE)
#' }

Clarity_createOpenSynoptic <- function(
  api_key = NULL,
  format = c("USFS"),
  baseUrl = "https://clarity-data-api.clarity.io/v1/open/all-recent-measurement/pm25/hourly"
) {

  # ----- Validate parameters --------------------------------------------------

  if ( is.null(api_key) )
    api_key <- MazamaCoreUtils::getAPIKey("Clarity-read")

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(baseUrl)

  format <- match.arg(format)

  # Check if MazamaSpatialUtils package has been initialized
  # via initializeMazamaSpatialUtils()
  if ( !spatialIsInitialized() ) {
    stop('`Clarity_createSynoptic` requires MazamaSpatialUtils to be initialized:

            initializeMazamaSpatialUtils()

         Please see `?initializeMazamaSpatialUtils for more details.')
  }


  # ----- Load data ------------------------------------------------------------

  # Download, parse and enhance synoptic data
  if ( logger.isInitialized() )
    logger.debug("----- Clarity_getAllOpenHourly() -----")

  DFList <-
    Clarity_getAllOpenHourly(
      api_key = api_key,
      format = format,
      baseUrl = baseUrl
    )

  # > names(DFList)
  # [1] "synoptic" "QC"       "pm2.5"     "nowcast"

  if ( logger.isInitialized() )
    logger.debug("----- Clarity_enhance() -----")

  syn <-
    Clarity_enhanceRawSynopticData(
      DFList$synoptic
    )

  if ( logger.isInitialized() )
    logger.debug("----- finished enhancing -----")

  # ----- Return ---------------------------------------------------------------

  # Add a class name
  class(synoptic) <- union("synoptic", class(synoptic))

  return(synoptic)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {


}
