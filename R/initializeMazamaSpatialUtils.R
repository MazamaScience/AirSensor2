#' @export
#' @importFrom MazamaCoreUtils logger.setup logger.setLevel WARN
#'
#' @title Initialize MazamaSpatialUtils package
#'
#' @param spatialDataDir Directory where \pkg{MazamaSpatialUtils} datasets are found.
#' @param stateCodeDataset \pkg{MazamaSpatialUtils} dataset returning ISO 3166-2 .
#' alpha-2 stateCodes.
#' @param USCountiesDataset \pkg{MazamaSpatialUtils} dataset returning US county names.
#' @param logLevel Logging level used if logging has not already been
#' initialized.
#'
#' @description Convenience function that wraps:
#'
#' \preformatted{
#'   data("SimpleCountriesEEZ", package = "MazamaSpatialUtils")
#'   data("SimpleTimezones", package = "MazamaSpatialUtils")
#'
#'   MazamaSpatialUtils::setSpatialDataDir("~/Data/Spatial")
#'   MazamaSpatialUtils::loadSpatialData("NaturalEarthAdm1.rda")
#'   MazamaSpatialUtils::loadSpatialData("USCensusCounties.rda")
#' }
#'
#' This function should be run before using \code{pas_load()}, as
#' \code{pas_load()} uses the spatial data loaded by
#' \code{initializeMazamaSpatialUtils()} to enhance raw synoptic data via
#' \code{pas_enhanceData()}.
#'
#' If file logging is desired, these commands should be run individually with
#' output log files specified as arguments to \code{logger.setup()} from the
#' \pkg{MazamaCoreUtils} package.
#'

initializeMazamaSpatialUtils <- function(
  spatialDataDir = "~/Data/Spatial",
  stateCodeDataset = "NaturalEarthAdm1.rda",
  USCountiesDataset = "USCensusCounties.rda",
  logLevel = WARN
) {

  # ----- Validate Parameters --------------------------------------------------

  # Set up logging if not already set up
  if ( !MazamaCoreUtils::logger.isInitialized() ) {
    MazamaCoreUtils::logger.setup()
    MazamaCoreUtils::logger.setLevel(logLevel)
  }

  # ----- Load spatial data ----------------------------------------------------

  # Package internal data
  utils::data("SimpleCountriesEEZ", package = "MazamaSpatialUtils")
  utils::data("SimpleTimezones", package = "MazamaSpatialUtils")

  # Package external data
  MazamaSpatialUtils::setSpatialDataDir(spatialDataDir)
  MazamaSpatialUtils::loadSpatialData(stateCodeDataset)
  MazamaSpatialUtils::loadSpatialData(USCountiesDataset)

  # Add env variable for initialization detection
  Sys.setenv(MAZAMA_SPATIAL_IS_INITIALIZED = "TRUE")

  return(invisible(NULL))
}

#' @export
#' @title Check if MazamaSpatialUtils has been initialized
#'
#' @description Logical convenience function to check if
#' \code{initializeMazamaSpatialUtils()} has been run.
#'
#' @return Logical.

spatialIsInitialized <- function() {
  value <- as.logical(Sys.getenv("MAZAMA_SPATIAL_IS_INITIALIZED"))
  if ( is.na(value) ) {
    return(FALSE)
  } else {
    return(value)
  }
}
