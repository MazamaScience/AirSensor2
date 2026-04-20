#' Initialize MazamaSpatialUtils
#'
#' @description
#' Convenience function that loads the spatial datasets needed by
#' any of the "enhanceRaw" functions.
#'
#' This function wraps the equivalent sequence:
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
#' It should be run before using `pas_load()`, because `pas_load()` uses the
#' spatial data loaded here to enhance raw synoptic data via
#' `pas_enhanceData()`.
#'
#' If file logging is desired, the individual logging commands should be run
#' separately with output files specified in `MazamaCoreUtils::logger.setup()`.
#'
#' @param spatialDataDir Directory containing external
#'   \pkg{MazamaSpatialUtils} spatial datasets.
#' @param stateCodeDataset Name of the \pkg{MazamaSpatialUtils} dataset
#'   containing state or administrative-level boundaries and codes.
#' @param USCountiesDataset Name of the \pkg{MazamaSpatialUtils} dataset
#'   containing US county names.
#' @param logLevel Logging level to use if logging has not already been
#'   initialized.
#'
#' @return Invisible `NULL`.
#'
#' @export
#' @importFrom MazamaCoreUtils logger.setup logger.setLevel WARN
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

#' Check whether MazamaSpatialUtils has been initialized
#'
#' @description
#' Convenience function that checks whether
#' `initializeMazamaSpatialUtils()` has been run in the current R session.
#'
#' @return Logical `TRUE` if initialization has been performed in the current
#'   session, `FALSE` otherwise.
#'
#' @export
spatialIsInitialized <- function() {
  identical(Sys.getenv("MAZAMA_SPATIAL_IS_INITIALIZED"), "TRUE")
}
