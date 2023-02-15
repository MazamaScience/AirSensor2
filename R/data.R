#' @encoding UTF-8
#' @title Example raw Purple Air Synoptic dataset
#' @format A tibble with 1523 rows and 45 columns of data.
#' @description The \code{example_pas_raw} dataset provides a quickly loadable
#' version of raw Purple Air synoptic data for practicing and code
#' examples. This dataset contains data for sensors in Washington and Oregon
#' and was generated on 2023-02-15 by running:
#'
#' \preformatted{
#' library(AirSensor2)
#'
#' initializeMazamaSpatialUtils()
#'
#' example_pas_raw <-
#'   pas_downloadParseRawData(
#'     MY_API_READ_KEY,
#'     fields = pas_PM25_FIELDS,
#'     maxAge = 3600 * 24,
#'     outsideOnly = TRUE,
#'     west = -125,
#'     east = -117,
#'     south = 42,
#'     north = 49
#'   )
#'
#' save(example_pas_raw, file = "data/example_pas_raw.rda")
#' }
#'
#' This dataset can be converted into a standard \emph{pas} object with:
#'
#' \preformatted{
#' pas <- pas_enhanceRawData(example_pas_raw)
#' }
#'
#' @seealso example_pas
#' @source https://www2.purpleair.com
"example_pas_raw"


#' @encoding UTF-8
#' @title Example enhanced Purple Air Synoptic dataset
#' @format A tibble with 1481 rows and 57 columns of data.
#' @description The \code{example_pas} dataset provides a quickly loadable
#' version of a \emph{pas} object for practicing and code examples.
#' This dataset contains data for sensors in Washington and Oregon
#' and was generated on 2023-02-15 by running:
#'
#' \preformatted{
#' library(AirSensor2)
#'
#' initializeMazamaSpatialUtils()
#'
#' example_pas <-
#'   pas_createNew(
#'     apiReadKey = MY_API_READ_KEY,
#'     countryCodes = "US",
#'     stateCodes = c("WA", "OR"),
#'     counties = NULL,
#'     lookbackDays = 1,
#'     outsideOnly = TRUE
#'   )
#'
#' save(example_pas, file = "data/example_pas.rda")
#' }
#'
#' @seealso example_pas_raw
#' @source https://www2.purpleair.com
"example_pas"


