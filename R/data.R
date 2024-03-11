#' @encoding UTF-8
#' @title Example raw Purple Air Synoptic dataset
#' @format A tibble with 2416 rows and 37 columns of data.
#' @description The \code{example_pas_raw} dataset provides a quickly loadable
#' version of raw Purple Air synoptic data for practicing and code examples
#' This dataset contains data for sensors in Washington and Oregon and was
#' generated on 2023-11-29 by running:
#'
#' \preformatted{
#' library(AirSensor2)
#'
#' initializeMazamaSpatialUtils()
#'
#' source("global_vars.R") # contains PurpleAir_API_READ_KEY
#'
#' example_pas_raw <-
#'   pas_downloadParseRawData(
#'     PurpleAir_API_READ_KEY,
#'     fields = PurpleAir_PAS_AVG_PM25_FIELDS,
#'     location_type = 0,
#'     modified_since = NULL,
#'     max_age = 3600 * 24,
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
#' @format A tibble with 2358 rows and 51 columns of data.
#' @description The \code{example_pas} dataset provides a quickly loadable
#' version of a \emph{pas} object for practicing and code examples.
#' This dataset contains data for sensors in Washington and Oregon
#' and was generated on 2023-11-29 by running:
#'
#' \preformatted{
#' library(AirSensor2)
#'
#' initializeMazamaSpatialUtils()
#'
#' source("global_vars.R") # contains PurpleAir_API_READ_KEY
#'
#' example_pas <-
#'   pas_createNew(
#'     api_key = PurpleAir_API_READ_KEY,
#'     fields = PurpleAir_PAS_AVG_PM25_FIELDS,
#'     countryCodes = "US",
#'     stateCodes = c("WA", "OR"),
#'     counties = NULL,
#'     lookbackDays = 1,
#'     location_type = 0
#'   )
#'
#' save(example_pas, file = "data/example_pas.rda")
#' }
#'
#' @seealso example_pas_raw
#' @source https://www2.purpleair.com
"example_pas"


#' @encoding UTF-8
#' @title Example enhanced Purple Air Synoptic dataset
#' @format A tibble with 2358 rows and 51 columns of data.
#' @description The \code{example_pas} dataset provides a quickly loadable
#' version of a \emph{pas} object for practicing and code examples.
#' This dataset contains data for sensors in Washington and Oregon
#' and was generated on 2023-11-29 by running:
#'
#' \preformatted{
#' library(AirSensor2)
#'
#' initializeMazamaSpatialUtils()
#'
#' source("global_vars.R") # contains PurpleAir_API_READ_KEY
#'
#' example_pas_historical <-
#'   pas_createNew(
#'     api_key = PurpleAir_API_READ_KEY,
#'     fields = PurpleAir_PAS_METADATA_FIELDS,
#'     countryCodes = "US",
#'     stateCodes = "WA",
#'     counties = "Okanogan",
#'     lookbackDays = 365 * 10,     # 10 years
#'     location_type = 0            # Outdoor only
#'   )
#'
#' save(example_pas_historical, file = "data/example_pas_historical.rda")
#' }
#'
#' @seealso example_pas_raw
#' @source https://www2.purpleair.com
"example_pas_historical"


#' @encoding UTF-8
#' @title Example Purple Air Timeseries dataset
#' @format A list containing two tibbles named 'meta' and 'data'.
#' @description The \code{example_pat} dataset provides a quickly loadable
#' version of a \emph{pat} object for practicing and code examples.
#' This dataset contains 24 hours worth of data for a single PurpleAir sensor
#' and was generated on 2023-08-12 by running:
#'
#' \preformatted{
#' library(AirSensor2)
#'
#' source("global_vars.R") # contains PurpleAir_API_READ_KEY
#'
#' example_pat <-
#'   pat_createNew(
#'     api_key = PurpleAir_API_READ_KEY,
#'     pas = example_pas,
#'     sensor_index = "76545",
#'     startdate = "2023-01-01",
#'     enddate = "2023-01-02",
#'     timezone = "America/Los_Angeles"
#'   )
#'
#' save(example_pat, file = "data/example_pat.rda")
#' #' }
#'
#' @seealso example_pas
#' @source https://www2.purpleair.com
"example_pat"


