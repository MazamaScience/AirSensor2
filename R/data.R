#' Example raw PurpleAir synoptic dataset
#'
#' A tibble with 39 rows and 18 columns of data.
#'
#' The `example_pas_pm25_raw` dataset provides a quickly loadable
#' version of raw PurpleAir synoptic data for practice and code examples.
#' This dataset contains data for sensors in a box around the Methow Valley
#' in Washington and was generated on 2024-03-12 by running:
#'
#' \preformatted{
#' library(AirSensor2)
#'
#' initializeMazamaSpatialUtils()
#'
#' source("global_vars.R") # contains PurpleAir_API_READ_KEY
#'
#' example_pas_pm25_raw <-
#'   pas_downloadParseRawData(
#'     PurpleAir_API_READ_KEY,
#'     fields = PurpleAir_PAS_AVG_PM25_FIELDS,
#'     location_type = 0,
#'     modified_since = NULL,
#'     max_age = 3600 * 24,
#'     west = -120.5,
#'     east = -120,
#'     south = 48.2,
#'     north = 48.7
#'   )
#'
#' save(example_pas_pm25_raw, file = "data/example_pas_pm25_raw.rda")
#' }
#'
#' This dataset can be converted into a standard *pas* object with:
#'
#' \preformatted{
#' pas <-
#'   pas_enhanceRawData(
#'     example_pas_pm25_raw,
#'     countryCodes = "US",
#'     stateCodes = "WA"
#'   )
#' }
#'
#' @format A tibble with 39 rows and 18 columns of data.
#' @seealso example_pas_pm25
#' @source https://www2.purpleair.com
"example_pas_pm25_raw"


#' Example enhanced PurpleAir synoptic PM2.5 dataset
#'
#' A tibble with 2287 rows and 33 columns of data.
#'
#' The `example_pas_pm25` dataset provides a quickly loadable
#' version of a *pas* object for practice and code examples. This
#' object contains metadata suitable for generating *pat* objects
#' with [pat_create()].
#'
#' This dataset contains data for sensors in Washington and Oregon
#' and was generated on 2024-03-12 by running:
#'
#' \preformatted{
#' library(AirSensor2)
#'
#' initializeMazamaSpatialUtils()
#'
#' source("global_vars.R") # contains PurpleAir_API_READ_KEY
#'
#' example_pas_pm25 <-
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
#' save(example_pas_pm25, file = "data/example_pas_pm25.rda")
#' }
#'
#' @format A tibble with 2287 rows and 33 columns of data.
#' @seealso example_pas_pm25_raw
#' @seealso example_pas_historical
#' @seealso example_pas_metadata
#' @source https://www2.purpleair.com
"example_pas_pm25"


#' Example historical PurpleAir synoptic dataset
#'
#' A tibble with 122 rows and 22 columns of data.
#'
#' The `example_pas_historical` dataset provides a quickly loadable
#' version of a *pas* object for practice and code examples. This
#' object contains fewer metadata fields than other `example_pas_~` objects
#' and is intended for use with [pas_lifespanPlot()].
#'
#' This dataset contains data for sensors in Washington and Oregon
#' and was generated on 2024-03-12 by running:
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
#'     fields = PurpleAir_PAS_MINIMAL_FIELDS,
#'     countryCodes = "US",
#'     stateCodes = "WA",
#'     counties = "Okanogan",
#'     lookbackDays = 0,            # all years
#'     location_type = 0            # outdoor only
#'   )
#'
#' save(example_pas_historical, file = "data/example_pas_historical.rda")
#' }
#'
#' @format A tibble with 122 rows and 22 columns of data.
#' @seealso example_pas_pm25
#' @seealso example_pas_metadata
#' @source https://www2.purpleair.com
"example_pas_historical"


#' Example PurpleAir synoptic dataset with extended metadata
#'
#' A tibble with 122 rows and 31 columns of data.
#'
#' The `example_pas_metadata` dataset provides a quickly loadable
#' version of a *pas* object for practice and code examples. This
#' object contains more metadata than other `example_pas_~` objects.
#'
#' This dataset contains data for sensors in Washington and Oregon
#' and was generated on 2024-03-12 by running:
#'
#' \preformatted{
#' library(AirSensor2)
#'
#' initializeMazamaSpatialUtils()
#'
#' source("global_vars.R") # contains PurpleAir_API_READ_KEY
#'
#' example_pas_metadata <-
#'   pas_createNew(
#'     api_key = PurpleAir_API_READ_KEY,
#'     fields = PurpleAir_PAS_METADATA_FIELDS,
#'     countryCodes = "US",
#'     stateCodes = "WA",
#'     counties = "Okanogan",
#'     lookbackDays = 0,            # all years
#'     location_type = 0            # outdoor only
#'   )
#'
#' save(example_pas_metadata, file = "data/example_pas_metadata.rda")
#' }
#'
#' @format A tibble with 122 rows and 31 columns of data.
#' @seealso example_pas_pm25
#' @seealso example_pas_historical
#' @source https://www2.purpleair.com
"example_pas_metadata"


#' Example PurpleAir timeseries dataset
#'
#' A list containing two tibbles named `meta` and `data`.
#'
#' The `example_pat` dataset provides a quickly loadable
#' version of a *pat* object for practice and code examples.
#' This dataset contains 24 hours of data for a single PurpleAir sensor
#' and was generated on 2024-03-12 by running:
#'
#' \preformatted{
#' library(AirSensor2)
#'
#' source("global_vars.R") # contains PurpleAir_API_READ_KEY
#'
#' example_pat <-
#'   pat_create(
#'     api_key = PurpleAir_API_READ_KEY,
#'     pas = example_pas_metadata,
#'     sensor_index = "95189",
#'     startdate = "2024-03-01",
#'     enddate = "2024-03-03",
#'     timezone = "America/Los_Angeles",
#'     average = 0,
#'     fields = PurpleAir_PAT_QC_FIELDS
#'   )
#'
#' save(example_pat, file = "data/example_pat.rda")
#' }
#'
#' @format A list containing two tibbles named 'meta' and 'data'.
#' @seealso example_pas_metadata
#' @source https://www2.purpleair.com
"example_pat"


#' Example PurpleAir timeseries dataset with EPA fields
#'
#' A list containing two tibbles named `meta` and `data`.
#'
#' The `example_pat_epa` dataset provides a quickly loadable
#' version of a *pat* object for practice and code examples. This
#' *pat* object contains the `pm2.5_cf` fields required for the EPA
#' correction algorithm used in [pat_applyCorrection()].
#'
#' This dataset contains 24 hours of data for a single PurpleAir sensor
#' and was generated on 2024-10-30 by running:
#'
#' \preformatted{
#' library(AirSensor2)
#'
#' source("global_vars.R") # contains PurpleAir_API_READ_KEY
#'
#' example_pat_epa <-
#'   pat_create(
#'     api_key = PurpleAir_API_READ_KEY,
#'     pas = example_pas_metadata,
#'     sensor_index = "95189",
#'     startdate = "2024-03-01",
#'     enddate = "2024-03-03",
#'     timezone = "America/Los_Angeles",
#'     average = 0,
#'     fields = PurpleAir_PAT_EPA_HOURLY_FIELDS
#'   )
#'
#' save(example_pat_epa, file = "data/example_pat_epa.rda")
#' }
#'
#' @format A list containing two tibbles named 'meta' and 'data'.
#' @seealso example_pas_metadata
#' @source https://www2.purpleair.com
"example_pat_epa"
