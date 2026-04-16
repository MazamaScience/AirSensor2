#' @export
#'
#' @title Check *pat* object for validity
#'
#' @param pat *pat* object.
#'
#' @description Checks on the validity of an *pat* object. If any test
#' fails, this function will stop with a warning message.
#'
#' @return Returns `TRUE` invisibly if the *pat* object is valid.
#'
#' @seealso [pat_isValid()]
#'
#' @examples
#' library(AirSensor2)
#'
#' pat_check(example_pat)
#'
#' # This would throw an error
#' if ( FALSE ) {
#'
#'   broken_pat <- example_pat
#'   names(broken_pat) <- c('meta', 'bop')
#'   pat_check(broken_pat)
#'
#' }
#'
pat_check <- function(pat) {
  return(MazamaTimeSeries::sts_check(pat))
}


#' @export
#'
#' @name pat_isValid
#' @title Test *pat* object for correct structure
#'
#' @param pat *pat* object
#' @param verbose Logical specifying whether to produce detailed warning messages.
#'
#' @description The `pat` is checked for the presence of core
#' `meta` and `data` columns.
#'
#' Core `meta` columns include:
#'
#' \itemize{
#'   \item{`deviceDeploymentID` -- unique identifier (see \pkg{MazmaLocationUtils})}
#'   \item{`deviceID` -- device identifier}
#'   \item{`locationID` -- location identifier (see \pkg{MazmaLocationUtils})}
#'   \item{`locationName` -- English language name}
#'   \item{`longitude` -- decimal degrees E}
#'   \item{`latitude` -- decimal degrees N}
#'   \item{`elevation` -- elevation of station in m}
#'   \item{`countryCode` -- ISO 3166-1 alpha-2}
#'   \item{`stateCode` -- ISO 3166-2 alpha-2}
#'   \item{`timezone` -- Olson time zone}
#' }
#'
#' Core `data` columns include:
#'
#' \itemize{
#'   \item{`datetime` -- measurement time (UTC)}
#' }
#'
#' @return `TRUE` if `pat` has the correct structure,
#' `FALSE` otherwise.
#'
#' @examples
#' library(AirSensor2)
#'
#' pat_isValid(example_pat)
#'
pat_isValid <- function(
    pat = NULL,
    verbose = FALSE
) {
  return(invisible(MazamaTimeSeries::sts_isValid(pat, verbose)))
}


#' @export
#'
#' @title Test for empty *pat* object
#'
#' @param pat *pat* object
#'
#' @return `TRUE` if no data exist in `pat`, `FALSE` otherwise.
#'
#' @description Convenience function for `nrow(pat$data) == 0`.
#' This makes for more readable code in functions that need to test for this.
#'
#' @examples
#' library(AirSensor2)
#'
#' pat_isEmpty(example_pat)
#'
pat_isEmpty <- function(pat) {
  return(MazamaTimeSeries::sts_isEmpty(pat))
}


#' @importFrom rlang .data
#' @export
#'
#' @title Retain only distinct data records in `pat$data`
#'
#' @param pat *pat* object
#'
#' @return An *pat* object where each record is associated with a unique
#' time.
#' (A list with `meta` and `data` dataframes.)
#'
#' @description Three successive steps are used to guarantee that the
#' `datetime` axis contains no repeated values:
#'
#' \enumerate{
#' \item{remove any duplicate records}
#' \item{guarantee that rows are in `datetime` order}
#' \item{average together fields for any remaining records that share the same
#' `datetime`}
#' }
#'
pat_distinct <- function(pat) {
  return(MazamaTimeSeries::sts_distinct(pat))
}


#' @title Extract dataframes from *pat* objects
#'
#' @description
#' These functions are convenient wrappers for extracting the dataframes that
#' comprise a *pat* object. These functions are designed to be useful when
#' manipulating data in a pipeline using `\%>\%`.
#'
#' Below is a table showing equivalent operations for each function.
#'
#' `pat_getData(pat)` is equivalent to `pat$data`.
#'
#' `pat_getMeta(pat)` is equivalent to `pat$meta`.
#'
#' @param pat *pat* object to extract dataframe from.
#'
#' @return A dataframe from the *pat* object.
#'
#' @name pat_getDataFrame
#' @aliases pat_getData pat_getMeta
#'
NULL


#' @export
#' @rdname pat_getDataFrame
#'
pat_getData <- function(pat) {
  return(MazamaTimeSeries::sts_extractData(pat))
}


#' @export
#' @rdname pat_getDataFrame
#'
pat_getMeta <- function(pat) {
  return(MazamaTimeSeries::sts_extractMeta(pat))
}

