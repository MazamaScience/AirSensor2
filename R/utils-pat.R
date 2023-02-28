#' @export
#'
#' @title Check \emph{pat} object for validity
#'
#' @param pat \emph{pat} object.
#'
#' @description Checks on the validity of an \emph{pat} object. If any test
#' fails, this function will stop with a warning message.
#'
#' @return Returns \code{TRUE} invisibly if the \emph{pat} object is valid.
#'
#' @seealso \link{pat_isValid}
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
#' @title Test \emph{pat} object for correct structure
#'
#' @param pat \emph{pat} object
#' @param verbose Logical specifying whether to produce detailed warning messages.
#'
#' @description The \code{pat} is checked for the presence of core
#' \code{meta} and \code{data} columns.
#'
#' Core \code{meta} columns include:
#'
#' \itemize{
#'   \item{\code{deviceDeploymentID} -- unique identifier (see \pkg{MazmaLocationUtils})}
#'   \item{\code{deviceID} -- device identifier}
#'   \item{\code{locationID} -- location identifier (see \pkg{MazmaLocationUtils})}
#'   \item{\code{locationName} -- English language name}
#'   \item{\code{longitude} -- decimal degrees E}
#'   \item{\code{latitude} -- decimal degrees N}
#'   \item{\code{elevation} -- elevation of station in m}
#'   \item{\code{countryCode} -- ISO 3166-1 alpha-2}
#'   \item{\code{stateCode} -- ISO 3166-2 alpha-2}
#'   \item{\code{timezone} -- Olson time zone}
#' }
#'
#' Core \code{data} columns include:
#'
#' \itemize{
#'   \item{\code{datetime} -- measurement time (UTC)}
#' }
#'
#' @return \code{TRUE} if \code{pat} has the correct structure,
#' \code{FALSE} otherwise.
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
#' @title Test for empty \emph{pat} object
#'
#' @param pat \emph{pat} object
#'
#' @return \code{TRUE} if no data exist in \code{pat}, \code{FALSE} otherwise.
#'
#' @description Convenience function for \code{nrow(pat$data) == 0}.
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
#' @title Retain only distinct data records in \code{pat$data}
#'
#' @param pat \emph{pat} object
#'
#' @return An \emph{pat} object where each record is associated with a unique
#' time.
#' (A list with \code{meta} and \code{data} dataframes.)
#'
#' @description Three successive steps are used to guarantee that the
#' \code{datetime} axis contains no repeated values:
#'
#' \enumerate{
#' \item{remove any duplicate records}
#' \item{guarantee that rows are in \code{datetime} order}
#' \item{average together fields for any remaining records that share the same
#' \code{datetime}}
#' }
#'
pat_distinct <- function(pat) {
  return(MazamaTimeSeries::sts_distinct(pat))
}


#' @title Extract dataframes from \emph{pat} objects
#'
#' @description
#' These functions are convenient wrappers for extracting the dataframes that
#' comprise a \emph{pat} object. These functions are designed to be useful when
#' manipulating data in a pipeline using \code{\%>\%}.
#'
#' Below is a table showing equivalent operations for each function.
#'
#' \code{pat_getData(pat)} is equivalent to \code{pat$data}.
#'
#' \code{pat_getMeta(pat)} is equivalent to \code{pat$meta}.
#'
#' @param pat \emph{pat} object to extract dataframe from.
#'
#' @return A dataframe from the \emph{pat} object.
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

