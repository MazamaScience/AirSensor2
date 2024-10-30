#' @export
#' @importFrom rlang .data
#'
#' @title General purpose data filtering for \emph{pat} time series objects
#'
#' @param pat \emph{pat} object.
#' @param ... Logical predicates defined in terms of the variables in
#' \code{pat$data}.
#'
#' @description A generalized data filter for \emph{pat} objects to
#' choose rows/cases where conditions are true.  Multiple conditions are
#' combined with \code{&} or separated by a comma. Only rows where the condition
#' evaluates to TRUE are kept. Rows where the condition evaluates to \code{NA}
#' are dropped.
#'
#' If an empty \emph{pat} object is passed in, it is immediately returned,
#' allowing for multiple filtering steps to be piped together and only checking
#' for an empty \emph{pat} object at the end of the pipeline.
#'
#' @note Filtering is done on values in \code{pat$data}.
#'
#' @return A subset of the incoming \code{pat} time series object.
#' (A list with \code{meta} and \code{data} dataframes.)
#'
#' @seealso \link{pat_filterDate}
#' @seealso \link{pat_filterDatetime}
#' @examples
#'
#' library(AirSensor2)
#'
#' # Unhealthy values
#' example_pat %>%
#'   pat_filter(pm2.5_atm_a > 25, pm2.5_atm_b > 25) %>%
#'   pat_getData() %>%
#'   head()
#'

pat_filter <- function(
    pat,
    ...
) {
  return(
    MazamaTimeSeries::sts_filter(pat, ...)
  )
}
