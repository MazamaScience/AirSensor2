#' @export
#' @importFrom rlang .data
#'
#' @title Date filtering for \emph{pat} time series objects
#'
#' @param pat \emph{pat} object.
#' @param startdate Desired start datetime (ISO 8601).
#' @param enddate Desired end datetime (ISO 8601).
#' @param timezone Olson timezone used to interpret dates.
#' @param unit Units used to determine time at end-of-day.
#' @param ceilingStart Logical instruction to apply
#'   \code{\link[lubridate]{ceiling_date}} to the \code{startdate} rather than
#'   \code{\link[lubridate]{floor_date}}
#' @param ceilingEnd Logical instruction to apply
#'   \code{\link[lubridate]{ceiling_date}} to the \code{enddate} rather than
#'   \code{\link[lubridate]{floor_date}}
#'
#' @description Subsets a \emph{pat} object by date. This function
#' always filters to day-boundaries. For sub-day filtering, use
#' \code{pat_filterDatetime()}.
#'
#' Dates can be anything that is understood by \code{MazamaCoreUtils::parseDatetime()}
#' including either of the following recommended formats:
#'
#' \itemize{
#' \item{\code{"YYYYmmdd"}}
#' \item{\code{"YYYY-mm-dd"}}
#' }
#'
#' Timezone determination precedence assumes that if you are passing in
#' \code{POSIXct} values then you know what you are doing.
#'
#' \enumerate{
#' \item{get timezone from \code{startdate} if it is \code{POSIXct}}
#' \item{use passed in \code{timezone}}
#' \item{get timezone from \code{pat}}
#' }
#'
#' @note The returned data will run from the beginning of \code{startdate} until
#' the \strong{beginning} of \code{enddate} -- \emph{i.e.} no values associated
#' with \code{enddate} will be returned. The exception being when
#' \code{enddate} is less than 24 hours after \code{startdate}. In that case, a
#' single day is returned.
#'
#' @return A subset of the incoming \emph{pat} time series object.
#' (A list with \code{meta} and \code{data} dataframes.)
#'
#' @seealso \link{pat_filter}
#' @seealso \link{pat_filterDatetime}
# @examples
# library(AirSensor2)
#
# example_pat %>%
#   pat_filterDate(startdate = 20180808, enddate = 20180815) %>%
#   pat_getData() %>%
#   head()
#

pat_filterDate <- function(
    pat = NULL,
    startdate = NULL,
    enddate = NULL,
    timezone = NULL,
    unit = "sec",
    ceilingStart = FALSE,
    ceilingEnd = FALSE
) {
  return(
    MazamaTimeSeries::sts_filterDate(
      sts = pat,
      startdate = startdate,
      enddate = enddate,
      timezone = timezone,
      unit = unit,
      ceilingStart = ceilingStart,
      ceilingEnd = ceilingEnd
    )
  )
}
