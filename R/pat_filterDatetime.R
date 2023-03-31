#' @export
#' @importFrom rlang .data
#'
#' @title Datetime filtering for \emph{pat} time series objects
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
#' @description Subsets a \emph{pat} object by datetime. This function
#' allows for sub-day filtering as opposed to \code{pat_filterDate()} which
#' always filters to day-boundaries.
#'
#' Datetimes can be anything that is understood by
#' \code{MazamaCoreUtils::parseDatetime()}. For non-\code{POSIXct} values,
#' the recommended format is \code{"YYYY-mm-dd HH:MM:SS"}.
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
#' @return A subset of the incoming \emph{pat} time series object.
#' (A list with \code{meta} and \code{data} dataframes.)
#'
#' @seealso \link{pat_filter}
#' @seealso \link{pat_filterDate}
#'
# @examples
# library(AirSensor2)
#
# example_pat %>%
#   pat_filterDatetime(
#     startdate = "2018-08-08 06:00:00",
#     enddate = "2018-08-14 18:00:00"
#   ) %>%
#   pat_extractData() %>%
#   head()
#

pat_filterDatetime <- function(
    pat = NULL,
    startdate = NULL,
    enddate = NULL,
    timezone = NULL,
    unit = "sec",
    ceilingStart = FALSE,
    ceilingEnd = FALSE
) {
  return(
    MazamaTimeSeries::sts_filterDatetime(
      pat = pat,
      startdate = startdate,
      enddate = enddate,
      timezone = timezone,
      unit = unit,
      ceilingStart = ceilingStart,
      ceilingEnd = ceilingEnd
    )
  )
}
