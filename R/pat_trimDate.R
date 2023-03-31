#' @export
#' @importFrom rlang .data
#'
#' @title Trim \emph{pat} time series object to full days
#'
#' @param pat \emph{pat} object.
#' @param timezone Olson timezone used to interpret dates.
#'
#' @description Trims the date range of a \emph{pat} object to local time date
#' boundaries which are \emph{within} the range of data. This has the effect
#' of removing partial-day data records at the start and end of the timeseries
#' and is useful when calculating full-day statistics.
#'
#' Day boundaries are calculated using the specified \code{timezone} or, if
#' \code{NULL}, using \code{pat$meta$timezone}.
#'
#' @return A subset of the incoming \emph{pat} time series object.
#' (A list with \code{meta} and \code{data} dataframes.)
#'
# @examples
# library(AirSensor2)
#
# UTC_week <- pat_filterDate(
#   example_pat,
#   startdate = 20180808,
#   enddate = 20180815,
#   timezone = "UTC"
# )
#
# # UTC day boundaries
# head(UTC_week$data)
#
# # Trim to local time day boundaries
# local_week <- pat_trimDate(UTC_week)
# head(local_week$data)
#

pat_trimDate <- function(
    pat = NULL,
    timezone = NULL
) {
  return(
    MazamaTimeSeries::sts_trimDate(pat, timezone)
  )
}
