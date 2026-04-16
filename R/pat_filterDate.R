#' @export
#' @importFrom rlang .data
#'
#' @title Date filtering for *pat* time series objects
#'
#' @param pat *pat* object.
#' @param startdate Desired start datetime (ISO 8601).
#' @param enddate Desired end datetime (ISO 8601).
#' @param timezone Olson timezone used to interpret dates.
#' @param unit Units used to determine time at end-of-day.
#' @param ceilingStart Logical instruction to apply
#'   [lubridate::ceiling_date()] to the `startdate` rather than
#'   [lubridate::floor_date()]
#' @param ceilingEnd Logical instruction to apply
#'   [lubridate::ceiling_date()] to the `enddate` rather than
#'   [lubridate::floor_date()]
#'
#' @description Subsets a *pat* object by date. This function
#' always filters to day-boundaries. For sub-day filtering, use
#' `pat_filterDatetime()`.
#'
#' Dates can be anything that is understood by `MazamaCoreUtils::parseDatetime()`
#' including either of the following recommended formats:
#'
#' \itemize{
#' \item{`"YYYYmmdd"`}
#' \item{`"YYYY-mm-dd"`}
#' }
#'
#' Timezone determination precedence assumes that if you are passing in
#' `POSIXct` values then you know what you are doing.
#'
#' \enumerate{
#' \item{get timezone from `startdate` if it is `POSIXct`}
#' \item{use passed in `timezone`}
#' \item{get timezone from `pat`}
#' }
#'
#' @note The returned data will run from the beginning of `startdate` until
#' the **beginning** of `enddate` -- *i.e.* no values associated
#' with `enddate` will be returned. The exception being when
#' `enddate` is less than 24 hours after `startdate`. In that case, a
#' single day is returned.
#'
#' @return A subset of the incoming *pat* time series object.
#' (A list with `meta` and `data` dataframes.)
#'
#' @seealso [pat_filter()]
#' @seealso [pat_filterDatetime()]
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
