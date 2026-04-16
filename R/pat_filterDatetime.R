#' @export
#' @importFrom rlang .data
#'
#' @title Datetime filtering for *pat* time series objects
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
#' @description Subsets a *pat* object by datetime. This function
#' allows for sub-day filtering as opposed to `pat_filterDate()` which
#' always filters to day-boundaries.
#'
#' Datetimes can be anything that is understood by
#' `MazamaCoreUtils::parseDatetime()`. For non-`POSIXct` values,
#' the recommended format is `"YYYY-mm-dd HH:MM:SS"`.
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
#' @return A subset of the incoming *pat* time series object.
#' (A list with `meta` and `data` dataframes.)
#'
#' @seealso [pat_filter()]
#' @seealso [pat_filterDate()]
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
