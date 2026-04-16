#' @export
#' @importFrom rlang .data
#'
#' @title Date filtering for PurpleAir Synoptic objects
#'
#' @param pas PurpleAir Synoptic *pas* object.
#' @param startdate Desired start datetime (ISO 8601).
#' @param enddate Desired end datetime (ISO 8601).
#' @param timezone Optional Olson timezone used to interpret dates.
#'
#' @description Subsets a *pas* object by date.
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
#' }
#'
#' @note
#' The incoming `pas` object must contain the `date_created` and
#' `last_modified` fields. The returned *pas* object will retain sites
#' where `startdate <= last_seen && enddate >=  pas$date_created`.
#'
#' @seealso [pas_filter()]
#'
#' @examples
#' library(AirSensor2)
#'
#' august_2018 <-
#'  example_pas_historical %>%
#'   pas_filterDate(
#'     startdate = 20180701,
#'     enddate = 20181101,
#'     timezone = "America/Los_Angeles"
#'  )
#'
#' fields <- c("sensor_index", "name", "date_created", "last_seen")
#' head(august_2018[,fields])
#'

pas_filterDate <- function(
    pas = NULL,
    startdate = NULL,
    enddate = NULL,
    timezone = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(pas)
  MazamaCoreUtils::stopIfNull(startdate)
  MazamaCoreUtils::stopIfNull(enddate)

  if ( !"date_created" %in% names(pas) ) {
    stop(paste0(
      "Incoming pas has no 'date_created' column.\n",
      "Please create a pas object using PurpleAir_PAS_METADATA_FIELDS"
    ))
  }

  if ( !"last_seen" %in% names(pas) ) {
    stop(paste0(
      "Incoming pas has no 'date_created' column.\n",
      "Please create a pas object using PurpleAir_PAS_METADATA_FIELDS"
    ))
  }

  # ----- Filter pas -----------------------------------------------------------

  timeRange <-
    MazamaCoreUtils::timeRange(
      starttime = startdate,
      endtime = enddate,
      timezone = timezone,
      unit = "day"
    )

  new_pas <-
    pas %>%
    dplyr::filter(
      !!timeRange[1] <= .data$last_seen, !!timeRange[2] >=  .data$date_created
    )

  return(new_pas)

}
