#' @export
#' @importFrom rlang .data
#'
#' @title Date filtering for PurpleAir Synoptic objects
#'
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param startdate Desired start datetime (ISO 8601).
#' @param enddate Desired end datetime (ISO 8601).
#' @param timezone Optional Olson timezone used to interpret dates.
#'
#' @description Subsets a \emph{pas} object by date.
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
#' }
#'
#' @note
#' The incoming \code{pas} object must contain the \code{date_created} and
#' \code{last_modified} fields. The returned \emph{pas} object will retain sites
#' where \code{startdate <= last_seen && enddate >=  pas$date_created}.
#'
#' @seealso \link{pas_filter}
#'
#' @examples
#' library(AirSensor2)
#'
#' august_2018 <-
#'  example_pas %>%
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
      "Please create a pas object using PurpleAir_SENSOR_METADATA_FIELDS"
    ))
  }

  if ( !"last_seen" %in% names(pas) ) {
    stop(paste0(
      "Incoming pas has no 'date_created' column.\n",
      "Please create a pas object using PurpleAir_SENSOR_METADATA_FIELDS"
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
