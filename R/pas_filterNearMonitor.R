#' @export
#' @importFrom rlang .data
#'
#' @title Find PurpleAir sensors near a monitor
#'
#' @param pas PurpleAir \emph{pas} object.
#' @param monitor \emph{mts_monitor} object from the \pkg{AirMonitor} package.
#' @param radius Distance from target with unit (i.e "1000 m").
#'
#' @description Filter for PurpleAir sensors within a specified distance from
#' a monitoring site.
#'
#' @details The \code{radius} parameter should be a numeric string with a metric
#' unit separated by a space, such as \code{"250 m"} or \code{"10 km"}. The
#' resulting \emph{pas} will be contain only those sensors that are within
#' `radius` of one of the device-deployments in \code{monitor}.
#'
#' Three additional columns of metadata will be added to the returned \emph{pas}
#' object:
#'
#' \enumerate{
#'   \item{\code{monitor_DDID} -- nearest monitor deviceDeploymentID}
#'   \item{\code{monitor_AQSID} -- nearest monitor AQSID (if any)}
#'   \item{\code{monitor_distance} -- distance in meters to the nearest monitor}
#' }
#'
#' @return A subset of the incoming \emph{pas} object with additional fields.
#'
#' @seealso \link{pas_filter}
#' @seealso \link{pas_filterArea}
#' @seealso \link{pas_filterNear}
#'
#' @examples
#' library(AirSensor2)
#'
#' monitor <-
#'   AirMonitor::NW_Megafires %>%
#'   AirMonitor::monitor_filter(countyName == "Okanogan")
#' pas <-
#'   example_pas_pm25 %>%
#'   pas_filter(countyName == "Okanogan")
#'
#' # Near Omak, WA
#' pas_near_monitors <-
#'   pas %>%
#'   pas_filterNearMonitor(
#'     monitor,
#'     radius = "1000 m"
#'   )
#'
#' pas_near_monitors %>%
#'   dplyr::select(sensor_index, locationName, monitor_AQSID, monitor_distance)
#'

pas_filterNearMonitor <- function(
  pas = NULL,
  monitor = NULL,
  radius = "1 km"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(pas)
  MazamaCoreUtils::stopIfNull(monitor)
  MazamaCoreUtils::stopIfNull(radius)

  # A little involved to catch the case where the user forgets to pass in 'pas'

  result <- try({
    if ( !"PurpleAir_synoptic" %in% class(pas) )
      stop("First argument is not of class 'PurpleAir_synoptic'.")
  }, silent = TRUE)

  if ( class(result) %in% "try-error" ) {
    err_msg <- geterrmessage()
    if ( stringr::str_detect(err_msg, "object .* not found") ) {
      stop(paste0(err_msg, "\n(Did you forget to pass in the 'pas' object?)"))
    } else {
      stop(err_msg)
    }
  }

  if ( !stringr::str_ends(radius, "[ km]") )
    stop("Radius requires a value and a unit (i.e '1 m' or '1 km')")

  parts <-
    stringr::str_split(
      string = radius,
      pattern = " ",
      simplify = TRUE
    )

  if ( tolower(parts[,2]) == "km" ) {
    radius_m <- as.numeric(parts[,1]) * 1000
  } else if ( tolower(parts[,2]) == "m" ) {
    radius_m <- as.numeric(parts[,1])
  } else {
    stop(sprintf("Unit \"%s\" is not understood. Use 'm' or 'km'.", parts[,2]))
  }

  # ----- Calculate distances --------------------------------------------------

  pas$monitor_DDID <- NA
  pas$monitor_AQSID <- NA
  pas$monitor_distance <- NA

  for ( i in seq_len(nrow(pas)) ) {

    mon <-
      monitor %>%
      AirMonitor::monitor_filterByDistance(
        longitude = pas$longitude[i],
        latitude = pas$latitude[i],
        radius = radius_m,
        count = 1,
        addToMeta = TRUE
      )

    if ( nrow(mon$meta) == 1 ) {
      pas$monitor_DDID[i] <- mon$meta$deviceDeploymentID
      pas$monitor_AQSID[i] <- mon$meta$AQSID
      pas$monitor_distance[i] <- mon$meta$distanceFromTarget
    }

  }

  # ----- Subset pas -----------------------------------------------------------

  return_pas <-
    pas %>%
    pas_filter(!is.na(.data$monitor_DDID))

  # ----- Return ---------------------------------------------------------------

  return(return_pas)

}


# ===== DEBUG ==================================================================

if ( FALSE ) {

  library(AirMonitor)

  pas <- example_pas %>% pas_filter(countyName == "Okanogan")
  monitor <- NW_Megafires %>% monitor_filter(countyName == "Okanogan")
  radius <- "1 km"


}
