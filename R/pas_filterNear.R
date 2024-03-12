#' @export
#'
#' @title Find PurpleAir sensors within radial distance
#'
#' @param pas PurpleAir \emph{pas} object.
#' @param longitude a Target longitude.
#' @param latitude a Target latitude.
#' @param radius Distance from target with unit (i.e "15 km").
#'
#' @description Filter for PurpleAir sensors within a specified distance from
#' a target location.
#'
#' @details The \code{radius} parameter should be a numeric string with a metric unit
#' separated by a space, such as \code{"250 m"} or \code{"10 km"}.
#'
#' @return A subset of the incoming \emph{pas} object.
#'
#' @seealso \link{pas_filter}
#' @seealso \link{pas_filterArea}
#' @seealso \link{pas_filterNearMonitor}
#'
#' @examples
#' library(AirSensor2)
#'
#' # Near Omak, WA
#' Omak_pas <-
#'   example_pas_pm25 %>%
#'   pas_filterNear(
#'     longitude = -119.5375,
#'     latitude = 48.4125,
#'     radius = "20 km"
#'   )
#'
#' if ( interactive() ) {
#'   pas_leaflet(Omak_pas)
#' }
#'

pas_filterNear <- function(
  pas = NULL,
  longitude = NULL,
  latitude = NULL,
  radius = "1 km"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(pas)
  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)
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

  distance <-
    geodist::geodist(
      x = cbind(
        "x" = longitude,
        "y" = latitude
      ),
      y = cbind(
        "x" = pas$longitude,
        "y" = pas$latitude
      ),
      measure = "geodesic"
    )

  # ----- Return ---------------------------------------------------------------

  return(pas[which(distance <= radius_m),])

}
