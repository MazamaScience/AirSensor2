#' @export
#' @importFrom rlang .data
#'
#' @title Rectangle area filtering for PurpleAir Synoptic objects
#'
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param w West edge of area bounding box (deg E).
#' @param e East edge of area bounding box (deg E).
#' @param s South edge of area bounding box (deg N).
#' @param n North edge of area bounding box (deg N).
#'
#' @description Filters \emph{pas} object sensors based on a bounding box.
#'
#' @return A subset of the given \emph{pas} object containing only records
#' within the bounding box. This includes sensors located precisely on a boundary.
#'
#' @seealso \link{pas_filter}
#' @seealso \link{pas_filterNear}
#' @seealso \link{pas_filterNearMonitor}
#'
#' @examples
#' library(AirSensor2)
#'
#' pas <- example_pas
#' range(pas$longitude)
#' range(pas$latitude)
#'
#' Lane_County_pas <-
#'   pas %>%
#'   pas_filterArea(
#'     w = -124.16,
#'     e = -121.76,
#'     s = 43.43,
#'     n = 44.30
#'   )
#'
#' range(Lane_County_pas$longitude)
#' range(Lane_County_pas$latitude)
#'
#' if ( interactive() ) {
#'   pas_leaflet(Lane_County_pas)
#' }
#'

pas_filterArea <- function(
  pas = NULL,
  w = NULL,
  e = NULL,
  s = NULL,
  n = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(pas)

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

  if ( is.null(w) ) w <- min(pas$longitude, na.rm = TRUE)
  if ( is.null(e) ) e <- max(pas$longitude, na.rm = TRUE)
  if ( is.null(s) ) s <- min(pas$latitude, na.rm = TRUE)
  if ( is.null(n) ) n <- max(pas$latitude, na.rm = TRUE)

  # ----- Filter the tibble ----------------------------------------------------

  pas <-
    pas %>%
    dplyr::filter(.data$longitude >= w & .data$longitude <= e) %>%
    dplyr::filter(.data$latitude >= s & .data$latitude <= n)

  # ----- Return ---------------------------------------------------------------

  return(pas)

}
