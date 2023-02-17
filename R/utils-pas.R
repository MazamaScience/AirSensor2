#' @export
#'
#' @title Test for an empty \emph{pas} object
#'
#' @param pas A \emph{pa_synoptic} object.
#'
#' @return \code{TRUE} if no data exist in \code{pas}, \code{FALSE} otherwise.
#'
#' @description Convenience function for \code{nrow(pas) == 0}.
#' This makes for more readable code in functions that need to test for this.
#'
#' @examples
#' library(AirSensor2)
#' example_pas %>% pas_isEmpty() %>% print()
#' example_pas %>% pas_filter(latitude > 90) %>% pas_isEmpty() %>% print()

pas_isEmpty <- function(pas = NULL) {
  return( nrow(pas) == 0 )
}

#' @export
#'
#' @title Test for spatial metadata in \emph{pas} object
#'
#' @param pas A PurpleAir Synoptic \emph{pas} object.
#'
#' @return \code{TRUE} if \code{pas} contains core spatial metadata,
#' \code{FALSE} otherwise.
#'
#' @description Tests for the existence of the following core spatial metadata
#' columns:
#'
#' \itemize{
#'   \item{longitude -- decimal degrees E}
#'   \item{latitude -- decimal degrees N}
#'   \item{timezone -- Olson timezone}
#'   \item{countryCode -- ISO 3166-1 alpha-2}
#'   \item{stateCode -- ISO 3166-2 alpha-2}
#' }
#'
#' @examples
#' library(AirSensor2)
#' example_pas_raw %>% pas_hasSpatial() %>% print()
#' example_pas %>% pas_hasSpatial() %>% print()

pas_hasSpatial <- function(pas = NULL) {

  if ( is.null(pas) ) return(FALSE)

  # Test the following
  parameters <- c(
    "longitude", "latitude", "timezone", "countryCode", "stateCode"
  )

  if ( all(parameters %in% names(pas)) ) {

    return(TRUE)

  } else {

    return(FALSE)

  }

}

