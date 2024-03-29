#' @export
#' @importFrom rlang .data
#'
#' @title Returns a column of data from a PurpleAir synoptic object
#'
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param name Name of the data column to return.
#'
#' @description A simple wrapper around \code{dplyr::pull()} to help with
#' readability of user recipes.
#' @return A single column of data from the incoming \emph{pas} object.
#'
#' @examples
#' library(AirSensor2)
#'
#' # Lane (County) Regional Air Protection Agency
#' LRAPA_sensor_indices <-
#'   example_pas_pm25 %>%
#'   pas_filter(stringr::str_detect(name, "Ambassador")) %>%
#'   pas_pull("sensor_index")
#'
#' print(LRAPA_sensor_indices)

pas_pull <- function(
  pas,
  name = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(name)

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

  # ----- Get column -----------------------------------------------------------

  data <-
    pas %>%
    dplyr::pull(!!name)

  # ----- Return ---------------------------------------------------------------

  return(data)

}
