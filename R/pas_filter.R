#' @export
#' @importFrom rlang .data
#'
#' @title General purpose filtering for PurpleAir Synoptic objects
#'
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param ... Logical predicates defined in terms of the variables in the
#'   \code{pas}. Multiple conditions are combined with & or seperated by a comma.
#'   Only rows where the condition evaluates to TRUE are kept.
#'
#' @description A generalized data filter for \emph{pas} objects to
#'   choose rows/cases where conditions are true. Rows where the condition
#'   evaluates to NA are dropped.
#'
#'   This function is essentially \code{dplyr::\link[dplyr:filter]{filter}}
#'   with an extra check on the validity of the \emph{pas} object.
#'
#' @return A subset of the incoming \emph{pas} object.
#'
#' @seealso \link{pas_filterArea}, \link{pas_filterNear}
#'
#' @examples
#' library(AirSensor2)
#'
#' nrow(example_pas)
#'
#' # Washington
#' WA_pas <-
#'   example_pas %>%
#'   pas_filter(stateCode == "WA")
#'
#' nrow(WA_pas)
#'
#' # Okanogan and Ferry Counties
#' Colville_Tribes_pas <-
#'   example_pas %>%
#'   pas_filter(stateCode == "WA") %>%
#'   pas_filter(countyName %in% c("Okanogan", "Ferry"))
#'
#' nrow(Colville_Tribes_pas)

pas_filter <- function(
  pas,
  ...
) {

  # ----- Validate parameters --------------------------------------------------

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

  # ----- Apply filter ---------------------------------------------------------

  pas <-
    pas %>%
    dplyr::filter(...)

  # ----- Return ---------------------------------------------------------------

  return(pas)

}
