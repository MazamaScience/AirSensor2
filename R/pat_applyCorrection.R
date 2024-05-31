#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.error logger.debug logger.isInitialized
#'
#' @title Apply correction to PurpleAir PM2.5 data.
#'
#' @param pat Previously generated \emph{houly pat} object.
#' @param name Name of correction to apply. One of \code{"EPA_FASM"}
#'
#' @return A PurpleAir Timeseries \emph{pat} object with an additional
#' \code{"pm2.5_corrected"} variable.
#'
#' @description A correction equation is applied to fields of the incoming
#' \emph{pat} object to generate a \code{"pm2.5_corrected"} time series which is
#' added to the returned \emph{pat} object.
#'
#' The default, \code{"EPA_FASM"} correction is described on slide 37 of a
#' presentation on the correction of PurpleAir data for the
#' \href{https://www.epa.gov/sites/default/files/2021-05/documents/toolsresourceswebinar_purpleairsmoke_210519b.pdf}{EPA Fire and Smoke Map}.
#'
#' This correction has two parts:
#'
#'   Low Concentration (\eqn{pm2.5\_cf\_1 <= 343 \mu g/m^3}):
#'
#'   \eqn{pm2.5\_corrected = 0.52 * pm\_2.5\_cf\_1 - 0.086 * humidity + 5.75}
#'
#' High Concentration (\eqn{pm2.5\_cf\_1 > 343 \mu g/m^3}):
#'
#'   \eqn{pm2.5\_corrected = 0.46 * pm2.5\_cf\_1 + 3.93 * 10^{-4} * pm2.5\_cf\_1^2 + 2.97}
#'
# @examples
# \donttest{
# # Fail gracefully if any resources are not available
# try({
#
# library(AirSensor2)
#
#
#
# }, silent = FALSE)
# }

pat_applyCorrection <- function(
    pat = NULL,
    name = c("EPA_FASM")
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(pat)
  name <- match.arg(name)

  hourly_fields <-
    PurpleAir_PAT_EPA_HOURLY_FIELDS %>%
    stringr::str_split_1(",")

  if ( !all(hourly_fields %in% names(pat$data)) ) {
    stop(sprintf(
      "Required fields missing from 'pat' which must include all of \"%s\"",
      PurpleAir_PAT_EPA_HOURLY_FIELDS
    ))
  }

  data <- pat$data

  # ----- Apply correction -----------------------------------------------------

  if ( name == "EPA_FASM" ) {

    data$pm2.5_corrected <- as.numeric(NA)

    mask_low <- data$pm2.5_cf_1 <= 343
    mask_low[is.na(mask_low)] <- TRUE    # NOTE:  Can't have NAs in a mask

    data$pm2.5_corrected[mask_low] <-
      0.52 * data$pm2.5_cf_1[mask_low] - 0.086 * data$humidity[mask_low] + 5.75

    data$pm2.5_corrected[!mask_low] <-
      0.46 * data$pm2.5_cf_1[!mask_low] + 3.93 * .0001 * data$pm2.5_cf_1[!mask_low]^2 + 2.97

  }

  # ----- Return ---------------------------------------------------------------

  pat$data <- data

  return(pat)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  api_key = PurpleAir_API_READ_KEY
  pas = example_pas
  sensor_index = "76545"
  startdate = "2023-01-01"
  enddate = "2023-01-08"
  timezone = "America/Los_Angeles"
  fields = PurpleAir_PAT_EPA_HOURLY_FIELDS
  baseUrl = "https://api.purpleair.com/v1/sensors"
  verbose = TRUE

}
