#' Apply correction to PurpleAir PM2.5 data
#'
#' A correction equation is applied to fields of the incoming *pat* object to
#' generate a `pm2.5_corrected` time series which is added to the returned
#' *pat* object.
#'
#' @param pat Previously generated *hourly pat* object.
#'
#' @return A PurpleAir Timeseries *pat* object with an additional
#' `pm2.5_corrected` variable.
#'
#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.error logger.debug logger.isInitialized

pat_applyCorrection <- function(
    pat = NULL
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

  PAcfatm <- data$pm2.5_atm
  RH <- data$humidity

  data$pm2.5_corrected <- as.numeric(NA)

  # PAcfatm < 30
  mask <- !is.na(PAcfatm) & PAcfatm < 30

  data$pm2.5_corrected[mask] <-
    0.524 * PAcfatm[mask] -
    0.0862 * RH[mask] +
    5.75

  # 30 <= PAcfatm < 50
  mask <- !is.na(PAcfatm) & PAcfatm >= 30 & PAcfatm < 50
  x <- PAcfatm[mask] / 20 - 3 / 2

  data$pm2.5_corrected[mask] <-
    (0.786 * x + 0.524 * (1 - x)) * PAcfatm[mask] -
    0.0862 * RH[mask] +
    5.75

  # 50 <= PAcfatm < 210
  mask <- !is.na(PAcfatm) & PAcfatm >= 50 & PAcfatm < 210

  data$pm2.5_corrected[mask] <-
    0.786 * PAcfatm[mask] -
    0.0862 * RH[mask] +
    5.75

  # 210 <= PAcfatm < 260
  mask <- !is.na(PAcfatm) & PAcfatm >= 210 & PAcfatm < 260
  x <- PAcfatm[mask] / 50 - 21 / 5

  data$pm2.5_corrected[mask] <-
    (0.69 * x + 0.786 * (1 - x)) * PAcfatm[mask] -
    0.0862 * RH[mask] * (1 - x) +
    2.966 * x +
    5.75 * (1 - x) +
    8.84e-4 * PAcfatm[mask]^2 * x

  # 260 <= PAcfatm
  mask <- !is.na(PAcfatm) & PAcfatm >= 260

  data$pm2.5_corrected[mask] <-
    2.966 +
    0.69 * PAcfatm[mask] +
    8.84e-4 * PAcfatm[mask]^2

  # EPA guidance recommends setting negative corrected values to zero.
  data$pm2.5_corrected <-
    pmax(data$pm2.5_corrected, 0, na.rm = FALSE)

  # ----- Return ---------------------------------------------------------------

  pat$data <- data

  return(pat)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  api_key = PurpleAir_API_READ_KEY
  pas = example_pas_pm25
  sensor_index = "76545"
  startdate = "2023-01-01"
  enddate = "2023-01-08"
  timezone = "America/Los_Angeles"
  fields = PurpleAir_PAT_EPA_HOURLY_FIELDS
  baseUrl = "https://api.purpleair.com/v1/sensors"
  verbose = TRUE

}
