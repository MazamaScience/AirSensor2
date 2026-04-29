#' Apply AirGradient PM2.5 correction
#'
#' Applies the AirGradient PM2.5 correction based on the EPA correction
#' algorithm for the Plantower PM sensor.
#'
#' The input `data` must include:
#'
#' * `datetime`
#' * `pm25`
#' * `relativehumidity`
#'
#' A new `pm25_corrected` column is added. Negative corrected values are set
#' to zero.
#'
#' @param data Data frame containing AirGradient measurements.
#'
#' @return The input data frame with an added `pm25_corrected` column.
#'
#' @export
AirGradient_applyCorrection <- function(data = NULL) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(data)

  requiredNames <- c(
    "datetime",
    "pm25",
    "relativehumidity"
  )

  if ( !all(requiredNames %in% names(data)) ) {
    stop(sprintf(
      "Required fields missing from 'data' which must include all of: %s",
      paste(requiredNames, collapse = ", ")
    ))
  }

  # ----- Apply correction -----------------------------------------------------

  AGraw <- data$pm25
  RHraw <- data$relativehumidity

  data$pm25_corrected <- as.numeric(NA)

  # AGraw < 30
  mask <- !is.na(AGraw) & AGraw < 30

  data$pm25_corrected[mask] <-
    0.524 * AGraw[mask] -
    0.0862 * RHraw[mask] +
    5.75

  # 30 <= AGraw < 50
  mask <- !is.na(AGraw) & AGraw >= 30 & AGraw < 50
  x <- AGraw[mask] / 20 - 3 / 2

  data$pm25_corrected[mask] <-
    (0.786 * x + 0.524 * (1 - x)) * AGraw[mask] -
    0.0862 * RHraw[mask] +
    5.75

  # 50 <= AGraw < 210
  mask <- !is.na(AGraw) & AGraw >= 50 & AGraw < 210

  data$pm25_corrected[mask] <-
    0.786 * AGraw[mask] -
    0.0862 * RHraw[mask] +
    5.75

  # 210 <= AGraw < 260
  mask <- !is.na(AGraw) & AGraw >= 210 & AGraw < 260
  x <- AGraw[mask] / 50 - 21 / 5

  data$pm25_corrected[mask] <-
    0.69 * x * AGraw[mask] +
    0.786 * (1 - x) * AGraw[mask] -
    0.0862 * RHraw[mask] * (1 - x) +
    2.966 * x +
    5.75 * (1 - x) +
    8.84e-4 * AGraw[mask]^2 * x

  # 260 <= AGraw
  mask <- !is.na(AGraw) & AGraw >= 260

  data$pm25_corrected[mask] <-
    2.966 +
    0.69 * AGraw[mask] +
    8.84e-4 * AGraw[mask]^2

  # AirGradient recommends setting negative corrected values to zero.
  data$pm25_corrected <-
    round(pmax(data$pm25_corrected, 0, na.rm = FALSE), 1)

  return(data)

}
