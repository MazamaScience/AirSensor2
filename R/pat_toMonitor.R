#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.isInitialized logger.debug
#' @importFrom MazamaCoreUtils getAPIKey
#'
#' @title Convert a PurpleAir Timeseries to a 'mts_monitor' object
#'
#' @param pat Previously generated \emph{pat} object.
#' @param applyCorrection Logical specifying whether to apply the EPA
#' correction algorithm. (See \link{pat_applyCorrection}.)
#'
#' @description Enhance an hourly PurpleAir Timeseries \emph{pat} object created
#' with \code{pat_createHourly()} and create an object of class \code{mts_monitor}
#' for use with the \href{https://mazamascience.github.io/AirMonitor/}{AirMonitor} package.
#'
#' @return An AirMonitor package \emph{mts_monitor} object.
#'
#' @references \href{https://www2.purpleair.com}{PurpleAir}
#' @references \href{https://api.purpleair.com}{PurpleAir API}
#' @references \href{https://www2.purpleair.com/policies/terms-of-service}{PurpleAir Terms of service}
#' @references \href{https://www2.purpleair.com/pages/license}{PurpleAir Data license}
#' @references \href{https://www2.purpleair.com/pages/attribution}{PurpleAir Data Attribution}
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' # AirSensor2 package
#' library(AirSensor2)
#'
#' monitor <-
#'   example_pat %>%
#'   pat_toMonitor(
#'   )
#'
#' AirMonitor::monitor_timeseriesPlot(monitor, shadedNight = TRUE)
#' }, silent = FALSE)
#' }

pat_toMonitor <- function(
    pat = NULL,
    applyCorrection = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(pat)

  hourly_fields <-
    PurpleAir_PAT_EPA_HOURLY_FIELDS %>%
    stringr::str_split_1(",")

  if ( !all(hourly_fields %in% names(pat$data)) ) {
    stop(sprintf(
      "Required fields missing from 'pat' which must include all of \"%s\"",
      PurpleAir_PAT_EPA_HOURLY_FIELDS
    ))
  }

  # ----- Create meta ----------------------------------------------------------

  monitor_metaNames <-
    c(
      "deviceDeploymentID",
      "deviceID",
      "locationID",
      "locationName",
      "longitude",
      "latitude",
      "elevation",
      "countryCode",
      "stateCode",
      "countyName",
      "timezone",
      "houseNumber",
      "street",
      "city",
      "postalCode",
      "sensor_index",
      "privacy",
      "sensorManufacturer"
    )

  meta <-
    pat$meta %>%
    # Add more metadata used by the AirMonitor package
    dplyr::mutate(
      pollutant = "PM2.5",
      units = "UG/M3",
      address = as.character(NA),
      dataIngestSource = "PurpleAir",
      dataIngestUrl = as.character(NA),
      AQSID = as.character(NA),
      fullAQSID = as.character(NA),
      deploymentType = as.character(NA),
      # Others required by monitor_leaflet
      deviceType = .data$model,
      deviceDescription = as.character(NA),
      deviceExtra = as.character(NA),
      dataIngestURL = as.character(NA),
      dataIngestUnitID = .data$sensor_index,
      dataIngestExtra = as.character(NA),
      dataIngestDescription = as.character(NA)
    )

  # TODO:  This step can be removed when AirMonitor gets upgraded to replace
  # TODO:  'zip' with 'postalCode'.
  if ( !"zip" %in% names(meta) ) {
    meta$zip <- meta$postalCode
  }

  # Remove "PurpleAir_timeseries" and "synoptic" classes
  attributes(meta)$class <-
    setdiff(attributes(meta)$class, c("PurpleAir_timeseries", "synoptic"))

  # ----- Guarantee uniform time axis ------------------------------------------

  pat_corrected <-
    pat %>%
    pat_distinct() %>%
    pat_applyCorrection()

  # ----- Correct data ---------------------------------------------------------

  if ( applyCorrection ) {
    pat_corrected <- pat_applyCorrection(pat)
    columns <- c("datetime", "pm2.5_corrected")
    data <- pat_corrected$data %>% dplyr::select(dplyr::all_of(columns))
  } else {
    columns <- c("datetime", "pm2.5_cf_1")
    data <- pat_corrected$data %>% dplyr::select(dplyr::all_of(columns))
  }

  names(data) <- c("datetime", meta$deviceDeploymentID)

  # ----- Return ---------------------------------------------------------------

  monitor <- list(meta = meta, data = data)

  monitor <- structure(monitor, class = c("mts_monitor", "mts", class(monitor)))

  return(monitor)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {


}
