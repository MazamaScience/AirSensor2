#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.isInitialized logger.debug
#' @importFrom MazamaCoreUtils getAPIKey
#'
#' @title Convert a PurpleAir Timeseries to a 'mts_monitor' object
#'
#' @param pat Previously generated *pat* object.
#' @param applyCorrection Logical specifying whether to apply the EPA
#' correction algorithm. (See [pat_applyCorrection()].)
#'
#' @description Enhance an hourly PurpleAir Timeseries *pat* object created
#' with `pat_createHourly()` and create an object of class `mts_monitor`
#' for use with the [AirMonitor](https://mazamascience.github.io/AirMonitor/) package.
#'
#' @return An AirMonitor package *mts_monitor* object.
#'
#' @references [PurpleAir](https://www2.purpleair.com)
#' @references [PurpleAir API](https://api.purpleair.com)
#' @references [PurpleAir Terms of service](https://www2.purpleair.com/policies/terms-of-service)
#' @references [PurpleAir Data license](https://www2.purpleair.com/pages/license)
#' @references [PurpleAir Data Attribution](https://www2.purpleair.com/pages/attribution)
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' # AirSensor2 package
#' library(AirSensor2)
#' monitor <-
#'   example_pat_epa %>%
#'   pat_toMonitor()
#'
#' AirMonitor::monitor_timeseriesPlot(monitor, shadedNight = TRUE)
#'
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

  meta_df <- pat$meta

  model_value <-
    if ("model" %in% names(meta_df))
      meta_df$model
  else
    "PurpleAir sensor"

  meta <-
    meta_df %>%
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
      deviceType = model_value,
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
