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

#' pas_PM25_FIELDS
#'
#' @export
#' @docType data
#' @name pas_PM25_FIELDS
#' @title Comma-separated list of fields needed for PM2.5 data analysis.
#' @format String with comma-separated field names
#' @description Character string with default PurpleAir field names used in
#' \code{pas_downloadParaseRawData()}. These fields are sufficient for most
#' QC algorithms and include most of the "information and status" fields,
#' "humidity", "temperature", "pressure" and the PM2.5 "pseudo average" fields.
#'
#' @references \href{https://api.purpleair.com/#api-sensors-get-sensor-data}{Get Sensor Data API}

# From: https://api.purpleair.com/#api-sensors-get-sensor-data
#
# The 'Fields' parameter specifies which 'sensor data fields' to include in the response. It is a comma separated list with one or more of the following:
#
# Station information and status fields:
#   name, icon, model, hardware, location_type, private, latitude, longitude, altitude, position_rating, led_brightness, firmware_version, firmware_upgrade, rssi, uptime, pa_latency, memory, last_seen, last_modified, date_created, channel_state, channel_flags, channel_flags_manual, channel_flags_auto, confidence, confidence_manual, confidence_auto
#
# Environmental fields:
#   humidity, humidity_a, humidity_b, temperature, temperature_a, temperature_b, pressure, pressure_a, pressure_b
#
# Miscellaneous fields:
#   voc, voc_a, voc_b, ozone1, analog_input
#
# PM1.0 fields:
#   pm1.0, pm1.0_a, pm1.0_b, pm1.0_atm, pm1.0_atm_a, pm1.0_atm_b, pm1.0_cf_1, pm1.0_cf_1_a, pm1.0_cf_1_b
#
# PM2.5 fields:
#   pm2.5_alt, pm2.5_alt_a, pm2.5_alt_b, pm2.5, pm2.5_a, pm2.5_b, pm2.5_atm, pm2.5_atm_a, pm2.5_atm_b, pm2.5_cf_1, pm2.5_cf_1_a, pm2.5_cf_1_b
#
# PM2.5 pseudo (simple running) average fields:
#   pm2.5_10minute, pm2.5_10minute_a, pm2.5_10minute_b, pm2.5_30minute, pm2.5_30minute_a, pm2.5_30minute_b, pm2.5_60minute, pm2.5_60minute_a, pm2.5_60minute_b, pm2.5_6hour, pm2.5_6hour_a, pm2.5_6hour_b, pm2.5_24hour, pm2.5_24hour_a, pm2.5_24hour_b, pm2.5_1week, pm2.5_1week_a, pm2.5_1week_b
#
# PM10.0 fields:
#   pm10.0, pm10.0_a, pm10.0_b, pm10.0_atm, pm10.0_atm_a, pm10.0_atm_b, pm10.0_cf_1, pm10.0_cf_1_a, pm10.0_cf_1_b
#
# Particle count fields:
#   0.3_um_count, 0.3_um_count_a, 0.3_um_count_b, 0.5_um_count, 0.5_um_count_a, 0.5_um_count_b, 1.0_um_count, 1.0_um_count_a, 1.0_um_count_b, 2.5_um_count, 2.5_um_count_a, 2.5_um_count_b, 5.0_um_count, 5.0_um_count_a, 5.0_um_count_b, 10.0_um_count 10.0_um_count_a, 10.0_um_count_b
#
# ThingSpeak fields, used to retrieve data from api.thingspeak.com:
#   primary_id_a, primary_key_a, secondary_id_a, secondary_key_a, primary_id_b, primary_key_b, secondary_id_b, secondary_key_b

pas_PM25_FIELDS <-
  paste(
    # Station information and status fields:
    "name, icon, model, hardware, location_type, private, latitude, longitude, altitude, position_rating, led_brightness, firmware_version, firmware_upgrade, rssi, uptime, pa_latency, memory, last_seen, last_modified, date_created, channel_state, channel_flags, channel_flags_manual, channel_flags_auto, confidence, confidence_manual, confidence_auto",
    # Environmental fields:
    "humidity, temperature, pressure",
    # Miscellaneous fields:
    # PM1.0 fields:
    # PM2.5 fields:
    # PM2.5 pseudo average fields:
    "pm2.5_10minute, pm2.5_30minute, pm2.5_60minute, pm2.5_6hour, pm2.5_24hour, pm2.5_1week",
    # PM10.0 fields:
    # Particle count field:
    # ThingSpeak fields:
    # "primary_id_a, primary_key_a, secondary_id_a, secondary_key_a, primary_id_b, primary_key_b, secondary_id_b, secondary_key_b",
    sep = ",",
    collapse = ","
  ) %>%
  stringr::str_replace_all(" ", "")

