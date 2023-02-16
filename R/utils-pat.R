
#' pat_PM25_FIELDS
#'
#' @export
#' @docType data
#' @name pat_PM25_FIELDS
#' @title Comma-separated list of fields needed for PM2.5 data analysis.
#' @format String with comma-separated field names
#' @description Character string with default PurpleAir field names used in
#' \code{pat_downloadParaseRawData()}. These fields are sufficient for most
#' QC algorithms and include most of the "information and status" fields,
#' "humidity", "temperature", "pressure" and the PM2.5 "pseudo average" fields.
#'
#' @references \href{https://api.purpleair.com/#api-sensors-get-sensor-history-csv}{Get Sensor History API}

# From: https://api.purpleair.com/#api-sensors-get-sensor-history-csv
#
# The 'Fields' parameter specifies which 'sensor data fields' to include in the
# response. Not all fields are available as history fields and we will be working
# to add more as time goes on. Fields marked with an asterisk (*) may not be
# available when using averages. It is a comma separated list with one or more of the following:
#
# Station information and status fields:
#   hardware*, latitude*, longitude*, altitude*, firmware_version*, rssi, uptime, pa_latency, memory,
#
# Environmental fields:
#   humidity, humidity_a, humidity_b, temperature, temperature_a, temperature_b, pressure, pressure_a, pressure_b
#
# Miscellaneous fields:
#   voc, voc_a, voc_b, analog_input
#
# PM1.0 fields:
#   pm1.0_atm, pm1.0_atm_a, pm1.0_atm_b, pm1.0_cf_1, pm1.0_cf_1_a, pm1.0_cf_1_b
#
# PM2.5 fields:
#   pm2.5_alt, pm2.5_alt_a, pm2.5_alt_b, pm2.5_atm, pm2.5_atm_a, pm2.5_atm_b, pm2.5_cf_1, pm2.5_cf_1_a, pm2.5_cf_1_b
#
# PM10.0 fields:
#   pm10.0_atm, pm10.0_atm_a, pm10.0_atm_b, pm10.0_cf_1, pm10.0_cf_1_a, pm10.0_cf_1_b
#
# Visibility fields:
#   scattering_coefficient, scattering_coefficient_a, scattering_coefficient_b, deciviews, deciviews_a, deciviews_b, visual_range, visual_range_a, visual_range_b
#
# Particle count fields:
#   0.3_um_count, 0.3_um_count_a, 0.3_um_count_b, 0.5_um_count, 0.5_um_count_a, 0.5_um_count_b, 1.0_um_count, 1.0_um_count_a, 1.0_um_count_b, 2.5_um_count, 2.5_um_count_a, 2.5_um_count_b, 5.0_um_count, 5.0_um_count_a, 5.0_um_count_b, 10.0_um_count, 10.0_um_count_a, 10.0_um_count_b
#
# For field descriptions, please see the 'sensor data fields'. section.

pat_PM25_FIELDS <-
  paste(
    # Station information and status fields:
    "hardware, latitude, longitude, altitude, firmware_version, rssi, uptime, pa_latency, memory",
    # Environmental fields:
    "humidity, temperature, pressure",
    # Miscellaneous fields:
    # PM1.0 fields:
    # PM2.5 fields:
    "pm2.5_alt, pm2.5_alt_a, pm2.5_alt_b, pm2.5_atm, pm2.5_atm_a, pm2.5_atm_b, pm2.5_cf_1, pm2.5_cf_1_a, pm2.5_cf_1_b",
    # PM10.0 fields:
    # Visibility fields:
    # Particle count fields:
    sep = ",",
    collapse = ","
  ) %>%
  stringr::str_replace_all(" ", "")

