# Test getting data from a single sensor

library(AirSensor2)

MazamaCoreUtils::getAPIKey(PA_API_READ_KEY)
apiReadKey = PA_API_READ_KEY
maxAge = 3600 * 24
outsideOnly = TRUE
west = -180  #-125
east = 180   #-117
south = -90  #42
north = 90   #49
baseUrl = "https://api.purpleair.com/v1/sensors:sensor_index"

webserviceUrl <- baseUrl

fields <-
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
    "primary_id_a, primary_key_a, secondary_id_a, secondary_key_a, primary_id_b, primary_key_b, secondary_id_b, secondary_key_b",
    sep = ",",
    collapse = ","
  ) %>%
  stringr::str_replace_all(" ", "")

queryList <-
  list(
    fields = fields,
    sensor_index = "131075"
  )

# NOTE:  Didn't get it to work but the API docs say that this only gets data
# NOTE:  associated with the most recent timestamp.



