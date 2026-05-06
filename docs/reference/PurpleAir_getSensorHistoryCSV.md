# Retrieve historical data for a single sensor as CSV

Sends a request to the PurpleAir API endpoint described at:
<https://api.purpleair.com/#api-sensors-get-sensor-history-csv>

## Usage

``` r
PurpleAir_getSensorHistoryCSV(
  api_key = NULL,
  sensor_index = NULL,
  start_timestamp = NULL,
  end_timestamp = NULL,
  average = 10,
  fields = PurpleAir_PAT_QC_FIELDS,
  read_keys = NULL,
  baseUrl = "https://api.purpleair.com/v1/sensors"
)
```

## Arguments

- api_key:

  PurpleAir API READ key.

- sensor_index:

  The \`sensor_index\` as found in the JSON for this specific sensor.

- start_timestamp:

  Optional Unix timestamp in seconds since Jan 1, 1970.

- end_timestamp:

  Optional Unix timestamp in seconds since Jan 1, 1970.

- average:

  Temporal averaging in minutes performed by PurpleAir. One of: 0 (raw),
  10, 30, 60 (hour), 360, 1440 (day).

- fields:

  Character string specifying which 'sensor data fields' to include in
  the response.

- read_keys:

  Optional, comma separated list of sensor read_keys is required for
  private devices. It is separate from the api_key and each sensor has
  its own read_key. Submit multiple keys by separating them with a comma
  (,) character for example: \`"key-one,key-two,key-three"\`.

- baseUrl:

  URL endpoint for the "Get Sensor History (CSV)" API.

## Value

Tibble with historical data for a single sensor.

## Examples

``` r
# \donttest{
# Fail gracefully if any resources are not available
try({

library(AirSensor2)

start <-
  MazamaCoreUtils::parseDatetime("2023-01-29 00:00:00", timezone = "UTC") %>%
  as.numeric()

end <-
  MazamaCoreUtils::parseDatetime("2023-01-30 00:00:00", timezone = "UTC") %>%
  as.numeric()

PurpleAir_getSensorHistoryCSV(
  api_key = PurpleAir_API_READ_KEY,
  sensor_index = 896,
  start_timestamp = start,
  end_timestamp = end,
  average = 0,
  fields = PurpleAir_PAT_QC_FIELDS
)

}, silent = FALSE)
#> Error in eval(expr, envir) : object 'PurpleAir_API_READ_KEY' not found
# }
```
