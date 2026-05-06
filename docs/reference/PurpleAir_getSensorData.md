# Retrieve the latest data of a single sensor

Sends a request to the PurpleAir API endpoint described at:
<https://api.purpleair.com/#api-sensors-get-sensor-data>

## Usage

``` r
PurpleAir_getSensorData(
  api_key = NULL,
  sensor_index = NULL,
  fields = PurpleAir_PAList_PM25_FIELDS,
  baseUrl = "https://api.purpleair.com/v1/sensors"
)
```

## Arguments

- api_key:

  PurpleAir API READ key.

- sensor_index:

  The \`sensor_index\` as found in the JSON for this specific sensor.

- fields:

  Optional parameter specifying sensor data fields to return.

- baseUrl:

  URL endpoint for the "Get Member Data" API.

## Value

List containing all recent data for a single sensor.

## Examples

``` r
# \donttest{
# Fail gracefully if any resources are not available
try({

library(AirSensor2)

  PurpleAir_getSensorData(
    api_key = PurpleAir_API_READ_KEY,
    sensor_index = MY_SENSOR_INDEX,
    fields = PurpleAir_PAList_PM25_FIELDS
  )

}, silent = FALSE)
#> Error in eval(expr, envir) : object 'PurpleAir_API_READ_KEY' not found
# }
```
