# Retrieve the latest data of multiple sensors matching the provided parameters

Sends a request to the PurpleAir API endpoint described at:
<https://api.purpleair.com/#api-sensors-get-sensors-data>

If \`show_only\` is used to request specific sensors, the bounding box
information is ignored.

## Usage

``` r
PurpleAir_getSensorsData(
  api_key = NULL,
  fields = PurpleAir_PAS_MINIMAL_FIELDS,
  location_type = NULL,
  read_keys = NULL,
  show_only = NULL,
  modified_since = NULL,
  max_age = 0,
  nwlng = NULL,
  nwlat = NULL,
  selng = NULL,
  selat = NULL,
  baseUrl = "https://api.purpleair.com/v1/sensors"
)
```

## Arguments

- api_key:

  PurpleAir API READ key.

- fields:

  Optional parameter specifying sensor data fields to return.

- location_type:

  The \`location_type\` of the sensors. Possible values are: 0 =
  Outside, 1 = Inside or \`NULL\` = both.

- read_keys:

  Optional comma separated list of sensor read_keys is required for
  private devices. It is separate to the api_key and each sensor has its
  own read_key. Submit multiple keys by separating them with a comma (,)
  character for example: key-one,key-two,key-three.

- show_only:

  Optional comma separated list of sensor_index values. When provided,
  the results are limited only to the sensors included in this list.

- modified_since:

  The modified_since parameter causes only sensors modified after the
  provided time stamp to be included in the results. Using the
  time_stamp value from a previous call (recommended) will limit results
  to those with new values since the last request. Using a value of 0
  will match sensors modified at any time.

- max_age:

  Filter results to only include sensors modified or updated within the
  last \`max_age\` seconds. Using a value of 0 will match sensors of any
  age.

- nwlng:

  A north west longitude for the bounding box.

- nwlat:

  A north west latitude for the bounding box.

- selng:

  A south east longitude for the bounding box.

- selat:

  A south east latitude for the bounding box.

- baseUrl:

  URL endpoint for the "Get Member Data" API.

## Value

List containing latest data for multiple sensors.

## Examples

``` r
# \donttest{
# Fail gracefully if any resources are not available
try({

library(AirSensor2)

  PurpleAir_getSensorsData(
    api_key = PurpleAir_API_READ_KEY,
    fields = PurpleAir_PAS_MINIMAL_FIELDS
  )

}, silent = FALSE)
#> Error in eval(expr, envir) : object 'PurpleAir_API_READ_KEY' not found
# }
```
