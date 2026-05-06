# Download time series data from PurpleAir

Download and parse time series data for a specific \`sensor_index\`.

## Usage

``` r
pat_downloadParseRawData(
  api_key = NULL,
  sensor_index = NULL,
  startdate = NULL,
  enddate = NULL,
  timezone = "UTC",
  average = 0,
  fields = PurpleAir_PAT_QC_FIELDS,
  read_keys = NULL,
  baseUrl = "https://api.purpleair.com/v1/sensors"
)
```

## Arguments

- api_key:

  PurpleAir API Read Key. If \`api_key = NULL\`, it will be obtained
  using \`getAPIKey("PurpleAir-read")\`. See
  \[MazamaCoreUtils::setAPIKey()\].

- sensor_index:

  PurpleAir sensor unique identifier.

- startdate:

  Desired start datetime (ISO 8601).

- enddate:

  Desired end datetime (ISO 8601).

- timezone:

  Olson timezone used to interpret dates.

- average:

  Temporal averaging in minutes performed by PurpleAir. One of: 0 (raw),
  10, 30, 60 (hour), 360, 1440 (day).

- fields:

  Character string with PurpleAir field names for the Get Sensor Data
  API.

- read_keys:

  Optional, comma separated list of sensor read_keys is required for
  private devices. It is separate from the api_key and each sensor has
  its own read_key. Submit multiple keys by separating them with a comma
  (,) character for example: \`"key-one,key-two,key-three"\`.

- baseUrl:

  Base URL for the PurpleAir API.

## Value

Dataframe of time series PurpleAir data.

## References

\[PurpleAir\](https://www2.purpleair.com)

\[PurpleAir API\](https://api.purpleair.com)

\[PurpleAir Terms of
service\](https://www2.purpleair.com/policies/terms-of-service)

\[PurpleAir Data license\](https://www2.purpleair.com/pages/license)

\[PurpleAir Data
Attribution\](https://www2.purpleair.com/pages/attribution)

## Examples

``` r
# \donttest{
# Fail gracefully if any resources are not available
try({

library(AirSensor2)

# Hourly (uncorrected) PM2.5
pat_raw <-
  pat_downloadParseRawData(
    api_key = PurpleAir_API_READ_KEY,
    sensor_index = "2323",
    startdate = "2023-02-01",
    enddate = "2023-02-03",
    timezone = "UTC",
    average = 60,
    fields = "pm2.5_atm"
  )

head(pat_raw)

}, silent = FALSE)
#> Error in eval(expr, envir) : object 'PurpleAir_API_READ_KEY' not found
# }
```
