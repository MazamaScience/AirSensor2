# Create a raw data PurpleAir timeseries dataset.

Create a \`pat\` object for a specific \`sensor_index\`. This function
splits up the requested time range into 2-day intervals (the maximum
allowed by the PurpleAir API) and makes repeated calls to
\`pat_downloadParseRawData()\`. The \`sleep\` parameter waits a small
amount of time between API requests.

The PurpleAir API will respond with "rate limiting" errors unless sleep
is set appropriately. When \`parallel = TRUE\`, \`sleep\` is ignored.

## Usage

``` r
pat_createRaw(
  api_key = NULL,
  pas = NULL,
  sensor_index = NULL,
  startdate = NULL,
  enddate = NULL,
  timezone = "UTC",
  fields = PurpleAir_PAT_QC_FIELDS,
  read_keys = NULL,
  sleep = 0.5,
  parallel = FALSE,
  baseUrl = "https://api.purpleair.com/v1/sensors",
  verbose = TRUE
)
```

## Arguments

- api_key:

  PurpleAir API Read Key. If \`api_key = NULL\`, it will be obtained
  using \`getAPIKey("PurpleAir-read")\`. See
  \[MazamaCoreUtils::setAPIKey()\].

- pas:

  Previously generated \*pas\* object containing \`sensor_index\`.

- sensor_index:

  PurpleAir sensor unique identifier.

- startdate:

  Desired start time (ISO 8601) or \`POSIXct\`.

- enddate:

  Desired end time (ISO 8601) or \`POSIXct\`.

- timezone:

  Olson timezone used to interpret dates.

- fields:

  Character string with PurpleAir field names for the Get Sensor Data
  API.

- read_keys:

  Optional, comma separated list of sensor read_keys is required for
  private devices. It is separate from the api_key and each sensor has
  its own read_key. Submit multiple keys by separating them with a comma
  (,) character for example: \`"key-one,key-two,key-three"\`.

- sleep:

  Seconds to sleep between API requests.

- parallel:

  Logical specifying whether to attempt simultaneous downloads using
  \[parallel::mcparallel()\]. (Not available on Windows.)

- baseUrl:

  Base URL for the PurpleAir API.

- verbose:

  Logical controlling the generation of warning and error messages.

## Value

A raw PurpleAir Timeseries \*pat\* object.

## Note

Parallel processing using \`parallel = TRUE\` is not available on
Windows machines.

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

# AirSensor2 package
library(AirSensor2)

# Set user's PurpleAir_API_READ_KEY
source('global_vars.R')
setAPIKey("PurpleAir-read", PurpleAir_API_READ_KEY)

# Initialize spatial datasets
initializeMazamaSpatialUtils()

pat <-
  pat_createRaw(
    pas = example_pas_pm25,
    sensor_index = "76545",
    startdate = "2023-01-01",
    enddate = "2023-01-03",
    timezone = "UTC",
    verbose = TRUE
  )

View(pat$data[1:100,])

}, silent = FALSE)
#> Warning: cannot open file 'global_vars.R': No such file or directory
#> Error in file(filename, "r", encoding = encoding) : 
#>   cannot open the connection
# }
```
