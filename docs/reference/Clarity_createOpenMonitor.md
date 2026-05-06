# Create a new Clarity 'mts_monitor' object

Download, parse and enhance hourly timeseries data from Clarity and
create an object of class \`mts_monitor\` for use with the AirMonitor
package.

## Usage

``` r
Clarity_createOpenMonitor(
  api_key = NULL,
  synoptic = NULL,
  datasourceId = NULL,
  format = "USFS2",
  parameter = c("pm2.5", "nowcast"),
  applyQC = TRUE
)
```

## Arguments

- api_key:

  Clarity API READ Key. If \`api_key = NULL\`, it will be obtained using
  \`getAPIKey("Clarity-read")\`.

- synoptic:

  Previously generated \*synoptic\* object containing \`datasourceId\`.

- datasourceId:

  Clarity sensor identifier.

- format:

  Customized output format ("USFS2", "USFS").

- parameter:

  Parameter to use for data ("pm2.5" or "nowcast")

- applyQC:

  Logical specifying whether to use the Clarity QCFlag to invalidate
  data values.

## Value

An AirMonitor package \*mts_monitor\* object.

## Examples

``` r
# \donttest{
# Fail gracefully if any resources are not available
try({

library(AirSensor2)

initializeMazamaSpatialUtils()

synoptic <-
  Clarity_createOpenSynoptic(
    api_key = Clarity_API_READ_KEY
  )

mon <-
  Clarity_createOpenMonitor(
    api_key = Clarity_API_READ_KEY,
    synoptic = synoptic,
    datasourceId = "DACHW7097",
    parameter = "pm2.5"
  )

}, silent = FALSE)
#> Error in eval(expr, envir) : object 'Clarity_API_READ_KEY' not found
# }
```
