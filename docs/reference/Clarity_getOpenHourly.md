# Retrieve current hourly data from a single source.

Sends a request to the Clarity API endpoint for Open Data.

When \`format = "USFS2"\`, two additional fields are returned in the
"meta" dataframe: \`calibrationId\` and \`calibrationCategory\`.

- \*Measurement data from\* – Drill down on one open datasource

- \*Measurements returned\* – Hourly values for last 10 days

- \*PM2.5 Mass Concentration\* – 1-Hour Mean Nowcast

## Usage

``` r
Clarity_getOpenHourly(
  api_key = NULL,
  datasourceId = NULL,
  format = "USFS2",
  baseUrl = "https://clarity-data-api.clarity.io/v1/open/datasource-measurement"
)
```

## Arguments

- api_key:

  Clarity API READ Key. If \`api_key = NULL\`, it will be obtained using
  \`getAPIKey("Clarity-read")\`.

- datasourceId:

  Clarity sensor identifier.

- format:

  Customized output format ("USFS2", "USFS").

- baseUrl:

  URL endpoint.

## Value

List containing five data frames: \`meta\`, \`pm2.5_QCFlag\`, \`pm2.5\`,
\`nowcast_QCFlag\` and \`nowcast\`.

## Examples

``` r
# \donttest{
# Fail gracefully if any resources are not available
try({

library(AirSensor2)

  Clarity_getOpenHourly(
    api_key = Clarity_API_READ_KEY,
    datasourceId = "DAABL1560",
    format = "USFS2"
  )

}, silent = FALSE)
#> Error in eval(expr, envir) : object 'Clarity_API_READ_KEY' not found
# }
```
