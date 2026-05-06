# Retrieve current individual records for all Open Data sensors.

Sends a request to the Clarity API endpoint for Open Data.

When \`format = "USFS2"\`, two additional fields are returned in the
"meta" dataframe: \`calibrationId\` and \`calibrationCategory\`.

- \*Measurement data from\* – All open datasources

- \*Measurements returned\* – Individual values for last complete hour
  (hour-aligned) plus fraction of current hour

- \*PM2.5 Mass Concentration\* – Individual sample

## Usage

``` r
Clarity_getAllOpenIndividual(
  api_key = NULL,
  format = c("USFS2", "USFS"),
  baseUrl =
    "https://clarity-data-api.clarity.io/v1/open/all-recent-measurement/pm25/individual"
)
```

## Arguments

- api_key:

  Clarity API READ Key. If \`api_key = NULL\`, it will be obtained using
  \`getAPIKey("Clarity-read")\`.

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

  Clarity_getAllIndividualOpen(
    api_key = Clarity_API_READ_KEY,
    format = "USFS2"
  )

}, silent = FALSE)
#> Error in Clarity_getAllIndividualOpen(api_key = Clarity_API_READ_KEY,  : 
#>   could not find function "Clarity_getAllIndividualOpen"
# }
```
