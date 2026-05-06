# Create a new Clarity synoptic dataset

Download, parse and enhance synoptic data from Clarity and return the
results as a useful tibble with class \`clarity_synoptic\`.

Steps include:

1\) Download and parse synoptic data

2\) Replace variable names with more consistent, more human readable
names.

3\) Add spatial metadata for each sensor including:

- timezone – olson timezone

- countryCode – ISO 3166-1 alpha-2

- stateCode – ISO 3166-2 alpha-2

4\) Convert data types from character to \`POSIXct\` and \`numeric\`.

## Usage

``` r
Clarity_createOpenSynoptic(
  api_key = NULL,
  format = c("USFS2", "USFS"),
  baseUrl =
    "https://clarity-data-api.clarity.io/v1/open/all-recent-measurement/pm25/hourly"
)
```

## Arguments

- api_key:

  Clarity API READ Key. If \`api_key = NULL\`, it will be obtained using
  \`getAPIKey("Clarity-read")\`.

- format:

  Customized output format ("USFS2", "USFS").

- baseUrl:

  Base URL for the PurpleAir API.

## Value

A Clarity Synoptic \*pas\* object.

## Examples

``` r
# \donttest{
# Fail gracefully if any resources are not available
try({

library(AirSensor2)

initializeMazamaSpatialUtils()

synoptic <-
  Clarity_createOpenSynoptic(
    api_key = Clarity_API_READ_KEY,
    format = "USFS2"
  )

pas %>% pas_leaflet()

}, silent = FALSE)
#> Error in eval(expr, envir) : object 'Clarity_API_READ_KEY' not found
# }
```
