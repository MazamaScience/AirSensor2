# Check the validity and type for the provided \`api_key\`

Sends a request to the PurpleAirAPI API endpoint described at:
<https://api.purpleair.com/#api-keys-check-api-key>

## Usage

``` r
PurpleAir_checkAPIKey(
  api_key = NULL,
  baseUrl = "https://api.purpleair.com/v1/keys"
)
```

## Arguments

- api_key:

  PurpleAir API key.

- baseUrl:

  URL endpoint for the "Check Key" API.

## Value

List containing key type information.

## Examples

``` r
# \donttest{
# Fail gracefully if any resources are not available
try({

library(AirSensor2)

  PurpleAir_checkAPIKey(
    api_key = PurpleAir_API_READ_KEY
  )

}, silent = FALSE)
#> Error in eval(expr, envir) : object 'PurpleAir_API_READ_KEY' not found
# }
```
