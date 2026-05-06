# Retrieve all groups owned by the provided \`api_key\`

Sends a request to the PurpleAirAPI API endpoint described at:
<https://api.purpleair.com/#api-groups-get-groups-list>

## Usage

``` r
PurpleAir_getGroupsList(
  api_key = NULL,
  baseUrl = "https://api.purpleair.com/v1/groups"
)
```

## Arguments

- api_key:

  PurpleAir API READ ey.

- baseUrl:

  URL endpoint for the "Get Groups List" API.

## Value

List containing all groups owned by \`api_key\`.

## Examples

``` r
# \donttest{
# Fail gracefully if any resources are not available
try({

library(AirSensor2)

  PurpleAir_getGroupsList(
    api_key = PurpleAir_API_READ_KEY
  )

}, silent = FALSE)
#> Error in eval(expr, envir) : object 'PurpleAir_API_READ_KEY' not found
# }
```
