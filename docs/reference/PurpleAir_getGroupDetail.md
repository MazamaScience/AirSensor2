# Retrieve all members of the specified group

Sends a request to the PurpleAirAPI API endpoint described at:
<https://api.purpleair.com/#api-groups-get-group-detail>

## Usage

``` r
PurpleAir_getGroupDetail(
  api_key = NULL,
  group_id = NULL,
  baseUrl = "https://api.purpleair.com/v1/groups"
)
```

## Arguments

- api_key:

  PurpleAir API READ key.

- group_id:

  The \`group_id\` of the requested group. This group must be owned by
  the \`api_key\`.

- baseUrl:

  URL endpoint for the "Get Group Detail" API.

## Value

List containing all members of the specified group.

## Examples

``` r
# \donttest{
# Fail gracefully if any resources are not available
try({

library(AirSensor2)

  PurpleAir_getGroupDetail(
    api_key = PurpleAir_API_READ_KEY,
    group_id = MY_GROUP_ID
  )

}, silent = FALSE)
#> Error in eval(expr, envir) : object 'PurpleAir_API_READ_KEY' not found
# }
```
