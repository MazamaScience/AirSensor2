# Retrieve recent data for a single sensor in the specified group

Sends a request to the PurpleAir API endpoint described at:
<https://api.purpleair.com/#api-groups-get-member-data>

## Usage

``` r
PurpleAir_getMemberData(
  api_key = NULL,
  group_id = NULL,
  member_id = NULL,
  fields = NULL,
  baseUrl = "https://api.purpleair.com/v1/groups"
)
```

## Arguments

- api_key:

  PurpleAir API READ key.

- group_id:

  The \`group_id\` of the requested group. This group must be owned by
  the \`api_key\`.

- member_id:

  Unique \`member_id\` for a sensor within \`group_id\`.

- fields:

  Optional parameter specifying sensor data fields to return.

- baseUrl:

  URL endpoint for the "Get Member Data" API.

## Value

List containing all recent data for a single sensor.

## Examples

``` r
# \donttest{
# Fail gracefully if any resources are not available
try({

library(AirSensor2)

  PurpleAir_getMemberData(
    api_key = PurpleAir_API_READ_KEY,
    group_id = MY_GROUP_ID,
    member_id = MY_MEMBER_ID
  )

}, silent = FALSE)
#> Error in eval(expr, envir) : object 'PurpleAir_API_READ_KEY' not found
# }
```
