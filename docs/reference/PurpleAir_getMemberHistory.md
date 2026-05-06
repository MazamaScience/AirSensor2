# Retrieve historical data for a single sensor of the specified group

Sends a request to the PurpleAirAPI API endpoint described at:
<https://api.purpleair.com/#api-groups-get-member-history>

## Usage

``` r
PurpleAir_getMemberHistory(
  api_key = NULL,
  group_id = NULL,
  member_id = NULL,
  start_timestamp = NULL,
  end_timestamp = NULL,
  average = 10,
  fields = PurpleAir_PAT_QC_FIELDS,
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

- start_timestamp:

  Desired start datetime (ISO 8601).

- end_timestamp:

  Desired end datetime (ISO 8601).

- average:

  Temporal averaging in minutes performed by PurpleAir. One of: 0 (raw),
  10, 30, 60 (hour), 360, 1440 (day).

- fields:

  Character string specifying which 'sensor data fields' to include in
  the response.

- baseUrl:

  URL endpoint for the "Get Groups list" API.

## Value

Tibble with historical data for a single sensor.

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
