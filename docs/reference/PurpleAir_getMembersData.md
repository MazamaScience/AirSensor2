# Retrieve current data for all sensors in the specified group

Sends a request to the PurpleAir API endpoint described at:
<https://api.purpleair.com/#api-groups-get-members-data>

Retrieves data for all sensors in the specified group.

## Usage

``` r
PurpleAir_getMembersData(
  api_key = NULL,
  group_id = NULL,
  fields = PurpleAir_PAList_PM25_FIELDS,
  location_type = NULL,
  max_age = 604800,
  baseUrl = "https://api.purpleair.com/v1/groups"
)
```

## Arguments

- api_key:

  PurpleAir API READ key.

- group_id:

  The \`group_id\` of the requested group. This group must be owned by
  the \`api_key\`.

- fields:

  Comma-separated list of 'sensor data fields' to include in the
  response.

- location_type:

  The \`location_type\` of the sensors. Possible values are: 0 =
  Outside, 1 = Inside or \`NULL\` = both.

- max_age:

  Filter results to only include sensors modified or updated within the
  last \`max_age\` seconds. Using a value of 0 will match sensors of any
  age.

- baseUrl:

  URL endpoint for the "Get Members Data" API.

## Value

List containing current data for all sensors in the specified group.

## Examples

``` r
# \donttest{
# Fail gracefully if any resources are not available
try({

library(AirSensor2)

  PurpleAir_getMembersData(
    api_key = PurpleAir_API_READ_KEY,
    group_id = MY_GROUP_ID
  )

}, silent = FALSE)
#> Error in eval(expr, envir) : object 'PurpleAir_API_READ_KEY' not found
# }
```
