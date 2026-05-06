# Create a new member within the specified group

Sends a request to the PurpleAir API endpoint described at:
<https://api.purpleair.com/#api-groups-create-member>

## Usage

``` r
PurpleAir_createMember(
  api_key = NULL,
  group_id = NULL,
  sensor_index = NULL,
  baseUrl = "https://api.purpleair.com/#api-groups-create-member"
)
```

## Arguments

- api_key:

  PurpleAir API WRITE key.

- group_id:

  The \`group_id\` of the requested group. This group must be owned by
  the \`api_key\`.

- sensor_index:

  Sensor index as returned by \`PurpleAir_getSensorsData()\`.

- baseUrl:

  URL endpoint for the "Create Member" API.

## Value

List containing data associated with this this sensor.

## Examples

``` r
# \donttest{
# Fail gracefully if any resources are not available
try({

library(AirSensor2)

  PurpleAir_getGroupDetail(
    api_key = PurpleAir_API_READ_KEY,
    group_id = MY_GROUP_ID,
    sensor_index = MY_SENSOR_INDEX
  )

}, silent = FALSE)
#> Error in PurpleAir_getGroupDetail(api_key = PurpleAir_API_READ_KEY, group_id = MY_GROUP_ID,  : 
#>   unused argument (sensor_index = MY_SENSOR_INDEX)
# }
```
