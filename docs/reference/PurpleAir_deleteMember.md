# Delete a member from the specified group

Sends a request to the PurpleAirAPI API endpoint described at:
<https://api.purpleair.com/#api-groups-delete-member>

## Usage

``` r
PurpleAir_deleteMember(
  api_key = NULL,
  group_id = NULL,
  member_id = NULL,
  baseUrl = "https://api.purpleair.com/v1/groups"
)
```

## Arguments

- api_key:

  PurpleAir API WITE key.

- group_id:

  The \`group_id\` of the requested group.

- member_id:

  The \`member_id\` to be deleted.

- baseUrl:

  URL endpoint for the "Delete Member" API.

## Value

No return.

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
