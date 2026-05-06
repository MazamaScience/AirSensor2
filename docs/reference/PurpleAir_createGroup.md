# Create a new group

Sends a request to the PurpleAirAPI API endpoint described at:
<https://api.purpleair.com/#api-groups-create-group>

## Usage

``` r
PurpleAir_createGroup(
  api_key = NULL,
  name = NULL,
  baseUrl = "https://api.purpleair.com/v1/groups"
)
```

## Arguments

- api_key:

  PurpleAir API WRITE key.

- name:

  Human readable name associated with the new group.

- baseUrl:

  URL endpoint for the "Create Group" API.

## Value

List containing all members of the specified group.

## Examples

``` r
# \donttest{
# Fail gracefully if any resources are not available
try({

library(AirSensor2)

  PurpleAir_createGroup(
    api_key = PurpleAir_API_WRITE_KEY,
    name = "My new group"
  )

}, silent = FALSE)
#> Error in eval(expr, envir) : object 'PurpleAir_API_WRITE_KEY' not found
# }
```
