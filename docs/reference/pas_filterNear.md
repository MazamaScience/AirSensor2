# Find PurpleAir sensors within radial distance

Filter for PurpleAir sensors within a specified distance from a target
location.

## Usage

``` r
pas_filterNear(pas = NULL, longitude = NULL, latitude = NULL, radius = "1 km")
```

## Arguments

- pas:

  PurpleAir \*pas\* object.

- longitude:

  a Target longitude.

- latitude:

  a Target latitude.

- radius:

  Distance from target with unit (i.e "15 km").

## Value

A subset of the incoming \*pas\* object.

## Details

The \`radius\` parameter should be a numeric string with a metric unit
separated by a space, such as \`"250 m"\` or \`"10 km"\`.

## See also

\[pas_filter()\]

\[pas_filterArea()\]

\[pas_filterNearMonitor()\]

## Examples

``` r
library(AirSensor2)

# Near Omak, WA
Omak_pas <-
  example_pas_pm25 %>%
  pas_filterNear(
    longitude = -119.5375,
    latitude = 48.4125,
    radius = "20 km"
  )

if ( interactive() ) {
  pas_leaflet(Omak_pas)
}
```
