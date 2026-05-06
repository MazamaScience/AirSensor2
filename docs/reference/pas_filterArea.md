# Rectangle area filtering for PurpleAir Synoptic objects

Filters \*pas\* object sensors based on a bounding box.

## Usage

``` r
pas_filterArea(pas = NULL, w = NULL, e = NULL, s = NULL, n = NULL)
```

## Arguments

- pas:

  PurpleAir Synoptic \*pas\* object.

- w:

  West edge of area bounding box (deg E).

- e:

  East edge of area bounding box (deg E).

- s:

  South edge of area bounding box (deg N).

- n:

  North edge of area bounding box (deg N).

## Value

A subset of the given \*pas\* object containing only records within the
bounding box. This includes sensors located precisely on a boundary.

## See also

\[pas_filter()\]

\[pas_filterNear()\]

\[pas_filterNearMonitor()\]

## Examples

``` r
library(AirSensor2)

pas <- example_pas_pm25
range(pas$longitude)
#> [1] -124.6264 -116.9615
range(pas$latitude)
#> [1] 42.01015 48.99036

Lane_County_pas <-
  pas %>%
  pas_filterArea(
    w = -124.16,
    e = -121.76,
    s = 43.43,
    n = 44.30
  )

range(Lane_County_pas$longitude)
#> [1] -124.1012 -121.9334
range(Lane_County_pas$latitude)
#> [1] 43.51040 44.28334

if ( interactive() ) {
  pas_leaflet(Lane_County_pas)
}
```
