# Find PurpleAir sensors near a monitor

Filter for PurpleAir sensors within a specified distance from a
monitoring site.

## Usage

``` r
pas_filterNearMonitor(pas = NULL, monitor = NULL, radius = "1 km")
```

## Arguments

- pas:

  PurpleAir \*pas\* object.

- monitor:

  \*mts_monitor\* object from the AirMonitor package.

- radius:

  Distance from target with unit (i.e "1000 m").

## Value

A subset of the incoming \*pas\* object with additional fields.

## Details

The \`radius\` parameter should be a numeric string with a metric unit
separated by a space, such as \`"250 m"\` or \`"10 km"\`. The resulting
\*pas\* will be contain only those sensors that are within \`radius\` of
one of the device-deployments in \`monitor\`.

Three additional columns of metadata will be added to the returned
\*pas\* object:

1.  \`monitor_DDID\` – nearest monitor deviceDeploymentID

2.  \`monitor_AQSID\` – nearest monitor AQSID (if any)

3.  \`monitor_distance\` – distance in meters to the nearest monitor

## See also

\[pas_filter()\]

\[pas_filterArea()\]

\[pas_filterNear()\]

## Examples

``` r
library(AirSensor2)

monitor <-
  AirMonitor::NW_Megafires %>%
  AirMonitor::monitor_filter(countyName == "Okanogan")
pas <-
  example_pas_pm25 %>%
  pas_filter(countyName == "Okanogan")

# Near Omak, WA
pas_near_monitors <-
  pas %>%
  pas_filterNearMonitor(
    monitor,
    radius = "1000 m"
  )

pas_near_monitors %>%
  dplyr::select(sensor_index, locationName, monitor_AQSID, monitor_distance)
#> # A tibble: 3 × 4
#>   sensor_index locationName                       monitor_AQSID monitor_distance
#>   <chr>        <chr>                              <chr>                    <dbl>
#> 1 44939        Twisp                              530470009                  960
#> 2 95189        MV Ambassador @ Little Start Scho… 530470010                  993
#> 3 104610       MV Clean Air Ambassador@Palomino … 530470010                  950
```
