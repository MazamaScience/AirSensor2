# Convert a PurpleAir Timeseries to a 'mts_monitor' object

Enhance an hourly PurpleAir Timeseries \*pat\* object created with
\`pat_createHourly()\` and create an object of class \`mts_monitor\` for
use with the \[AirMonitor\](https://mazamascience.github.io/AirMonitor/)
package.

## Usage

``` r
pat_toMonitor(pat = NULL, applyCorrection = TRUE)
```

## Arguments

- pat:

  Previously generated \*pat\* object.

- applyCorrection:

  Logical specifying whether to apply the EPA correction algorithm. (See
  \[pat_applyCorrection()\].)

## Value

An AirMonitor package \*mts_monitor\* object.

## References

\[PurpleAir\](https://www2.purpleair.com)

\[PurpleAir API\](https://api.purpleair.com)

\[PurpleAir Terms of
service\](https://www2.purpleair.com/policies/terms-of-service)

\[PurpleAir Data license\](https://www2.purpleair.com/pages/license)

\[PurpleAir Data
Attribution\](https://www2.purpleair.com/pages/attribution)

## Examples

``` r
# \donttest{
# Fail gracefully if any resources are not available
try({

# AirSensor2 package
library(AirSensor2)
monitor <-
  example_pat_epa %>%
  pat_toMonitor()

AirMonitor::monitor_timeseriesPlot(monitor, shadedNight = TRUE)

}, silent = FALSE)
#> Error in pat_toMonitor(.) : 
#>   Required fields missing from 'pat' which must include all of "humidity,temperature,pm2.5_atm,pm2.5_atm_a,pm2.5_atm_b"
# }
```
