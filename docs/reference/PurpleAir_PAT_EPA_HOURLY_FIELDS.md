# Comma-separated list of fields needed for EPA correction

Character string with a minimal set of PurpleAir field names used in
\`PurpleAir_createNewMonitor()\`. These fields are sufficient to apply
the EPA correction algorithm used in the AirNow \[Fire & Smoke
Map\](https://fire.airnow.gov).

Included fields:


    [1] "humidity"     "temperature"  "pm2.5_atm"   "pm2.5_atm_a"
    [5] "pm2.5_atm_b"

## Usage

``` r
PurpleAir_PAT_EPA_HOURLY_FIELDS
```

## Format

String with comma-separated field names

## References

\[EPA PurpleAir
Correction\](https://document.airnow.gov/airnow-fire-and-smoke-map-questions-and-answers.pdf).

\[Get Sensor History
API\](https://api.purpleair.com/#api-sensors-get-sensor-history-csv)

## See also

\[pat_applyCorrection()\]
