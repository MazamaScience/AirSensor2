# Comma-separated list of fields used to create a \*pas\* object

Character string with PurpleAir field names used in
\`pas_downloadParseRawData()\`. These fields include most of the
"information and status" fields, "humidity", "temperature", "pressure"
and simple running average PM2.5 fields for different time periods.

These fields are useful for creating maps of the latest quality and
weather parameters.

Included fields:


     [1] "name"           "location_type"  "private"        "latitude"
     [5] "longitude"      "last_seen"      "date_created"   "confidence"
     [9] "humidity"       "temperature"    "pressure"       "pm2.5_10minute"
    [13] "pm2.5_30minute" "pm2.5_60minute" "pm2.5_6hour"    "pm2.5_24hour"
    [17] "pm2.5_1week"

## Usage

``` r
PurpleAir_PAS_AVG_PM25_FIELDS
```

## Format

String with comma-separated field names

## References

\[Get Sensors Data
API\](https://api.purpleair.com/#api-sensors-get-sensors-data)
