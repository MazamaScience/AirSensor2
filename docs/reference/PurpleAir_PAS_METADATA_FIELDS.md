# Comma-separated list of metadata fields used to create a \*pas\* object

Character string with PurpleAir field names used in
\`pas_downloadParseRawData()\`. These fields exclude all measurements
but retain many fields needed when creating a \*pat\* object with
\`pat_create()\`.

Included fields:


     [1] "name"             "model"            "hardware"
     [4] "location_type"    "private"          "latitude"
     [7] "longitude"        "altitude"         "position_rating"
    [10] "firmware_version" "firmware_upgrade" "uptime"
    [13] "last_seen"        "last_modified"    "date_created"

## Usage

``` r
PurpleAir_PAS_METADATA_FIELDS
```

## Format

String with comma-separated field names

## References

\[Get Sensors Data
API\](https://api.purpleair.com/#api-sensors-get-sensors-data)
