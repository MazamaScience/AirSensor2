# Comma-separated list of metadata fields used to create a \*pas\* object

Character string with PurpleAir field names used in
\`pas_downloadParseRawData()\`. These fields exclude all measurements
but retains the minimal set of metadata required to create a \*pas\*
object. This \*pas\* object can then be passed on to \*pat\* creation
functions.

A metadata-only \*pas\* object can be useful when searching for
historical data using \[pas_filterDate()\].

Included fields:


    [1] "name"          "location_type" "latitude"
    [5] "longitude"     "last_seen"     "date_created"

## Usage

``` r
PurpleAir_PAS_MINIMAL_FIELDS
```

## Format

String with comma-separated field names

## References

\[Get Sensors Data
API\](https://api.purpleair.com/#api-sensors-get-sensors-data)
