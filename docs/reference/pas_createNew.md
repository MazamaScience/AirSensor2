# Create a new PurpleAir synoptic dataset

Download, parse and enhance synoptic data from PurpleAir and return the
results as a useful tibble with class \`PurpleAir_synoptic\`.

Steps include:

1\) Download and parse synoptic data

2\) Replace variable names with more consistent, more human readable
names.

3\) Add spatial metadata for each sensor including:

- timezone – olson timezone

- countryCode – ISO 3166-1 alpha-2

- stateCode – ISO 3166-2 alpha-2

4\) Convert data types from character to \`POSIXct\` and \`numeric\`.

Data requests are made with a bounding box determined from the required
\`countryCodes\` parameter. If a single country is specified and
additional \`stateCodes\` are specified, the bounding box will be
limited to those states. Withing a single state, \`counties\` may be
used to further limit the sensors requested.

If \`show_only\` is used to request specific sensors, the
\`countryCodes\` and \`stateCodes\` information is ignored when
requesting data. But these are still used to help speed up the
assignment of enhanced metadata..

## Usage

``` r
pas_createNew(
  api_key = NULL,
  fields = PurpleAir_PAS_MINIMAL_FIELDS,
  countryCodes = NULL,
  stateCodes = NULL,
  counties = NULL,
  lookbackDays = 1,
  location_type = 0,
  read_keys = NULL,
  show_only = NULL,
  baseUrl = "https://api.purpleair.com/v1/sensors"
)
```

## Arguments

- api_key:

  PurpleAir API Read Key.

- fields:

  Character string with PurpleAir field names for the Get Sensor Data
  API.

- countryCodes:

  ISO 3166-1 alpha-2 country codes used to subset the data. At least one
  countryCode must be specified.

- stateCodes:

  ISO-3166-2 alpha-2 state codes used to subset the data. Specifying
  stateCodes is optional.

- counties:

  US county names or 5-digit FIPS codes used to subset the data.
  Specifying counties is optional.

- lookbackDays:

  Number of days to "look back" for valid data. Data are filtered to
  only include sensors with data more recent than \`lookbackDays\` ago.
  Use \`lookbackDays = 0\` to get all historical sensors.

- location_type:

  The \`location_type\` of the sensors. Possible values are: 0 =
  Outside, 1 = Inside or \`NULL\` = both.

- read_keys:

  Optional, comma separated list of sensor read_keys is required for
  private devices. It is separate from the api_key and each sensor has
  its own read_key. Submit multiple keys by separating them with a comma
  (,) character for example: \`"key-one,key-two,key-three"\`.

- show_only:

  Optional, comma separated list of sensor_index values. When provided,
  results are requested only for the specified sensors.

- baseUrl:

  Base URL for the PurpleAir API.

## Value

A PurpleAir Synoptic \*pas\* object.

## Note

The \`fields\` parameter allows users to dial in which fields they are
interested in depending on their needs. However, the following fields
will be added if not specified in order to guarantee compatibility with
\`pas_enhanceRawData()\`:
\`"longitude,latitude,name,location_type,date_created,last_seen"\`.

Pregenerated fields for use in this function include:

- \[PurpleAir_PAS_MINIMAL_FIELDS\] – minimal set of fields

- \[PurpleAir_PAS_METADATA_FIELDS\] – instrument-only fields

- \[PurpleAir_PAS_AVG_PM25_FIELDS\] – includes measurements

## References

\[PurpleAir\](https://www2.purpleair.com)

\[PurpleAir API\](https://api.purpleair.com/)

\[PurpleAir Terms of
service\](https://www2.purpleair.com/policies/terms-of-service)

\[PurpleAir Data license\](https://www2.purpleair.com/pages/license)

\[PurpleAir Data
Attribution\](https://www2.purpleair.com/pages/attribution)

## See also

\[pas_downloadParseRawData()\]

\[pas_enhanceRawData()\]

## Examples

``` r
# \donttest{
# Fail gracefully if any resources are not available
try({

library(AirSensor2)

initializeMazamaSpatialUtils()

source("global_vars.R") # contains PurpleAir_API_READ_KEY

pas <-
  pas_createNew(
    api_key = PurpleAir_API_READ_KEY,
    countryCodes = "US",
    stateCodes = "WA",
    counties = c("Okanogan"),
    lookbackDays = 0, # all historical sensors
    location_type = 0
  )

}, silent = FALSE)
#> Warning: cannot open file 'global_vars.R': No such file or directory
#> Error in file(filename, "r", encoding = encoding) : 
#>   cannot open the connection
# }
```
