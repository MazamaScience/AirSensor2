# Enhance synoptic data from PurpleAir

Enhance raw synoptic data from PurpleAir to create an improved dataframe
compatible with the MazamaLocationUtils package.

Steps include:

1\) Replace variable names with more consistent, human readable names.

2\) Add spatial metadata for each sensor including:

- timezone – Olson timezone

- countryCode – ISO 3166-1 alpha-2

- stateCode – ISO 3166-2 alpha-2

3\) Convert data types from character to \`POSIXct\` and \`numeric\`.

4\) Add additional metadata items:

- sensorManufacturer = "PurpleAir"

Limiting spatial searches by country can greatly speed up the process of
enhancement. This is performed by providing a vector of ISO country
codes to the \`countryCodes\` argument. By default, no subsetting is
performed.

Users may further improve performance by also specifying \`stateCodes\`
when \`countryCodes\` is limited to a single country.

When a single US state is specified, named \`counties\` may be specified
to further speed up performance.

## Usage

``` r
pas_enhanceRawData(
  pas_raw = NULL,
  countryCodes = "US",
  stateCodes = NULL,
  counties = NULL
)
```

## Arguments

- pas_raw:

  Dataframe returned by \[pas_downloadParseRawData()\].

- countryCodes:

  ISO 3166-1 alpha-2 country codes used to subset the data.

- stateCodes:

  ISO-3166-2 alpha-2 state codes used to subset the data.

- counties:

  US county names or 5-digit FIPS codes used to subset the data.

## Value

Enhanced dataframe of synoptic PurpleAir data.

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
