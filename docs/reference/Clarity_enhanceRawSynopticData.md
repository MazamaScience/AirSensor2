# Enhance synoptic data from Clarity

Enhance raw synoptic data from Clarity to create an improved dataframe
compatible with the MazamaLocationUtils package.

Steps include:

1\) Replace variable names with more consistent, human readable names.

2\) Add spatial metadata for each sensor including:

- timezone – Olson timezone

- countryCode – ISO 3166-1 alpha-2

- stateCode – ISO 3166-2 alpha-2

3\) Convert data types from character to \`POSIXct\` and \`numeric\`.

4\) Add additional metadata items:

- sensorManufacturer = "Clarity"

## Usage

``` r
Clarity_enhanceRawSynopticData(rawSynoptic = NULL)
```

## Arguments

- rawSynoptic:

  'synoptic' dataframe returned by \`Clarity_getAllOpenHourly()\`.

## Value

Enhanced dataframe of synoptic Clarity data.

## See also

\[Clarity_getAllOpenHourly()\]
