# Example historical PurpleAir synoptic dataset

A tibble with 122 rows and 22 columns of data.

## Usage

``` r
example_pas_historical
```

## Format

A tibble with 122 rows and 22 columns of data.

## Source

https://www2.purpleair.com

## Details

The \`example_pas_historical\` dataset provides a quickly loadable
version of a \*pas\* object for practice and code examples. This object
contains fewer metadata fields than other \`example_pas\_~\` objects and
is intended for use with \[pas_lifespanPlot()\].

This dataset contains data for sensors in Washington and Oregon and was
generated on 2024-03-12 by running:


    library(AirSensor2)

    initializeMazamaSpatialUtils()

    source("global_vars.R") # contains PurpleAir_API_READ_KEY

    example_pas_historical <-
      pas_createNew(
        api_key = PurpleAir_API_READ_KEY,
        fields = PurpleAir_PAS_MINIMAL_FIELDS,
        countryCodes = "US",
        stateCodes = "WA",
        counties = "Okanogan",
        lookbackDays = 0,            # all years
        location_type = 0            # outdoor only
      )

    save(example_pas_historical, file = "data/example_pas_historical.rda")

## See also

example_pas_pm25

example_pas_metadata
