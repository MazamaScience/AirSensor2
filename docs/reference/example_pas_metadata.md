# Example PurpleAir synoptic dataset with extended metadata

A tibble with 122 rows and 31 columns of data.

## Usage

``` r
example_pas_metadata
```

## Format

A tibble with 122 rows and 31 columns of data.

## Source

https://www2.purpleair.com

## Details

The \`example_pas_metadata\` dataset provides a quickly loadable version
of a \*pas\* object for practice and code examples. This object contains
more metadata than other \`example_pas\_~\` objects.

This dataset contains data for sensors in Washington and Oregon and was
generated on 2024-03-12 by running:


    library(AirSensor2)

    initializeMazamaSpatialUtils()

    source("global_vars.R") # contains PurpleAir_API_READ_KEY

    example_pas_metadata <-
      pas_createNew(
        api_key = PurpleAir_API_READ_KEY,
        fields = PurpleAir_PAS_METADATA_FIELDS,
        countryCodes = "US",
        stateCodes = "WA",
        counties = "Okanogan",
        lookbackDays = 0,            # all years
        location_type = 0            # outdoor only
      )

    save(example_pas_metadata, file = "data/example_pas_metadata.rda")

## See also

example_pas_pm25

example_pas_historical
