# Example enhanced PurpleAir synoptic PM2.5 dataset

A tibble with 2287 rows and 33 columns of data.

## Usage

``` r
example_pas_pm25
```

## Format

A tibble with 2287 rows and 33 columns of data.

## Source

https://www2.purpleair.com

## Details

The \`example_pas_pm25\` dataset provides a quickly loadable version of
a \*pas\* object for practice and code examples. This object contains
metadata suitable for generating \*pat\* objects with \[pat_create()\].

This dataset contains data for sensors in Washington and Oregon and was
generated on 2024-03-12 by running:


    library(AirSensor2)

    initializeMazamaSpatialUtils()

    source("global_vars.R") # contains PurpleAir_API_READ_KEY

    example_pas_pm25 <-
      pas_createNew(
        api_key = PurpleAir_API_READ_KEY,
        fields = PurpleAir_PAS_AVG_PM25_FIELDS,
        countryCodes = "US",
        stateCodes = c("WA", "OR"),
        counties = NULL,
        lookbackDays = 1,
        location_type = 0
      )

    save(example_pas_pm25, file = "data/example_pas_pm25.rda")

## See also

example_pas_pm25_raw

example_pas_historical

example_pas_metadata
