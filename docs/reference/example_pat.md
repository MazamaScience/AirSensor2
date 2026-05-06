# Example PurpleAir timeseries dataset

A list containing two tibbles named \`meta\` and \`data\`.

## Usage

``` r
example_pat
```

## Format

A list containing two tibbles named 'meta' and 'data'.

## Source

https://www2.purpleair.com

## Details

The \`example_pat\` dataset provides a quickly loadable version of a
\*pat\* object for practice and code examples. This dataset contains 24
hours of data for a single PurpleAir sensor and was generated on
2024-03-12 by running:


    library(AirSensor2)

    source("global_vars.R") # contains PurpleAir_API_READ_KEY

    example_pat <-
      pat_create(
        api_key = PurpleAir_API_READ_KEY,
        pas = example_pas_metadata,
        sensor_index = "95189",
        startdate = "2024-03-01",
        enddate = "2024-03-03",
        timezone = "America/Los_Angeles",
        average = 0,
        fields = PurpleAir_PAT_QC_FIELDS
      )

    save(example_pat, file = "data/example_pat.rda")

## See also

example_pas_metadata
