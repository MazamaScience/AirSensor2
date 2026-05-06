# Example PurpleAir timeseries dataset with EPA fields

A list containing two tibbles named \`meta\` and \`data\`.

## Usage

``` r
example_pat_epa
```

## Format

A list containing two tibbles named 'meta' and 'data'.

## Source

https://www2.purpleair.com

## Details

The \`example_pat_epa\` dataset provides a quickly loadable version of a
\*pat\* object for practice and code examples. This \*pat\* object
contains the \`pm2.5_cf\` fields required for the EPA correction
algorithm used in \[pat_applyCorrection()\].

This dataset contains 24 hours of data for a single PurpleAir sensor and
was generated on 2024-10-30 by running:


    library(AirSensor2)

    source("global_vars.R") # contains PurpleAir_API_READ_KEY

    example_pat_epa <-
      pat_create(
        api_key = PurpleAir_API_READ_KEY,
        pas = example_pas_metadata,
        sensor_index = "95189",
        startdate = "2024-03-01",
        enddate = "2024-03-03",
        timezone = "America/Los_Angeles",
        average = 0,
        fields = PurpleAir_PAT_EPA_HOURLY_FIELDS
      )

    save(example_pat_epa, file = "data/example_pat_epa.rda")

## See also

example_pas_metadata
