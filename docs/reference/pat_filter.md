# General purpose data filtering for \*pat\* time series objects

A generalized data filter for \*pat\* objects to choose rows/cases where
conditions are true. Multiple conditions are combined with \`&\` or
separated by a comma. Only rows where the condition evaluates to TRUE
are kept. Rows where the condition evaluates to \`NA\` are dropped.

If an empty \*pat\* object is passed in, it is immediately returned,
allowing for multiple filtering steps to be piped together and only
checking for an empty \*pat\* object at the end of the pipeline.

## Usage

``` r
pat_filter(pat, ...)
```

## Arguments

- pat:

  \*pat\* object.

- ...:

  Logical predicates defined in terms of the variables in \`pat\$data\`.

## Value

A subset of the incoming \`pat\` time series object. (A list with
\`meta\` and \`data\` dataframes.)

## Note

Filtering is done on values in \`pat\$data\`.

## See also

\[pat_filterDate()\]

\[pat_filterDatetime()\]

## Examples

``` r

library(AirSensor2)

# Unhealthy values
example_pat %>%
  pat_filter(pm2.5_atm_a > 25, pm2.5_atm_b > 25) %>%
  pat_getData() %>%
  head()
#> # A tibble: 6 × 13
#>   datetime            hardware   firmware_version  rssi uptime pa_latency memory
#>   <dttm>              <chr>                 <dbl> <dbl>  <dbl>      <dbl>  <dbl>
#> 1 2024-03-02 03:16:44 2.0+OPENL…             7.02   -84 2.01e6        850  15744
#> 2 2024-03-02 03:18:42 2.0+OPENL…             7.02   -85 2.01e6       2480  15576
#> 3 2024-03-02 03:20:45 2.0+OPENL…             7.02   -83 2.01e6        517  15744
#> 4 2024-03-02 03:22:43 2.0+OPENL…             7.02   -82 2.01e6       3815  15576
#> 5 2024-03-02 03:24:42 2.0+OPENL…             7.02   -83 2.01e6       1253  15576
#> 6 2024-03-02 03:26:42 2.0+OPENL…             7.02   -82 2.01e6        674  15576
#> # ℹ 6 more variables: humidity <dbl>, temperature <dbl>, pressure <dbl>,
#> #   pm2.5_atm <dbl>, pm2.5_atm_a <dbl>, pm2.5_atm_b <dbl>
```
