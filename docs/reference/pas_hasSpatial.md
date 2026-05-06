# Test for spatial metadata in \*pas\* object

Tests for the existence of the following core spatial metadata columns:

- longitude – decimal degrees E

- latitude – decimal degrees N

- timezone – Olson timezone

- countryCode – ISO 3166-1 alpha-2

- stateCode – ISO 3166-2 alpha-2

## Usage

``` r
pas_hasSpatial(pas = NULL)
```

## Arguments

- pas:

  A PurpleAir Synoptic \*pas\* object.

## Value

\`TRUE\` if \`pas\` contains core spatial metadata, \`FALSE\` otherwise.

## Examples

``` r
library(AirSensor2)
example_pas_pm25_raw %>% pas_hasSpatial() %>% print()
#> [1] FALSE
example_pas_pm25 %>% pas_hasSpatial() %>% print()
#> [1] TRUE
```
