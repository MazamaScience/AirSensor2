# Test for an empty \*pas\* object

Convenience function for \`nrow(pas) == 0\`. This makes for more
readable code in functions that need to test for this.

## Usage

``` r
pas_isEmpty(pas = NULL)
```

## Arguments

- pas:

  A \*PurpleAir_synoptic\* object.

## Value

\`TRUE\` if no data exist in \`pas\`, \`FALSE\` otherwise.

## Examples

``` r
library(AirSensor2)
example_pas_pm25 %>% pas_isEmpty() %>% print()
#> [1] FALSE
example_pas_pm25 %>% pas_filter(latitude > 90) %>% pas_isEmpty() %>% print()
#> [1] TRUE
```
