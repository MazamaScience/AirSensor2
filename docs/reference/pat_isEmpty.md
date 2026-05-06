# Test for empty \*pat\* object

Convenience function for \`nrow(pat\$data) == 0\`. This makes for more
readable code in functions that need to test for this.

## Usage

``` r
pat_isEmpty(pat)
```

## Arguments

- pat:

  \*pat\* object

## Value

\`TRUE\` if no data exist in \`pat\`, \`FALSE\` otherwise.

## Examples

``` r
library(AirSensor2)

pat_isEmpty(example_pat)
#> [1] FALSE
```
