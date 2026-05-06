# Check \*pat\* object for validity

Checks on the validity of an \*pat\* object. If any test fails, this
function will stop with a warning message.

## Usage

``` r
pat_check(pat)
```

## Arguments

- pat:

  \*pat\* object.

## Value

Returns \`TRUE\` invisibly if the \*pat\* object is valid.

## See also

\[pat_isValid()\]

## Examples

``` r
library(AirSensor2)

pat_check(example_pat)

# This would throw an error
if ( FALSE ) {

  broken_pat <- example_pat
  names(broken_pat) <- c('meta', 'bop')
  pat_check(broken_pat)

}
```
