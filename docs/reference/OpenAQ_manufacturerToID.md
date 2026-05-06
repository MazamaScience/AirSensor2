# Convert manufacturer names to OpenAQ manufacturer IDs

Accepts a character vector of manufacturer names and returns the
corresponding OpenAQ manufacturer IDs.

## Usage

``` r
OpenAQ_manufacturerToID(x)
```

## Arguments

- x:

  Character vector of manufacturer names.

## Value

Integer vector of OpenAQ manufacturer IDs.

## Details

Matching is case-insensitive. If no match is found, \`NA\` is returned.

## Examples

``` r
if (FALSE) { # \dontrun{
OpenAQ_manufacturerToID(c("Met One Instruments", "Thermo Fisher Scientific", "Unknown"))
} # }
```
