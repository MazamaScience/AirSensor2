# Convert country codes or names to OpenAQ country IDs

Accepts a character vector of ISO country codes (e.g. \`"US"\`) or
country names (e.g. \`"United States"\`) and returns the corresponding
OpenAQ country IDs.

## Usage

``` r
OpenAQ_countryToID(x)
```

## Arguments

- x:

  Character vector of country codes or names.

## Value

Integer vector of OpenAQ country IDs.

## Details

Matching is case-insensitive. If no match is found, \`NA\` is returned.

## Examples

``` r
if (FALSE) { # \dontrun{
OpenAQ_countryToID(c("US", "Canada", "France", "Unknown"))
} # }
```
