# Convert license names to OpenAQ license IDs

Accepts a character vector of license names and returns the
corresponding OpenAQ license IDs.

## Usage

``` r
OpenAQ_licenseToID(x)
```

## Arguments

- x:

  Character vector of license names.

## Value

Integer vector of OpenAQ license IDs.

## Details

Matching is case-insensitive. If no match is found, \`NA\` is returned.

## Examples

``` r
if (FALSE) { # \dontrun{
OpenAQ_licenseToID(c("CC BY 4.0", "Public Domain", "Unknown"))
} # }
```
