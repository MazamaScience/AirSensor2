# Convert instrument names to OpenAQ instrument IDs

Accepts a character vector of instrument names and returns the
corresponding OpenAQ instrument IDs.

## Usage

``` r
OpenAQ_instrumentToID(x)
```

## Arguments

- x:

  Character vector of instrument names.

## Value

Integer vector of OpenAQ instrument IDs.

## Details

Matching is case-insensitive. If no match is found, \`NA\` is returned.

## Examples

``` r
if (FALSE) { # \dontrun{
OpenAQ_instrumentToID(c("BAM 1020", "PurpleAir PA-II", "Unknown"))
} # }
```
