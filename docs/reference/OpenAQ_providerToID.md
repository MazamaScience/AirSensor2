# Convert provider names to OpenAQ provider IDs

Accepts a character vector of provider names and returns the
corresponding OpenAQ provider IDs. Matching is performed against both
the \`name\` and \`export_prefix\` fields.

## Usage

``` r
OpenAQ_providerToID(x)
```

## Arguments

- x:

  Character vector of provider names or export prefixes.

## Value

Integer vector of OpenAQ provider IDs.

## Details

Matching is case-insensitive. If no match is found, \`NA\` is returned.
If both \`name\` and \`export_prefix\` match, \`name\` is preferred.

## Examples

``` r
if (FALSE) { # \dontrun{
OpenAQ_providerToID(c("US EPA", "airnow", "Unknown"))
} # }
```
