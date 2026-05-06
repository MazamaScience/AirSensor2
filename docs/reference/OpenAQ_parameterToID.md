# Convert parameter names to OpenAQ parameter IDs

Accepts a character vector of parameter names and returns the
corresponding OpenAQ parameter IDs. Matching is performed against both
the \`name\` and \`displayName\` fields.

## Usage

``` r
OpenAQ_parameterToID(x)
```

## Arguments

- x:

  Character vector of parameter names.

## Value

Integer vector of OpenAQ parameter IDs.

## Details

Matching is case-insensitive. If no match is found, \`NA\` is returned.
If both \`name\` and \`displayName\` match, \`name\` is preferred.

## Examples

``` r
if (FALSE) { # \dontrun{
OpenAQ_parameterToID(c("pm25", "PM2.5", "Ozone", "Unknown"))
} # }
```
