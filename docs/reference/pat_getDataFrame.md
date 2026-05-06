# Extract dataframes from \*pat\* objects

These functions are convenient wrappers for extracting the dataframes
that comprise a \*pat\* object. These functions are designed to be
useful when manipulating data in a pipeline using \`%\>%\`.

Below is a table showing equivalent operations for each function.

\`pat_getData(pat)\` is equivalent to \`pat\$data\`.

\`pat_getMeta(pat)\` is equivalent to \`pat\$meta\`.

## Usage

``` r
pat_getData(pat)

pat_getMeta(pat)
```

## Arguments

- pat:

  \*pat\* object to extract dataframe from.

## Value

A dataframe from the \*pat\* object.
