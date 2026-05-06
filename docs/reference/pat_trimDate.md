# Trim \*pat\* time series object to full days

Trims the date range of a \*pat\* object to local time date boundaries
which are \*within\* the range of data. This has the effect of removing
partial-day data records at the start and end of the timeseries and is
useful when calculating full-day statistics.

Day boundaries are calculated using the specified \`timezone\` or, if
\`NULL\`, using \`pat\$meta\$timezone\`.

## Usage

``` r
pat_trimDate(pat = NULL, timezone = NULL)
```

## Arguments

- pat:

  \*pat\* object.

- timezone:

  Olson timezone used to interpret dates.

## Value

A subset of the incoming \*pat\* time series object. (A list with
\`meta\` and \`data\` dataframes.)
