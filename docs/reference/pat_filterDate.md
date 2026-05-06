# Date filtering for \*pat\* time series objects

Subsets a \*pat\* object by date. This function always filters to
day-boundaries. For sub-day filtering, use \`pat_filterDatetime()\`.

Dates can be anything that is understood by
\`MazamaCoreUtils::parseDatetime()\` including either of the following
recommended formats:

- \`"YYYYmmdd"\`

- \`"YYYY-mm-dd"\`

Timezone determination precedence assumes that if you are passing in
\`POSIXct\` values then you know what you are doing.

1.  get timezone from \`startdate\` if it is \`POSIXct\`

2.  use passed in \`timezone\`

3.  get timezone from \`pat\`

## Usage

``` r
pat_filterDate(
  pat = NULL,
  startdate = NULL,
  enddate = NULL,
  timezone = NULL,
  unit = "sec",
  ceilingStart = FALSE,
  ceilingEnd = FALSE
)
```

## Arguments

- pat:

  \*pat\* object.

- startdate:

  Desired start datetime (ISO 8601).

- enddate:

  Desired end datetime (ISO 8601).

- timezone:

  Olson timezone used to interpret dates.

- unit:

  Units used to determine time at end-of-day.

- ceilingStart:

  Logical instruction to apply \[lubridate::ceiling_date()\] to the
  \`startdate\` rather than \[lubridate::floor_date()\]

- ceilingEnd:

  Logical instruction to apply \[lubridate::ceiling_date()\] to the
  \`enddate\` rather than \[lubridate::floor_date()\]

## Value

A subset of the incoming \*pat\* time series object. (A list with
\`meta\` and \`data\` dataframes.)

## Note

The returned data will run from the beginning of \`startdate\` until the
\*\*beginning\*\* of \`enddate\` – \*i.e.\* no values associated with
\`enddate\` will be returned. The exception being when \`enddate\` is
less than 24 hours after \`startdate\`. In that case, a single day is
returned.

## See also

\[pat_filter()\]

\[pat_filterDatetime()\]
