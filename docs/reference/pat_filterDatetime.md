# Datetime filtering for \*pat\* time series objects

Subsets a \*pat\* object by datetime. This function allows for sub-day
filtering as opposed to \`pat_filterDate()\` which always filters to
day-boundaries.

Datetimes can be anything that is understood by
\`MazamaCoreUtils::parseDatetime()\`. For non-\`POSIXct\` values, the
recommended format is \`"YYYY-mm-dd HH:MM:SS"\`.

Timezone determination precedence assumes that if you are passing in
\`POSIXct\` values then you know what you are doing.

1.  get timezone from \`startdate\` if it is \`POSIXct\`

2.  use passed in \`timezone\`

3.  get timezone from \`pat\`

## Usage

``` r
pat_filterDatetime(
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

## See also

\[pat_filter()\]

\[pat_filterDate()\]
