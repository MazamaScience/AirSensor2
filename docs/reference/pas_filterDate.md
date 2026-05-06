# Date filtering for PurpleAir Synoptic objects

Subsets a \*pas\* object by date.

Dates can be anything that is understood by
\`MazamaCoreUtils::parseDatetime()\` including either of the following
recommended formats:

- \`"YYYYmmdd"\`

- \`"YYYY-mm-dd"\`

Timezone determination precedence assumes that if you are passing in
\`POSIXct\` values then you know what you are doing.

1.  get timezone from \`startdate\` if it is \`POSIXct\`

2.  use passed in \`timezone\`

## Usage

``` r
pas_filterDate(pas = NULL, startdate = NULL, enddate = NULL, timezone = NULL)
```

## Arguments

- pas:

  PurpleAir Synoptic \*pas\* object.

- startdate:

  Desired start datetime (ISO 8601).

- enddate:

  Desired end datetime (ISO 8601).

- timezone:

  Optional Olson timezone used to interpret dates.

## Note

The incoming \`pas\` object must contain the \`date_created\` and
\`last_modified\` fields. The returned \*pas\* object will retain sites
where \`startdate \<= last_seen && enddate \>= pas\$date_created\`.

## See also

\[pas_filter()\]

## Examples

``` r
library(AirSensor2)

august_2018 <-
 example_pas_historical %>%
  pas_filterDate(
    startdate = 20180701,
    enddate = 20181101,
    timezone = "America/Los_Angeles"
 )

fields <- c("sensor_index", "name", "date_created", "last_seen")
head(august_2018[,fields])
#> # A tibble: 6 × 4
#>   sensor_index name                      date_created        last_seen          
#>   <chr>        <chr>                     <dttm>              <dttm>             
#> 1 6416         "Mid-Valley Clinic"       2018-01-25 20:08:36 2019-01-19 08:17:23
#> 2 6418         "Okanogan Conservation D… 2018-01-25 20:08:48 2020-11-11 17:41:57
#> 3 6424         "Okanogan Airport"        2018-01-25 20:09:18 2023-06-09 02:59:51
#> 4 6430         "Omak Waste Treatment"    2018-01-25 20:10:12 2021-02-18 21:13:50
#> 5 6432         "Mid Valley Hospital "    2018-01-25 20:10:33 2024-03-02 05:46:31
#> 6 6436         "Hopfer Road"             2018-01-25 20:10:50 2024-03-12 19:45:51
```
