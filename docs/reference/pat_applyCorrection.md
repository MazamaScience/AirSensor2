# Apply correction to PurpleAir PM2.5 data

A correction equation is applied to fields of the incoming \*pat\*
object to generate a \`pm2.5_corrected\` time series which is added to
the returned \*pat\* object.

## Usage

``` r
pat_applyCorrection(pat = NULL)
```

## Arguments

- pat:

  Previously generated \*hourly pat\* object.

## Value

A PurpleAir Timeseries \*pat\* object with an additional
\`pm2.5_corrected\` variable.
