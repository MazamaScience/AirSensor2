# Apply AirGradient PM2.5 correction

Applies the AirGradient PM2.5 correction based on the EPA correction
algorithm for the Plantower PM sensor.

## Usage

``` r
AirGradient_applyCorrection(data = NULL)
```

## Arguments

- data:

  Data frame containing AirGradient measurements.

## Value

The input data frame with an added \`pm25_corrected\` column.

## Details

The input \`data\` must include:

\* \`datetime\` \* \`pm25\` \* \`relativehumidity\`

A new \`pm25_corrected\` column is added. Negative corrected values are
set to zero.
