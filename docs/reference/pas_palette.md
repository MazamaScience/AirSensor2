# Color palettes for PurpleAir

Generates color palettes for PurpleAir synoptic data with the intention
of having a reproducible functional color generator. Default palettes
are available for the following parameters:

- "pm2.5\_~"

- "humidity

- "temperature

## Usage

``` r
pas_palette(pas = NULL, parameter = "pm2.5_60minute", paletteName = NULL, ...)
```

## Arguments

- pas:

  PurpleAir Synoptic \*pas\* object.

- parameter:

  Value to generate colors for, e.g. \`pm2.5_60minute\`.

- paletteName:

  Optional name of an RColorBrewer palette, \*e.g.\* \`"BuPu"\` or
  \`"Greens"\` to use when a default palette is unavailable.

- ...:

  Additional arguments passed on to \`leaflet::color~\` functions.

## Value

A list containing: 1) a label and color dataframe; and 2) a vector of
color values calculated from the data found in \`pas\`.

## Examples

``` r
library(AirSensor2)

# Smoke in Oregon
Oregon <-
  example_pas_pm25 %>%
  pas_filter(stateCode == "OR")

colorInfo <- pas_palette(Oregon, "pm2.5_60minute")

plot(
  x = Oregon$longitude,
  y = Oregon$latitude,
  pch = 0,
  col = colorInfo$colors
)
```
