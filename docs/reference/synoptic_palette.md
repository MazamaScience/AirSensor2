# Color palettes for Synoptic data

Generates color palettes for PurpleAir synoptic data with the intention
of having a reproducible functional color generator. Default palettes
are available for the following parameters:

- "pm2.5"

- "humidity

- "temperature

## Usage

``` r
synoptic_palette(synoptic = NULL, parameter = "pm2.5", paletteName = NULL, ...)
```

## Arguments

- synoptic:

  PurpleAir Synoptic \*synoptic\* object.

- parameter:

  Value to generate colors for, e.g. \`pm2.5\`.

- paletteName:

  Optional name of an RColorBrewer paeltte, \*e.g.\* \`"BuPu"\` or
  \`"Greens"\` to use when a default palette is unavailable.

- ...:

  Additional arguments passed on to \`leaflet::color~\` functions.

## Value

A list containing: 1) a label and color dataframe; and 2) a vector of
color values calculated from the data found in \`synoptic\`.
