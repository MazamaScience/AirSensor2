# Leaflet interactive map sensor metadata

This function creates interactive maps that will be displayed in
RStudio's 'Viewer' tab.

Typical usage would be to use the \`parameter\` argument to display pm25
values:

- "pm2.5"

## Usage

``` r
synoptic_leaflet(
  synoptic = NULL,
  parameter = "pm2.5",
  paletteName = NULL,
  radius = 10,
  opacity = 0.8,
  maptype = "terrain",
  extraVars = NULL
)
```

## Arguments

- synoptic:

  Synoptic metadata \*~\_synoptic\* object.

- parameter:

  Value to plot, e.g. \`pm25\`.

- paletteName:

  RColorBrewer palette name to use when \`parameter\` is something other
  than:

  - "pm2.5"

  - "humidity

  - "temperature

- radius:

  Radius (pixels) of monitor circles.

- opacity:

  Opacity of monitor circles.

- maptype:

  Optional name of leaflet ProviderTiles to use, e.g. \`terrain\`.

- extraVars:

  Character vector of additional column names to be shown in leaflet
  popups.

## Value

A leaflet "plot" object which, if not assigned, is rendered in Rstudio's
'Viewer' tab.

## Details

The \`maptype\` argument is mapped onto leaflet "ProviderTile" names.
Current mappings include:

- "roadmap":

  – "OpenStreetMap"

- "satellite":

  – "Esri.WorldImagery"

- "terrain":

  – "Esri.WorldTopoMap"

- "toner":

  – "Stamen.Toner"

If a character string not listed above is provided, it will be used as
the underlying map tile if available. See
<https://leaflet-extras.github.io/leaflet-providers/> for a list of
"provider tiles" to use as the background map.

## Note

The \`paletteName\` parameter can take the name of an RColorBrewer
paeltte, \*e.g.\* \`"BuPu"\` or \`"Greens"\`.
