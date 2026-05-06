# Leaflet interactive map of PurpleAir sensors

This function creates interactive maps that will be displayed in
RStudio's 'Viewer' tab.

When \`parameter\` is not specified, a simple map is displayed. When the
user clicks on a location, site metadata is displayed.

When \`parameter\` is specified, it is assumed that a \*pas\* object has
been created with measured parameters for a particular moment in time.

For a \*pas\* object that includes measurements, typical usage would be
to use the \`parameter\` argument to display pm25 values from one of:

- "pm2.5_10minute"

- "pm2.5_30minute"

- "pm2.5_60minute"

- "pm2.5_6hour"

- "pm2.5_24hour"

- "pm2.5_1week"

## Usage

``` r
pas_leaflet(
  pas = NULL,
  parameter = NULL,
  paletteName = NULL,
  radius = 10,
  opacity = 0.8,
  maptype = "terrain"
)
```

## Arguments

- pas:

  PurpleAir Synoptic \*pas\* object.

- parameter:

  Optional parameter used to color locations, e.g. \`pm25_1hr\`.

- paletteName:

  RColorBrewer palette name to use when \`parameter\` is something other
  than:

  - "pm2.5\_~"

  - "humidity

  - "temperature

- radius:

  Radius (pixels) of monitor circles.

- opacity:

  Opacity of monitor circles.

- maptype:

  Optional name of leaflet ProviderTiles to use, e.g. \`terrain\`.

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

## Examples

``` r
library(AirSensor2)

if ( interactive() ) {
  pas_leaflet(example_pas_pm25, parameter = "pm2.5_60minute")

  pas_leaflet(example_pas_pm25, parameter = "temperature")

  pas_leaflet(example_pas_pm25, parameter = "humidity")
}
```
