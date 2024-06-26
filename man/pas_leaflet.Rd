% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pas_leaflet.R
\name{pas_leaflet}
\alias{pas_leaflet}
\title{Leaflet interactive map of PurpleAir sensors}
\usage{
pas_leaflet(
  pas = NULL,
  parameter = NULL,
  paletteName = NULL,
  radius = 10,
  opacity = 0.8,
  maptype = "terrain"
)
}
\arguments{
\item{pas}{PurpleAir Synoptic \emph{pas} object.}

\item{parameter}{Optional parameter used to color locations, e.g. \code{pm25_1hr}.}

\item{paletteName}{\pkg{RColorBrewer} palette name to use when \code{parameter}
is something other than:
\itemize{
\item{"pm2.5_~"}
\item{"humidity}
\item{"temperature}
}}

\item{radius}{Radius (pixels) of monitor circles.}

\item{opacity}{Opacity of monitor circles.}

\item{maptype}{Optional name of leaflet ProviderTiles to use, e.g. \code{terrain}.}
}
\value{
A leaflet "plot" object which, if not assigned, is rendered in
Rstudio's 'Viewer' tab.
}
\description{
This function creates interactive maps that will be displayed in
RStudio's 'Viewer' tab.

When \code{parameter} is not specified, a simple map is displayed. When the
user clicks on a location, site metadata is displayed.

When \code{parameter} is specified, it is assumed that a \emph{pas} object
has been created with measured parameters for a particular moment in time.

For a \emph{pas} object that includes measurements, typical usage would be to
use the \code{parameter} argument to display pm25 values from one of:
\itemize{
\item{"pm2.5_10minute"}
\item{"pm2.5_30minute"}
\item{"pm2.5_60minute"}
\item{"pm2.5_6hour"}
\item{"pm2.5_24hour"}
\item{"pm2.5_1week"}
}
}
\details{
The \code{maptype} argument is mapped onto leaflet "ProviderTile"
names. Current mappings include:
\describe{
\item{"roadmap"}{ -- "OpenStreetMap"}
\item{"satellite"}{ -- "Esri.WorldImagery"}
\item{"terrain"}{ -- "Esri.WorldTopoMap"}
\item{"toner"}{ -- "Stamen.Toner"}
}

If a character string not listed above is provided, it will be used as the
underlying map tile if available. See
\url{https://leaflet-extras.github.io/leaflet-providers/} for a list of
"provider tiles" to use as the background map.
}
\note{
The \code{paletteName} parameter can take the name of an RColorBrewer
paeltte, \emph{e.g.} \code{"BuPu"} or \code{"Greens"}.
}
\examples{
library(AirSensor2)

if ( interactive() ) {
  pas_leaflet(example_pas, parameter = "pm2.5_60minute")

  pas_leaflet(example_pas, parameter = "temperature")

  pas_leaflet(example_pas, parameter = "humidity")
}
}
