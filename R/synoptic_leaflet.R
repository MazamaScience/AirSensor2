#' @export
#'
#' @title Leaflet interactive map sensor metadata
#'
#' @param synoptic Synoptic metadata \emph{~_synoptic} object.
#' @param parameter Value to plot, e.g. \code{pm25}.
#' @param paletteName \pkg{RColorBrewer} palette name to use when \code{parameter}
#' is something other than:
#' \itemize{
#' \item{"pm2.5"}
#' \item{"humidity}
#' \item{"temperature}
#' }
#' @param radius Radius (pixels) of monitor circles.
#' @param opacity Opacity of monitor circles.
#' @param maptype Optional name of leaflet ProviderTiles to use, e.g. \code{terrain}.
#' @param extraVars Character vector of additional column names to be shown in
#' leaflet popups.
#'
#' @description This function creates interactive maps that will be displayed in
#' RStudio's 'Viewer' tab.
#'
#' Typical usage would be to use the \code{parameter} argument to display pm25
#' values:
#' \itemize{
#' \item{"pm2.5"}
#' }
#'
#' @details The \code{maptype} argument is mapped onto leaflet "ProviderTile"
#' names. Current mappings include:
#' \enumerate{
#' \item{"roadmap"}{ -- "OpenStreetMap"}
#' \item{"satellite"}{ -- "Esri.WorldImagery"}
#' \item{"terrain"}{ -- "Esri.WorldTopoMap"}
#' \item{"toner"}{ -- "Stamen.Toner"}
#' }
#'
#' If a character string not listed above is provided, it will be used as the
#' underlying map tile if available. See
#' \url{https://leaflet-extras.github.io/leaflet-providers/} for a list of
#' "provider tiles" to use as the background map.
#'
#' @note The \code{paletteName} parameter can take the name of an RColorBrewer
#' paeltte, \emph{e.g.} \code{"BuPu"} or \code{"Greens"}.
#'
#' @return A leaflet "plot" object which, if not assigned, is rendered in
#' Rstudio's 'Viewer' tab.
#'

synoptic_leaflet <- function(
  synoptic = NULL,
  parameter = "pm2.5",
  paletteName = NULL,
  radius = 10,
  opacity = 0.8,
  maptype = "terrain",
  extraVars = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(synoptic)

  if ( !"synoptic" %in% class(synoptic) )
    stop("parameter 'synoptic' is not a valid 'synoptic' object.")

  if ( nrow(synoptic) == 0 )
    stop("parameter 'synoptic' has no data")

  if ( !parameter %in% names(synoptic) )
    stop(sprintf("parameter = '%s' is not found in the 'synoptic' object", parameter))

  if ( !is.numeric(radius) )
    stop("parameter 'radius' must be numeric")

  if ( !is.null(extraVars) ) {
    unrecognizedVars <- setdiff(extraVars, names(synoptic))
    if ( length(unrecognizedVars) > 0 ) {
      stop("variables in 'extraVars' not found in 'synoptic'")
    }
  }

  # ----- Choose colors and title ----------------------------------------------

  # Ignore warnings from RColorBrewer as leaflet::colorBin() does the right thing
  suppressWarnings({

    if ( stringr::str_detect(tolower(parameter), "^pm2\\.5") ) { # AQI

      colorInfo <- synoptic_palette(synoptic, parameter)

      cols <- colorInfo$colors
      labels <- colorInfo$key[,1]
      colors <- colorInfo$key[,2]

      legendTitle <- 'AQI'
      value <- round(synoptic[[parameter]], 1)
      unit <- '\U00B5g/m3'

    } else if ( parameter == "temperature" ) {       # Temperature

      colorInfo <- synoptic_palette(synoptic, "temperature", reverse = TRUE)

      cols <- colorInfo$colors
      labels <- colorInfo$key[,1]
      colors <- colorInfo$key[,2]

      legendTitle <- 'Temp in \U2109'
      value <- round(synoptic[[parameter]], 0)
      unit <- '\U2109'

    } else if ( parameter == "humidity" ) {          # Humidity

      colorInfo <- synoptic_palette(synoptic, "humidity", reverse = FALSE)

      cols <- colorInfo$colors
      labels <- colorInfo$key[,1]
      colors <- colorInfo$key[,2]

      value <- round(synoptic[[parameter]], 0)
      legendTitle <- 'Relative Humidity'
      unit <- '%'

    } else {

      # All other parameters
      domain <- range(synoptic[[parameter]], na.rm = TRUE)
      colorFunc <-
        leaflet::colorNumeric(
          "Purples",
          domain = domain,
          na.color = "#bbbbbb",
          reverse = FALSE
        )
      cols <- colorFunc(synoptic[[parameter]])
      breaks <- seq(domain[1], domain[2], length.out = 6)
      offset <- diff(breaks)[1] / 2
      levels <- signif(seq(breaks[1] + offset, breaks[6] - offset, length.out = 5), digits = 4)
      colors <- leaflet::colorBin("Purples", domain = range(breaks), bins = breaks, reverse = FALSE)(levels)
      labels <- as.character(levels)
      value <- signif(synoptic[[parameter]], 4)
      legendTitle <- parameter
      unit <- ''

    }

  })

  # * Create popupText -----

  popupText <- paste0(
    "<b>", synoptic$locationName, "</b><br/>",
    synoptic$deviceDeploymentID, "<br/>",
    "<br/>",
    "<b>", parameter, " = ", value, " ", unit, "</b>"
  )

  # Add extra vars
  for ( i in seq_along(popupText) ) {

    extraText <- vector("character", length(extraVars))
    for ( j in seq_along(extraVars) ) {
      var <- extraVars[j]
      extraText[j] <- paste0(var, " = ", synoptic[i, var], "<br>")
    }
    extraText <- paste0(extraText, collapse = "")

    popupText[i] <- paste0(popupText[i], "<hr>", extraText)
  }

  synoptic$popupText <- popupText

  # * Extract view information -----

  lonRange <- range(synoptic$longitude, na.rm = TRUE)
  latRange <- range(synoptic$latitude, na.rm = TRUE)
  maxRange <- max(diff(lonRange), diff(latRange), na.rm = TRUE)
  # Determine appropriate zoom level
  if (maxRange > 20) {
    zoom <- 4
  } else if (maxRange > 10) {
    zoom <- 5
  } else if (maxRange > 5) {
    zoom <- 6
  } else if (maxRange > 2) {
    zoom <- 7
  } else if (maxRange > 1) {
    zoom <- 8
  } else if (maxRange > 0.5) {
    zoom <- 9
  } else if (maxRange > 0.2) {
    zoom <- 10
  } else if (maxRange > 0.1) {
    zoom <- 11
  } else {
    zoom <- 12
  }

  # Convert maptype to a character string that addProviderTiles can read
  if ( missing(maptype) || maptype == 'terrain') {
    providerTiles <- "Esri.WorldTopoMap"
  } else if ( maptype == "roadmap" ) {
    providerTiles <- "OpenStreetMap"
  } else if ( maptype == "toner" ) {
    providerTiles <- "Stamen.Toner"
  } else if (maptype == "satellite" ) {
    providerTiles <- "Esri.WorldImagery"
  } else {
    providerTiles <- maptype
  }

  # ----- Create leaflet map ---------------------------------------------------

  m <-
    leaflet::leaflet(dplyr::select(synoptic, c("longitude", "latitude"))) %>%
    leaflet::setView(lng = mean(lonRange), lat = mean(latRange), zoom = zoom) %>%
    leaflet::addProviderTiles(providerTiles) %>%
    leaflet::addCircleMarkers(
      radius = radius,
      fillColor = cols,
      fillOpacity = opacity,
      stroke = FALSE,
      popup = synoptic$popupText,
      layerId = synoptic$locationName
    ) %>%
    leaflet::addLegend(
      position = 'bottomright',
      colors = rev(colors), # show low levels at the bottom
      labels = rev(labels),  # show low levels at the bottom
      opacity = 1,
      title = legendTitle)

  # ----- Return ---------------------------------------------------------------

  return(m)

}
