#' @export
#'
#' @title Color palettes for PurpleAir
#'
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param parameter Value to generate colors for, e.g. \code{pm2.5_1hour}.
#' @param paletteName Optional name of an RColorBrewer paeltte, \emph{e.g.}
#' \code{"BuPu"} or \code{"Greens"} to use when a default paleltte is unavailable.
#' @param ... Additional arguments passed on to \code{leaflet::color~} functions.
#'
#' @description Generates color palettes for PurpleAir synoptic data with the
#' intention of having a reproducible functional color generator. Default palettes
#' are available for the following parameters:
#' \itemize{
#' \item{"pm2.5_~"}
#' \item{"humidity}
#' \item{"temperature}
#' }
#'
#' @return An object that consists of a label and color dataframe, and
#' calculated color values from PurpleAir sensors

pas_palette <- function(
  pas = NULL,
  parameter = "pm2.5_60minutes",
  paletteName = NULL,
  ...
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(pas)

  # Ignore incoming paletteName for default parameters
  if ( stringr::str_detect(parameter, "^pm2\\.5_" ) ) {
    paletteName <- "AQI"
  } else if ( parameter == "humidity") {
    paletteName <- "humidity"
  } else if ( parameter == "temperature" ) {
    paletteName <- "temperature"
  } else {
    paletteName <- "Reds"
  }

  # TODO:  validate palette names

  # ----- Create color/legend info ---------------------------------------------

  if ( paletteName == "humidity" ) { # HUMIDITY

    colorFunc <-
      leaflet::colorNumeric(
        "BrBG",
        domain = c(0,100),
        na.color = "#bbbbbb",
        ...
      )

    breaks <- seq(0,100,length.out = 11)
    levels <- seq(5,95,length.out = 10)

    colorBreaks <-
      leaflet::colorBin(
        "BrBG",
        domain = range(breaks),
        bins = breaks,
        ...)(levels)

    labels <-
      c(
        '<10%',
        '10-20%',
        '20-30%',
        '30-40%',
        '40-50%',
        '50-60%',
        '60-70%',
        '70-80%',
        '80-90%',
        '>90%'
      )

    sensorColor <- colorFunc(pas$humidity)

  } else if ( paletteName == "temperature" ) { # TEMPERATURE

    colorFunc <-
      leaflet::colorNumeric(
        "RdYlBu",
        domain = c(-50,130),
        na.color = "#bbbbbb",
        ...
      )

    breaks <- seq(-20,120,length.out = 15)
    levels <- seq(-15,115,length.out = 14)

    colorBreaks <-
      leaflet::colorBin(
        "RdYlBu",
        domain = range(breaks),
        bins = breaks,
        ...)(levels)

    labels <-
      c(
        '<-10',
        '-10-0',
        '0-10',
        '10-20',
        '10-20',
        '20-30',
        '30-40',
        '40-50',
        '50-60',
        '70-80',
        '80-90',
        '90-100',
        '100-110',
        '>110'
      )

    sensorColor <- colorFunc(round(pas$temperature))

  } else if ( paletteName == "AQI" ) { # AQI COLORS

    colorFunc <-
      leaflet::colorBin(
        US_AQI$colors_EPA,
        bins = US_AQI$breaks_PM2.5,
        na.color = "#bbbbbb"
      )

    colorBreaks <- US_AQI$colors_EPA

    labels <- US_AQI$names_eng

    sensorColor <- colorFunc(pas[[parameter]])

  } else { # GENERIC COLOR FUNC

    # TODO:  Figure out good default behavior for non-standarad parameters

    colorFunc <-
      leaflet::colorNumeric(
        palette = paletteName,
        domain = c(0,200),
        na.color = "#bbbbbb",
        ...
      )

    breaks <- seq(0,200,length.out = 7)
    levels <- seq(5,195,length.out = 6)

    colorBreaks <-
      leaflet::colorBin(
        palette = paletteName,
        domain = range(breaks),
        bins = breaks,
        ...)(levels)

    labels <- rep("", 6)

    sensorColor <- colorFunc(pas[[parameter]])

  }

  # ----- Return ---------------------------------------------------------------

  colorInfo <- list(
    key = cbind(labels, colorBreaks),
    colors = sensorColor
  )

  return(colorInfo)

}


#' @export
#'
#' @title Color palettes for Synoptic data
#'
#' @param syn PurpleAir Synoptic \emph{syn} object.
#' @param parameter Value to generate colors for, e.g. \code{pm2.5}.
#' @param paletteName Optional name of an RColorBrewer paeltte, \emph{e.g.}
#' \code{"BuPu"} or \code{"Greens"} to use when a default paleltte is unavailable.
#' @param ... Additional arguments passed on to \code{leaflet::color~} functions.
#'
#' @description Generates color palettes for PurpleAir synoptic data with the
#' intention of having a reproducible functional color generator. Default palettes
#' are available for the following parameters:
#' \itemize{
#' \item{"pm2.5"}
#' \item{"humidity}
#' \item{"temperature}
#' }
#'
#' @return An object that consists of a label and color dataframe, and
#' calculated color values.

syn_palette <- function(
    syn = NULL,
    parameter = "pm2.5",
    paletteName = NULL,
    ...
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(syn)

  # Ignore incoming paletteName for default parameters
  if ( stringr::str_detect(parameter, "^pm2\\.5" ) ) {
    paletteName <- "AQI"
  } else if ( parameter == "humidity") {
    paletteName <- "humidity"
  } else if ( parameter == "temperature" ) {
    paletteName <- "temperature"
  } else {
    paletteName <- "Reds"
  }

  # TODO:  validate palette names

  # ----- Create color/legend info ---------------------------------------------

  if ( paletteName == "humidity" ) { # HUMIDITY

    colorFunc <-
      leaflet::colorNumeric(
        "BrBG",
        domain = c(0,100),
        na.color = "#bbbbbb",
        ...
      )

    breaks <- seq(0,100,length.out = 11)
    levels <- seq(5,95,length.out = 10)

    colorBreaks <-
      leaflet::colorBin(
        "BrBG",
        domain = range(breaks),
        bins = breaks,
        ...)(levels)

    labels <-
      c(
        '<10%',
        '10-20%',
        '20-30%',
        '30-40%',
        '40-50%',
        '50-60%',
        '60-70%',
        '70-80%',
        '80-90%',
        '>90%'
      )

    sensorColor <- colorFunc(syn$humidity)

  } else if ( paletteName == "temperature" ) { # TEMPERATURE

    colorFunc <-
      leaflet::colorNumeric(
        "RdYlBu",
        domain = c(-50,130),
        na.color = "#bbbbbb",
        ...
      )

    breaks <- seq(-20,120,length.out = 15)
    levels <- seq(-15,115,length.out = 14)

    colorBreaks <-
      leaflet::colorBin(
        "RdYlBu",
        domain = range(breaks),
        bins = breaks,
        ...)(levels)

    labels <-
      c(
        '<-10',
        '-10-0',
        '0-10',
        '10-20',
        '10-20',
        '20-30',
        '30-40',
        '40-50',
        '50-60',
        '70-80',
        '80-90',
        '90-100',
        '100-110',
        '>110'
      )

    sensorColor <- colorFunc(round(syn$temperature))

  } else if ( paletteName == "AQI" ) { # AQI COLORS

    colorFunc <-
      leaflet::colorBin(
        US_AQI$colors_EPA,
        bins = US_AQI$breaks_PM2.5,
        na.color = "#bbbbbb"
      )

    colorBreaks <- US_AQI$colors_EPA

    labels <- US_AQI$names_eng

    sensorColor <- colorFunc(syn[[parameter]])

  } else { # GENERIC COLOR FUNC

    # TODO:  Figure out good default behavior for non-standarad parameters

    colorFunc <-
      leaflet::colorNumeric(
        palette = paletteName,
        domain = c(0,200),
        na.color = "#bbbbbb",
        ...
      )

    breaks <- seq(0,200,length.out = 7)
    levels <- seq(5,195,length.out = 6)

    colorBreaks <-
      leaflet::colorBin(
        palette = paletteName,
        domain = range(breaks),
        bins = breaks,
        ...)(levels)

    labels <- rep("", 6)

    sensorColor <- colorFunc(syn[[parameter]])

  }

  # ----- Return ---------------------------------------------------------------

  colorInfo <- list(
    key = cbind(labels, colorBreaks),
    colors = sensorColor
  )

  return(colorInfo)

}

