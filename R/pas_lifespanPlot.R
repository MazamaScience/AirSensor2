#' @export
#' @importFrom rlang .data
#'
#' @title Time range plot showing the lifespan of individual PurpleAir sensors
#'
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param showSensor Logical specifying inclusion of \code{pas$sensor_index} in the plot.
#' @param sensorIdentifier Name of the column to use when identifying a sensor.
#' @param moreSpace Fractional amount which to expand the time axis so as to allow
#' more room for sensorIdentifiers.
#' @param ... Additional arguments to be passed to graphics::plot.default().
#'
#' @description Plots the lifespan of PurpleAir sensors -- the time range
#' between \code{pas$date_created} and \code{pas$last_seen}.  You can use
#' \code{dplyr::filter} and \code{dplyr::arrange()} to pre-process the \code{pas}
#' dataframe to generate informative results
#'
#' When \code{showSensor = TRUE}, typical values for \code{sensorIdentifier} would
#' be either \code{sensorIndex} or \code{locationName}.
#'
#' @return No return value. This function is called to create a plot on the
#' active graphics device.
#'
#' @examples
#' library(AirSensor2)
#'
#' # Plot all lifespans
#' example_pas_historical %>%
#'   pas_lifespanPlot()
#'
#' # Methow Valley Clean Air Ambassador sensors
#' example_pas_historical %>%
#'   pas_filter(stringr::str_detect(locationName, "Ambassador")) %>%
#'   pas_lifespanPlot(
#'     showSensor = TRUE,
#'     sensorIdentifier = "locationName",
#'     cex = .6,
#'     lwd = 2,
#'     moreSpace = .3
#'   )
#'
#' # Arrange by lifespan
#' example_pas_historical %>%
#'   pas_filter(stringr::str_detect(locationName, "Ambassador")) %>%
#'   dplyr::mutate(lifespan = last_seen - date_created) %>%
#'   dplyr::arrange(lifespan) %>%
#'   pas_lifespanPlot(
#'     showSensor = TRUE,
#'     sensorIdentifier = "locationName",
#'     cex = .6,
#'     lwd = 2,
#'     moreSpace = .3
#'   )
#'
#'


pas_lifespanPlot <- function(
    pas,
    showSensor = FALSE,
    sensorIdentifier = "sensor_index",
    moreSpace = 0,
    ...
) {

  # ----- Validate parameters --------------------------------------------------

  # A little involved to catch the case where the user forgets to pass in 'pas'

  result <- try({
    if ( !"PurpleAir_synoptic" %in% class(pas) )
      stop("First argument is not of class 'PurpleAir_synoptic'.")
  }, silent = TRUE)

  if ( class(result) %in% "try-error" ) {
    err_msg <- geterrmessage()
    if ( stringr::str_detect(err_msg, "object .* not found") ) {
      stop(paste0(err_msg, "\n(Did you forget to pass in the 'pas' object?)"))
    } else {
      stop(err_msg)
    }
  }

  if ( !"date_created" %in% names(pas) ) {
    stop("Parameter 'pas' does not have a 'date_created' column as required by pas_lifespanPlot()")
  }

  if ( !"date_created" %in% names(pas) ) {
    stop("Parameter 'pas' does not have a 'date_created' column as required by pas_lifespanPlot()")
  }

  MazamaCoreUtils::stopIfNull(moreSpace)

  # ----- Plot-------------------------------------------------------------

  index <- seq_len(nrow(pas))

  x0 <- pas$date_created
  y0 <- index
  x1 <- pas$last_seen
  y1 <- index

  # plot.default

  argsList <- list(...)

  argsList$x <- c(x0, x1)
  argsList$y <- c(y0, y1)
  argsList$pch <- ifelse("pch" %in% names(argsList), argsList$pch, 16)
  argsList$cex <- ifelse("cex" %in% names(argsList), argsList$cex, 0.5)
  argsList$col  <- ifelse("col" %in% names(argsList), argsList$col, "gray50")
  argsList$xlab  <- ifelse("xlab" %in% names(argsList), argsList$xlab, "")
  argsList$ylab  <- ifelse("ylab" %in% names(argsList), argsList$ylab, "")
  argsList$main  <- ifelse("main" %in% names(argsList), argsList$main, "Sensor Reporting Lifespan")
  argsList$las  <- ifelse("las" %in% names(argsList), argsList$las, 1)

  # X-axis
  if ( !is.null(moreSpace) ) {
    start <- min(pas$date_created)
    end <- max(pas$last_seen)
    days <- difftime(end, start, units = "days") %>% as.numeric()
    # Expand by 2% on either endby default
    start <- start - lubridate::ddays(days/50)
    end <- end + lubridate::ddays(days/50)
    argsList$xlim <- c(
      start - lubridate::ddays(moreSpace * days),
      end
    )
  }

  do.call(plot, argsList)

  # Grid lines
  graphics::abline(
    v = MazamaCoreUtils::parseDatetime(2010:2050, timezone = "UTC"),
    col = "gray80",
    lty = "dotted"
  )

  # segments

  argsList <- list(...)
  argsList$xlab = NULL
  argsList$ylab = NULL
  argsList$main = NULL
  argsList$las = NULL

  argsList$x0 <- x0
  argsList$y0 <- y0
  argsList$x0 <- x0
  argsList$x1 <- x1

  argsList$col  <- ifelse("col" %in% names(argsList), argsList$col, "gray50")
  argsList$lty  <- ifelse("lty" %in% names(argsList), argsList$lty, "solid")
  argsList$lwd  <- ifelse("lwd" %in% names(argsList), argsList$lwd, 1)

  do.call(graphics::segments, argsList)

  # sensor_index

  if ( showSensor ) {

    argsList <- list(...)
    graphics::text(
      x0,
      y0,
      pas[[sensorIdentifier]],
      pos = 2,
      cex = ifelse("cex" %in% names(argsList), argsList$cex, 0.5),
      xpd = ifelse("xpd" %in% names(argsList), argsList$xpd, NA)

    )

  }


  # ----- Return ---------------------------------------------------------------

}
