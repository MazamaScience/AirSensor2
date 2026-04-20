#' Plot the reporting lifespan of OpenAQ locations
#'
#' Plots the reporting lifespan of OpenAQ locations as the time range between
#' `datetime_first` and `datetime_last`.
#'
#' This function is useful for exploring which locations have long reporting
#' histories and which appear only briefly in the OpenAQ archive. You can use
#' `dplyr::filter()` and `dplyr::arrange()` to pre-process the `locations`
#' data frame before plotting.
#'
#' When `showLocation = TRUE`, typical values for `locationIdentifier` are
#' `"id"` or `"locationID"`.
#'
#' @param locations Data frame returned by [OpenAQ_createLocations()] or a
#'   similarly structured OpenAQ locations data frame.
#' @param showLocation Logical specifying whether to label locations on the plot.
#' @param locationIdentifier Name of the column to use when identifying a
#'   location. Typical values are `"id"` or `"locationID"`.
#' @param moreSpace Fractional amount by which to expand the time axis to allow
#'   more room for location labels.
#' @param ... Additional arguments passed to [graphics::plot.default()].
#'
#' @return No return value. This function is called for its side effect of
#'   creating a plot on the active graphics device.
#'
#' @examples
#' \donttest{
#' try({
#'   if (interactive()) {
#'     initializeMazamaSpatialUtils()
#'
#'     locations <-
#'       OpenAQ_createLocations(
#'         countryCodes = "US",
#'         stateCodes = "IL",
#'         counties = "Cook",
#'         api_key = OPENAQ_API_KEY
#'       )
#'
#'     # Plot all lifespans
#'     locations %>%
#'       OpenAQ_lifespanPlot()
#'
#'     # Label locations with OpenAQ integer ids
#'     locations %>%
#'       OpenAQ_lifespanPlot(
#'         showLocation = TRUE,
#'         cex = 0.6,
#'         lwd = 2,
#'         moreSpace = 0.3
#'       )
#'
#'     # Arrange by lifespan before plotting
#'     locations %>%
#'       dplyr::mutate(lifespan = .data$datetime_last - .data$datetime_first) %>%
#'       dplyr::arrange(.data$lifespan) %>%
#'       OpenAQ_lifespanPlot(
#'         showLocation = TRUE,
#'         cex = 0.6,
#'         lwd = 2,
#'         moreSpace = 0.3
#'       )
#'   }
#' }, silent = FALSE)
#' }
#'
#' @export
#' @importFrom rlang .data
OpenAQ_lifespanPlot <- function(
    locations,
    showLocation = FALSE,
    locationIdentifier = c("id", "locationID"),
    moreSpace = 0,
    ...
) {

  # ----- Validate parameters --------------------------------------------------

  result <- try({
    if ( !is.data.frame(locations) )
      stop("First argument is not a data frame.")
  }, silent = TRUE)

  if ( class(result) %in% "try-error" ) {
    err_msg <- geterrmessage()
    if ( stringr::str_detect(err_msg, "object .* not found") ) {
      stop(paste0(err_msg, "\n(Did you forget to pass in the 'locations' object?)"))
    } else {
      stop(err_msg)
    }
  }

  if ( !"datetime_first" %in% names(locations) ) {
    stop(
      "Parameter 'locations' does not have a 'datetime_first' column as ",
      "required by OpenAQ_lifespanPlot()"
    )
  }

  if ( !"datetime_last" %in% names(locations) ) {
    stop(
      "Parameter 'locations' does not have a 'datetime_last' column as ",
      "required by OpenAQ_lifespanPlot()"
    )
  }

  locationIdentifier <- match.arg(locationIdentifier)

  if ( showLocation && !(locationIdentifier %in% names(locations)) ) {
    stop(
      "Parameter 'locations' does not have a '", locationIdentifier,
      "' column as required by OpenAQ_lifespanPlot()"
    )
  }

  MazamaCoreUtils::setIfNull(moreSpace, 0)

  # ----- Plot -----------------------------------------------------------------

  index <- seq_len(nrow(locations))

  x0 <- locations$datetime_first
  y0 <- index
  x1 <- locations$datetime_last
  y1 <- index

  # plot.default

  argsList <- list(...)

  argsList$x <- c(x0, x1)
  argsList$y <- c(y0, y1)
  argsList$pch <- ifelse("pch" %in% names(argsList), argsList$pch, 16)
  argsList$cex <- ifelse("cex" %in% names(argsList), argsList$cex, 0.5)
  argsList$col <- ifelse("col" %in% names(argsList), argsList$col, "gray50")
  argsList$xlab <- ifelse("xlab" %in% names(argsList), argsList$xlab, "")
  argsList$ylab <- ifelse("ylab" %in% names(argsList), argsList$ylab, "")
  argsList$main <- ifelse("main" %in% names(argsList), argsList$main, "OpenAQ Location Reporting Lifespan")
  argsList$las <- ifelse("las" %in% names(argsList), argsList$las, 1)

  # X-axis
  if ( !is.null(moreSpace) ) {
    start <- min(locations$datetime_first, na.rm = TRUE)
    end <- max(locations$datetime_last, na.rm = TRUE)
    days <- difftime(end, start, units = "days") %>% as.numeric()

    # Expand by 2% on either end by default
    start <- start - lubridate::ddays(days / 50)
    end <- end + lubridate::ddays(days / 50)

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
  argsList$xlab <- NULL
  argsList$ylab <- NULL
  argsList$main <- NULL
  argsList$las <- NULL

  argsList$x0 <- x0
  argsList$y0 <- y0
  argsList$x1 <- x1
  argsList$y1 <- y1

  argsList$col <- ifelse("col" %in% names(argsList), argsList$col, "gray50")
  argsList$lty <- ifelse("lty" %in% names(argsList), argsList$lty, "solid")
  argsList$lwd <- ifelse("lwd" %in% names(argsList), argsList$lwd, 1)

  do.call(graphics::segments, argsList)

  # location labels

  if ( showLocation ) {

    argsList <- list(...)

    graphics::text(
      x0,
      y0,
      locations[[locationIdentifier]],
      pos = 2,
      cex = ifelse("cex" %in% names(argsList), argsList$cex, 0.5),
      xpd = ifelse("xpd" %in% names(argsList), argsList$xpd, NA)
    )

  }

  # ----- Return ---------------------------------------------------------------

  invisible(NULL)

}
