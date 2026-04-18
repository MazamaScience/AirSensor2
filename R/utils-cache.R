# Package-private cache --------------------------------------------------------

.pkg_cache <- new.env(parent = emptyenv())

.pkg_cache$OpenAQ_countries <- NULL
.pkg_cache$OpenAQ_instruments <- NULL
.pkg_cache$OpenAQ_licenses <- NULL
.pkg_cache$OpenAQ_manufacturers <- NULL
.pkg_cache$OpenAQ_owners <- NULL
.pkg_cache$OpenAQ_parameters <- NULL
.pkg_cache$OpenAQ_providers <- NULL


# Internal helpers -------------------------------------------------------------

#' @keywords internal
requireOpenAQ <- function() {

  if ( !requireNamespace("openaq", quietly = TRUE) ) {
    stop(
      "Package 'openaq' is required for OpenAQ-related functionality. ",
      "Please install it with install.packages('openaq').",
      call. = FALSE
    )
  }

}


#' @keywords internal
downloadOpenAQReferenceTable <- function(downloadFunction) {

  requireOpenAQ()

  tryCatch(
    downloadFunction(limit = 1000),
    error = function(e) {
      stop(
        "Unable to download OpenAQ reference data. ",
        "Please check your internet connection and try again.\n",
        "Original error: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )

}


# OpenAQ reference table helpers -----------------------------------------------

#' @keywords internal
internal_OpenAQ_getCountries <- function() {

  if ( is.null(.pkg_cache$OpenAQ_countries) ) {
    .pkg_cache$OpenAQ_countries <-
      downloadOpenAQReferenceTable(openaq::list_countries)
  }

  return(.pkg_cache$OpenAQ_countries)

}


#' @keywords internal
internal_OpenAQ_getInstruments <- function() {

  if ( is.null(.pkg_cache$OpenAQ_instruments) ) {
    .pkg_cache$OpenAQ_instruments <-
      downloadOpenAQReferenceTable(openaq::list_instruments)
  }

  return(.pkg_cache$OpenAQ_instruments)

}


#' @keywords internal
internal_OpenAQ_getLicenses <- function() {

  if ( is.null(.pkg_cache$OpenAQ_licenses) ) {
    .pkg_cache$OpenAQ_licenses <-
      downloadOpenAQReferenceTable(openaq::list_licenses)
  }

  return(.pkg_cache$OpenAQ_licenses)

}


#' @keywords internal
internal_OpenAQ_getManufacturers <- function() {

  if ( is.null(.pkg_cache$OpenAQ_manufacturers) ) {
    .pkg_cache$OpenAQ_manufacturers <-
      downloadOpenAQReferenceTable(openaq::list_manufacturers)
  }

  return(.pkg_cache$OpenAQ_manufacturers)

}


#' @keywords internal
internal_OpenAQ_getOwners <- function() {

  if ( is.null(.pkg_cache$OpenAQ_owners) ) {
    .pkg_cache$OpenAQ_owners <-
      downloadOpenAQReferenceTable(openaq::list_owners)
  }

  return(.pkg_cache$OpenAQ_owners)

}


#' @keywords internal
internal_OpenAQ_getParameters <- function() {

  if ( is.null(.pkg_cache$OpenAQ_parameters) ) {
    .pkg_cache$OpenAQ_parameters <-
      downloadOpenAQReferenceTable(openaq::list_parameters)
  }

  return(.pkg_cache$OpenAQ_parameters)

}


#' @keywords internal
internal_OpenAQ_getProviders <- function() {

  if ( is.null(.pkg_cache$OpenAQ_providers) ) {
    .pkg_cache$OpenAQ_providers <-
      downloadOpenAQReferenceTable(openaq::list_providers)
  }

  return(.pkg_cache$OpenAQ_providers)

}


# Reset helper -----------------------------------------------------------------

#' @keywords internal
internal_resetOpenAQCache <- function() {

  .pkg_cache$OpenAQ_countries <- NULL
  .pkg_cache$OpenAQ_instruments <- NULL
  .pkg_cache$OpenAQ_licenses <- NULL
  .pkg_cache$OpenAQ_manufacturers <- NULL
  .pkg_cache$OpenAQ_parameters <- NULL
  .pkg_cache$OpenAQ_providers <- NULL

  invisible(NULL)

}

