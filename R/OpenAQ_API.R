# ===== Download data ==========================================================

#' Get OpenAQ country metadata
#'
#' Returns OpenAQ country metadata as a tibble. Data are downloaded only once
#' per R session and then cached for re-use.
#'
#' This function requires the optional package `openaq`.
#'
#' For more information about the OpenAQ data access API, see:
#' \url{https://docs.openaq.org/about/about}
#'
#' @return A tibble of OpenAQ country metadata.
#' @export
OpenAQ_getCountries <- function() {

  return(internal_OpenAQ_getCountries())

}


#' Get OpenAQ instrument metadata
#'
#' Returns OpenAQ instrument metadata as a tibble. Data are downloaded only once
#' per R session and then cached for re-use.
#'
#' This function requires the optional package `openaq`.
#'
#' For more information about the OpenAQ data access API, see:
#' \url{https://docs.openaq.org/about/about}
#'
#' @return A tibble of OpenAQ instrument metadata.
#' @export
OpenAQ_getInstruments <- function() {

  return(internal_OpenAQ_getInstruments())

}


#' Get OpenAQ license metadata
#'
#' Returns OpenAQ license metadata as a tibble. Data are downloaded only once
#' per R session and then cached for re-use.
#'
#' This function requires the optional package `openaq`.
#'
#' For more information about the OpenAQ data access API, see:
#' \url{https://docs.openaq.org/about/about}
#'
#' @return A tibble of OpenAQ license metadata.
#' @export
OpenAQ_getLicenses <- function() {

  return(internal_OpenAQ_getLicenses())

}


#' Get OpenAQ manufacturer metadata
#'
#' Returns OpenAQ manufacturer metadata as a tibble. Data are downloaded only
#' once per R session and then cached for re-use.
#'
#' This function requires the optional package `openaq`.
#'
#' For more information about the OpenAQ data access API, see:
#' \url{https://docs.openaq.org/about/about}
#'
#' @return A tibble of OpenAQ manufacturer metadata.
#' @export
OpenAQ_getManufacturers <- function() {

  return(internal_OpenAQ_getManufacturers())

}


#' Get OpenAQ owner metadata
#'
#' Returns OpenAQ owner metadata as a tibble. Data are downloaded only once
#' per R session and then cached for re-use.
#'
#' This function requires the optional package `openaq`.
#'
#' For more information about the OpenAQ data access API, see:
#' \url{https://docs.openaq.org/about/about}
#'
#' @return A tibble of OpenAQ owner metadata.
#' @export
OpenAQ_getOwners <- function() {

  return(internal_OpenAQ_getOwners())

}


#' Get OpenAQ parameter metadata
#'
#' Returns OpenAQ parameter metadata as a tibble. Data are downloaded only once
#' per R session and then cached for re-use.
#'
#' This function requires the optional package `openaq`.
#'
#' For more information about the OpenAQ data access API, see:
#' \url{https://docs.openaq.org/about/about}
#'
#' @return A tibble of OpenAQ parameter metadata.
#' @export
OpenAQ_getParameters <- function() {

  return(internal_OpenAQ_getParameters())

}


#' Get OpenAQ provider metadata
#'
#' Returns OpenAQ provider metadata as a tibble. Data are downloaded only once
#' per R session and then cached for re-use.
#'
#' This function requires the optional package `openaq`.
#'
#' For more information about the OpenAQ data access API, see:
#' \url{https://docs.openaq.org/about/about}
#'
#' @return A tibble of OpenAQ provider metadata.
#' @export
OpenAQ_getProviders <- function() {

  return(internal_OpenAQ_getProviders())

}

# ===== Convert to ID ==========================================================

#' Convert country codes or names to OpenAQ country IDs
#'
#' Accepts a character vector of ISO country codes (e.g. `"US"`) or country
#' names (e.g. `"United States"`) and returns the corresponding OpenAQ
#' country IDs.
#'
#' Matching is case-insensitive. If no match is found, `NA` is returned.
#'
#' @param x Character vector of country codes or names.
#'
#' @return Integer vector of OpenAQ country IDs.
#'
#' @examples
#' \dontrun{
#' OpenAQ_countryToID(c("US", "Canada", "France", "Unknown"))
#' }
#'
#' @export
#' @importFrom rlang .data
OpenAQ_countryToID <- function(x) {

  # Basic validation
  if ( is.null(x) ) {
    return(NULL)
  }

  if ( !is.character(x) ) {
    stop("`x` must be a character vector.", call. = FALSE)
  }

  countries <- internal_OpenAQ_getCountries()

  # Normalize inputs
  x_upper <- toupper(x)

  # Prepare lookup vectors
  code_upper <- toupper(countries$code)
  name_upper <- toupper(countries$name)

  # Initialize result
  result <- rep(NA_integer_, length(x))

  for ( i in seq_along(x_upper) ) {

    val <- x_upper[i]

    # First try ISO code match
    idx <- match(val, code_upper)

    # If not found, try name match
    if ( is.na(idx) ) {
      idx <- match(val, name_upper)
    }

    if ( !is.na(idx) ) {
      result[i] <- countries$id[idx]
    }

  }

  return(result)

}

#' Convert instrument names to OpenAQ instrument IDs
#'
#' Accepts a character vector of instrument names and returns the corresponding
#' OpenAQ instrument IDs.
#'
#' Matching is case-insensitive. If no match is found, `NA` is returned.
#'
#' @param x Character vector of instrument names.
#'
#' @return Integer vector of OpenAQ instrument IDs.
#'
#' @examples
#' \dontrun{
#' OpenAQ_instrumentToID(c("BAM 1020", "PurpleAir PA-II", "Unknown"))
#' }
#'
#' @export
OpenAQ_instrumentToID <- function(x) {

  # Basic validation
  if ( is.null(x) ) {
    return(NULL)
  }

  if ( !is.character(x) ) {
    stop("`x` must be a character vector.", call. = FALSE)
  }

  instruments <- internal_OpenAQ_getInstruments()

  # Normalize inputs
  x_upper <- toupper(x)
  name_upper <- toupper(instruments$name)

  # Perform matching (vectorized)
  idx <- match(x_upper, name_upper)

  # Map to IDs
  result <- instruments$id[idx]

  # Ensure integer type and NA handling
  result <- as.integer(result)

  return(result)

}

#' Convert license names to OpenAQ license IDs
#'
#' Accepts a character vector of license names and returns the corresponding
#' OpenAQ license IDs.
#'
#' Matching is case-insensitive. If no match is found, `NA` is returned.
#'
#' @param x Character vector of license names.
#'
#' @return Integer vector of OpenAQ license IDs.
#'
#' @examples
#' \dontrun{
#' OpenAQ_licenseToID(c("CC BY 4.0", "Public Domain", "Unknown"))
#' }
#'
#' @export
OpenAQ_licenseToID <- function(x) {

  # Basic validation
  if ( is.null(x) ) {
    return(NULL)
  }

  if ( !is.character(x) ) {
    stop("`x` must be a character vector.", call. = FALSE)
  }

  licenses <- internal_OpenAQ_getLicenses()

  # Normalize inputs
  x_upper <- toupper(x)
  name_upper <- toupper(licenses$name)

  # Match
  idx <- match(x_upper, name_upper)

  # Map to IDs
  result <- licenses$id[idx]

  # Ensure integer output
  result <- as.integer(result)

  return(result)

}

#' Convert manufacturer names to OpenAQ manufacturer IDs
#'
#' Accepts a character vector of manufacturer names and returns the corresponding
#' OpenAQ manufacturer IDs.
#'
#' Matching is case-insensitive. If no match is found, `NA` is returned.
#'
#' @param x Character vector of manufacturer names.
#'
#' @return Integer vector of OpenAQ manufacturer IDs.
#'
#' @examples
#' \dontrun{
#' OpenAQ_manufacturerToID(c("Met One Instruments", "Thermo Fisher Scientific", "Unknown"))
#' }
#'
#' @export
OpenAQ_manufacturerToID <- function(x) {

  # Basic validation
  if ( is.null(x) ) {
    return(NULL)
  }

  if ( !is.character(x) ) {
    stop("`x` must be a character vector.", call. = FALSE)
  }

  manufacturers <- internal_OpenAQ_getManufacturers()

  # Normalize inputs
  x_upper <- toupper(x)
  name_upper <- toupper(manufacturers$name)

  # Match
  idx <- match(x_upper, name_upper)

  # Map to IDs
  result <- manufacturers$id[idx]

  # Ensure integer output
  result <- as.integer(result)

  return(result)

}

#' Convert parameter names to OpenAQ parameter IDs
#'
#' Accepts a character vector of parameter names and returns the corresponding
#' OpenAQ parameter IDs. Matching is performed against both the `name` and
#' `displayName` fields.
#'
#' Matching is case-insensitive. If no match is found, `NA` is returned.
#' If both `name` and `displayName` match, `name` is preferred.
#'
#' @param x Character vector of parameter names.
#'
#' @return Integer vector of OpenAQ parameter IDs.
#'
#' @examples
#' \dontrun{
#' OpenAQ_parameterToID(c("pm25", "PM2.5", "Ozone", "Unknown"))
#' }
#'
#' @export
OpenAQ_parameterToID <- function(x) {

  # Basic validation
  if ( is.null(x) ) {
    return(NULL)
  }

  if ( !is.character(x) ) {
    stop("`x` must be a character vector.", call. = FALSE)
  }

  parameters <- internal_OpenAQ_getParameters()

  # Normalize inputs
  x_upper <- toupper(x)
  name_upper <- toupper(parameters$name)
  display_upper <- toupper(parameters$displayName)

  # Initialize result
  result <- rep(NA_integer_, length(x_upper))

  # First pass: match on `name`
  idx_name <- match(x_upper, name_upper)
  matched_name <- !is.na(idx_name)

  result[matched_name] <- parameters$id[idx_name[matched_name]]

  # Second pass: match remaining on `displayName`
  remaining <- is.na(result)
  if ( any(remaining) ) {
    idx_display <- match(x_upper[remaining], display_upper)
    matched_display <- !is.na(idx_display)

    result[remaining][matched_display] <-
      parameters$id[idx_display[matched_display]]
  }

  return(as.integer(result))

}

#' Convert provider names to OpenAQ provider IDs
#'
#' Accepts a character vector of provider names and returns the corresponding
#' OpenAQ provider IDs. Matching is performed against both the `name` and
#' `export_prefix` fields.
#'
#' Matching is case-insensitive. If no match is found, `NA` is returned.
#' If both `name` and `export_prefix` match, `name` is preferred.
#'
#' @param x Character vector of provider names or export prefixes.
#'
#' @return Integer vector of OpenAQ provider IDs.
#'
#' @examples
#' \dontrun{
#' OpenAQ_providerToID(c("US EPA", "airnow", "Unknown"))
#' }
#'
#' @export
OpenAQ_providerToID <- function(x) {

  # Basic validation
  if ( is.null(x) ) {
    return(NULL)
  }

  if ( !is.character(x) ) {
    stop("`x` must be a character vector.", call. = FALSE)
  }

  providers <- internal_OpenAQ_getProviders()

  # Normalize inputs
  x_upper <- toupper(x)
  name_upper <- toupper(providers$name)
  prefix_upper <- toupper(providers$export_prefix)

  # Initialize result
  result <- rep(NA_integer_, length(x_upper))

  # ---- First pass: match on name ---------------------------------------------

  idx_name <- match(x_upper, name_upper)
  matched_name <- !is.na(idx_name)

  if ( any(matched_name) ) {
    result[matched_name] <- providers$id[idx_name[matched_name]]
  }

  # ---- Second pass: match remaining on export_prefix --------------------------

  remaining_idx <- which(is.na(result))

  if ( length(remaining_idx) > 0 ) {

    idx_prefix <- match(x_upper[remaining_idx], prefix_upper)
    matched_prefix <- !is.na(idx_prefix)

    if ( any(matched_prefix) ) {
      result[remaining_idx[matched_prefix]] <-
        providers$id[idx_prefix[matched_prefix]]
    }

  }

  return(as.integer(result))

}
