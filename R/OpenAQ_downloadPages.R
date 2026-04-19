#' Download multiple pages of OpenAQ results
#'
#' Repeatedly calls a page-fetching function and combines the returned
#' data frames with `dplyr::bind_rows()`. Additional pages are requested
#' until a page returns fewer than `limit` rows, an empty page is returned,
#' or `maxPages` pages have been retrieved.
#'
#' This helper is intended for internal reuse by other OpenAQ download
#' functions such as location, sensor, and measurement download wrappers.
#'
#' @param fetchPageFUN A function with signature `function(page, limit)` that
#'   returns a data frame for the requested page.
#' @param limit Maximum number of records to request per page.
#' @param maxPages Maximum number of pages to request.
#' @param sleepSeconds Number of seconds to pause before requesting each
#'   additional page after page 1.
#' @param warnTruncated Logical specifying whether to issue a warning when
#'   `maxPages` is reached and the last page appears full, suggesting that
#'   additional results may still be available.
#' @param verbose Logical specifying whether to emit progress messages.
#'
#' @return A data frame created by row-binding all retrieved pages.
#'   Attributes include:
#'   \describe{
#'     \item{pagesRetrieved}{Number of pages successfully retrieved.}
#'     \item{possiblyTruncated}{Logical indicating whether retrieval may have
#'       stopped before all available pages were downloaded.}
#'   }
#'
#' @noRd
OpenAQ_downloadPages <- function(
    fetchPageFUN,
    limit = 1000,
    maxPages = 1,
    sleepSeconds = 0.2,
    warnTruncated = TRUE,
    verbose = FALSE
) {

  # ----- Validate parameters --------------------------------------------------

  if ( !is.function(fetchPageFUN) ) {
    stop("'fetchPageFUN' must be a function.")
  }

  if ( !is.numeric(limit) || length(limit) != 1 || is.na(limit) || limit < 1 ) {
    stop("'limit' must be a single positive number.")
  }

  if ( !is.numeric(maxPages) || length(maxPages) != 1 || is.na(maxPages) || maxPages < 1 ) {
    stop("'maxPages' must be a single positive number.")
  }

  if ( !is.numeric(sleepSeconds) || length(sleepSeconds) != 1 || is.na(sleepSeconds) || sleepSeconds < 0 ) {
    stop("'sleepSeconds' must be a single non-negative number.")
  }

  if ( !is.logical(warnTruncated) || length(warnTruncated) != 1 || is.na(warnTruncated) ) {
    stop("'warnTruncated' must be TRUE or FALSE.")
  }

  if ( !is.logical(verbose) || length(verbose) != 1 || is.na(verbose) ) {
    stop("'verbose' must be TRUE or FALSE.")
  }

  limit <- as.integer(limit)
  maxPages <- as.integer(maxPages)

  # ----- Download pages -------------------------------------------------------

  pageList <- list()
  possiblyTruncated <- FALSE

  for ( page in seq_len(maxPages) ) {

    if ( page > 1 && sleepSeconds > 0 ) {
      Sys.sleep(sleepSeconds)
    }

    if ( verbose ) {
      message(sprintf("Requesting page %d of %d...", page, maxPages))
    }

    pageData <- fetchPageFUN(page = page, limit = limit)

    if ( !is.data.frame(pageData) ) {
      stop(sprintf("`fetchPageFUN()` must return a data frame. Page %d did not.", page))
    }

    page_nrow <- nrow(pageData)
    pageList[[page]] <- pageData

    if ( verbose ) {
      message(sprintf("Retrieved %d rows from page %d.", page_nrow, page))
    }

    # Stop when the API returns fewer than a full page, including zero rows.
    if ( page_nrow < limit ) {
      break
    }

    # If we have reached maxPages and the page is still full, there may be more.
    if ( page == maxPages && page_nrow == limit ) {
      possiblyTruncated <- TRUE
    }
  }

  # ----- Combine results ------------------------------------------------------

  data <- dplyr::bind_rows(pageList)

  attr(data, "pagesRetrieved") <- length(pageList)
  attr(data, "possiblyTruncated") <- possiblyTruncated

  # ----- Warn if needed -------------------------------------------------------

  if ( warnTruncated && possiblyTruncated ) {
    warning(
      paste0(
        "Downloaded ", length(pageList), " page(s) of results and the final page ",
        "contained ", limit, " rows. Additional results may still be available. ",
        "Consider narrowing the request or increasing `maxPages`."
      ),
      call. = FALSE
    )
  }

  return(data)

}
