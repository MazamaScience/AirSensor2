#' Download raw OpenAQ location data
#'
#' Downloads raw location metadata from OpenAQ and optionally retrieves
#' additional pages when many matching locations are found.
#'
#' When more matching locations are available than can be returned in a single
#' request, this function can automatically request additional pages up to
#' `maxPages`. If more pages may still be available after `maxPages` pages have
#' been retrieved, a warning is issued suggesting that the user narrow the
#' request.
#'
#' @param bbox Bounding box passed to [openaq::list_locations()].
#' @param providers_id Provider IDs passed to [openaq::list_locations()].
#' @param manufacturers_id Manufacturer IDs passed to [openaq::list_locations()].
#' @param monitor Logical value passed to [openaq::list_locations()].
#' @param mobile Logical value passed to [openaq::list_locations()].
#' @param countries_id Country IDs passed to [openaq::list_locations()].
#' @param limit Maximum number of records to request per page.
#' @param maxPages Maximum number of pages to request automatically.
#' @param sleepSeconds Number of seconds to pause between additional requests.
#' @param api_key OpenAQ API key.
#'
#' @return A dataframe of raw OpenAQ location metadata.
#' @export
OpenAQ_downloadRawLocations <- function(
    bbox = NULL,
    providers_id = NULL,
    manufacturers_id = NULL,
    monitor = NULL,
    mobile = FALSE,
    countries_id = NULL,
    limit = 1000,
    maxPages = 1,
    sleepSeconds = 0.2,
    api_key = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  if ( !is.numeric(limit) || length(limit) != 1 || is.na(limit) || limit < 1 ) {
    stop("parameter 'limit' must be a single positive number")
  }

  if ( !is.numeric(maxPages) || length(maxPages) != 1 || is.na(maxPages) || maxPages < 1 ) {
    stop("parameter 'maxPages' must be a single positive number")
  }

  if ( !is.numeric(sleepSeconds) || length(sleepSeconds) != 1 || is.na(sleepSeconds) || sleepSeconds < 0 ) {
    stop("parameter 'sleepSeconds' must be a single non-negative number")
  }

  limit <- as.integer(limit)
  maxPages <- as.integer(maxPages)

  # ----- Track metadata from requests -----------------------------------------

  firstMeta <- NULL
  lastMeta <- NULL

  # ----- Download pages -------------------------------------------------------

  openaq_raw <- OpenAQ_downloadPages(
    fetchPageFUN = function(page, limit) {

      pageData <- openaq::list_locations(
        bbox = bbox,
        ###coordinates = NULL,
        ###radius = NULL,
        providers_id = providers_id,
        ###parameters_id = NULL,
        ###owner_contacts_id = NULL,
        manufacturers_id = manufacturers_id,
        ###licenses_id = NULL,
        monitor = monitor,
        mobile = mobile,
        ###instruments_id = NULL,
        ###iso = NULL,
        countries_id = countries_id,
        ###order_by = NULL,
        ###sort_order = NULL,
        limit = limit,
        page = page,
        ###as_data_frame = TRUE,
        ###dry_run = FALSE,
        ###rate_limit = FALSE,
        api_key = api_key
      )

      pageMeta <- attr(pageData, "meta")

      if ( is.null(firstMeta) ) {
        firstMeta <- pageMeta
      }

      lastMeta <<- pageMeta

      as.data.frame(pageData, stringsAsFactors = FALSE)

    },
    limit = limit,
    maxPages = maxPages,
    sleepSeconds = sleepSeconds,
    warnTruncated = FALSE
  )

  # ----- Preserve metadata from the last request ------------------------------

  if ( !is.null(lastMeta) ) {
    attr(openaq_raw, "meta") <- lastMeta
    attr(openaq_raw, "meta")$page <- attr(openaq_raw, "pagesRetrieved")
  }

  # ----- Warn if more pages may exist -----------------------------------------

  possiblyTruncated <- isTRUE(attr(openaq_raw, "possiblyTruncated"))

  if ( possiblyTruncated ) {

    found_raw <- NULL

    if ( !is.null(firstMeta) && !is.null(firstMeta$found) ) {
      found_raw <- firstMeta$found
    }

    if ( !is.null(found_raw) ) {
      warning(
        "OpenAQ has ", found_raw, " matching locations, but only the first ",
        nrow(openaq_raw), " were downloaded.\n",
        "Try narrowing your request by specifying a state, county, or provider.",
        call. = FALSE
      )
    } else {
      warning(
        "OpenAQ may have more matching locations than were downloaded.\n",
        "Only the first ", nrow(openaq_raw), " were downloaded.\n",
        "Try narrowing your request by specifying a state, county, or provider.",
        call. = FALSE
      )
    }

  }

  # ----- Remove any duplicates ------------------------------------------------

  openaq_raw <- dplyr::distinct(openaq_raw)

  return(openaq_raw)

}
