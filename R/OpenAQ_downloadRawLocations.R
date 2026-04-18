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
#' @param api_key OpenAQ API key.
#' @param bbox Bounding box passed to [openaq::list_locations()].
#' @param providers_id Provider IDs passed to [openaq::list_locations()].
#' @param manufacturers_id Manufacturer IDs passed to [openaq::list_locations()].
#' @param monitor Logical value passed to [openaq::list_locations()].
#' @param mobile Logical value passed to [openaq::list_locations()].
#' @param countries_id Country IDs passed to [openaq::list_locations()].
#' @param limit Maximum number of records to request per page.
#' @param maxPages Maximum number of pages to request automatically.
#' @param sleepSeconds Number of seconds to pause between additional requests.
#'
#' @return A dataframe of raw OpenAQ location metadata.
#' @export

OpenAQ_downloadRawLocations <- function(
    api_key = NULL,
    bbox = NULL,
    providers_id = NULL,
    manufacturers_id = NULL,
    monitor = NULL,
    mobile = FALSE,
    countries_id = NULL,
    limit = 1000,
    maxPages = 1,
    sleepSeconds = 0.2
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

  # ----- Helper to request one page -------------------------------------------

  requestPage <- function(page) {
    openaq::list_locations(
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
  }

  # ----- Request pages --------------------------------------------------------

  dataList <- list()
  lastMeta <- NULL
  page <- 1
  morePagesPossible <- FALSE

  page <- 1

  while (page <= maxPages) {

    if ( page > 1 && sleepSeconds > 0 ) {
      Sys.sleep(sleepSeconds)
    }

    pageData <- requestPage(page = page)
    pageDF <- as.data.frame(pageData, stringsAsFactors = FALSE)
    pageMeta <- attr(pageData, "meta")

    dataList[[page]] <- pageDF
    lastMeta <- pageMeta

    nReturned <- nrow(pageDF)

    # Metadata from first page can help with user-facing warnings/messages,
    # but row count is the most reliable signal for when pagination is done.
    if ( page == 1 && !is.null(pageMeta$found) && !is.null(pageMeta$limit) ) {

      found_raw <- pageMeta$found
      meta_limit <- suppressWarnings(as.integer(pageMeta$limit))
      found_num <- suppressWarnings(
        as.integer(gsub("[^0-9]", "", as.character(found_raw)))
      )
      found_is_gt <- is.character(found_raw) && grepl("^\\s*>", found_raw)

      if ( maxPages == 1 && !is.na(found_num) && !is.na(meta_limit) ) {
        if ( found_is_gt || found_num > meta_limit ) {
          warning(
            "OpenAQ has ", found_raw, " matching locations, but only the first ",
            meta_limit, " can be returned at once.\n",
            "Try narrowing your request by specifying a state, county, or provider.",
            call. = FALSE
          )
        }
      }

    }

    # Stop when the current page is not full. This usually means we have reached
    # the final page.
    if ( nReturned < limit ) {
      morePagesPossible <- FALSE
      break
    }

    # If we have reached maxPages and still got a full page, there may be more.
    if ( page >= maxPages ) {
      morePagesPossible <- TRUE
      break
    }

    page <- page + 1

  }

  # ----- Combine results ------------------------------------------------------

  openaq_raw <- dplyr::bind_rows(dataList)

  # Preserve metadata from the last request, if available
  if ( !is.null(lastMeta) ) {
    attr(openaq_raw, "meta") <- lastMeta
    attr(openaq_raw, "meta")$page <- page
  }

  # ----- Warn if more pages may exist -----------------------------------------

  if ( morePagesPossible ) {

    meta <- attr(openaq_raw, "meta")
    found_raw <- NULL

    if ( !is.null(meta) && !is.null(meta$found) ) {
      found_raw <- meta$found
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

  # Remove any duplicates
  openaq_raw <- dplyr::distinct(openaq_raw)

  return(openaq_raw)

}

