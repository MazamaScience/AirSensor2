#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.error logger.debug logger.isInitialized
#' @importFrom MazamaCoreUtils getAPIKey
#'
#' @title Checks the validity and returns the key type for the provided \code{api_key}.
#'
#' @param api_key PurpleAir API key.
#' @param baseUrl URL endpoint for the "check key" API.
#'
#' @return List of information.
#'
#' @description Sends a request to the PurpleAirAPI API endpoint described at:
#' \url{https://api.purpleair.com/#api-keys-check-api-key}
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(AirSensor2)
#'
#'   pa_checkAPIKey(
#'     api_key = MY_API_READ_KEY
#'   )
#'
#' }, silent = FALSE)
#' }

pa_checkAPIKey <- function(
    api_key = NULL,
    baseUrl = "https://api.purpleair.com/v1/keys"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(baseUrl)

  # ----- Request data ---------------------------------------------------------

  # Strip off any final "/"
  baseUrl <- stringr::str_replace(baseUrl, "/$", "")

  # See: https://api.purpleair.com/#api-keys-check-api-key
  webserviceUrl <- baseUrl

  queryList <-
    list(
    )

  PAList <- PurpleAir_API_Request(
    webserviceUrl = webserviceUrl,
    apiKey = api_key,
    queryList = queryList
  )

  return(PAList)

}


#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.error logger.debug logger.isInitialized
#' @importFrom MazamaCoreUtils getAPIKey
#'
#' @title Retrieves a list of all groups owned by the provided \code{api_key}.
#'
#' @param api_key PurpleAir API key.
#' @param baseUrl URL endpoint for the "get groups list" API.
#'
#' @return List of information.
#'
#' @description Sends a request to the PurpleAirAPI API endpoint described at:
#' \url{https://api.purpleair.com/#api-keys-check-api-key}
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(AirSensor2)
#'
#'   pa_getGroupsList(
#'     api_key = MY_API_READ_KEY
#'   )
#'
#' }, silent = FALSE)
#' }

pa_getGroupsList <- function(
    api_key = NULL,
    baseUrl = "https://api.purpleair.com/v1/groups"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(baseUrl)

  # ----- Request data ---------------------------------------------------------

  # Strip off any final "/"
  baseUrl <- stringr::str_replace(baseUrl, "/$", "")

  # See: https://api.purpleair.com/#api-keys-check-api-key
  webserviceUrl <- baseUrl

  queryList <-
    list(
    )

  # TODO:  Use try and suggest using the API_READ_KEY
  PAList <- PurpleAir_API_Request(
    webserviceUrl = webserviceUrl,
    apiKey = api_key,
    queryList = queryList
  )

  return(PAList)

}


# ===== Private Functions ======================================================


PurpleAir_API_Request <- function(
    webserviceUrl = NULL,
    apiKey = NULL,
    queryList = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(webserviceUrl)
  MazamaCoreUtils::stopIfNull(apiKey)
  MazamaCoreUtils::stopIfNull(queryList)

  # ----- Request data ---------------------------------------------------------

  # NOTE:  https://httr.r-lib.org/articles/quickstart.html
  r <-
    httr::GET(
      webserviceUrl,
      httr::add_headers("X-API-Key" = apiKey),
      query = queryList
    )

  # * Error response -----

  if ( httr::http_error(r) ) {  # web service failed to respond

    status_code <- httr::status_code(r)

    err_msg <- sprintf(
      "web service error %s from:\n  %s\n\n%s",
      status_code,
      webserviceUrl,
      httpcode::http_code(status_code)$explanation
    )

    if ( logger.isInitialized() ) {
      logger.error("Web service failed to respond: %s", webserviceUrl)
      logger.error(err_msg)
    }

    stop(err_msg)

  }

  # * Success response -----

  content <- httr::content(r, as = "text", encoding = "UTF-8") # don't interpret

  # ----- Parse JSON -----------------------------------------------------------

  # * Convert JSON to an R list -----

  PAList <-
    jsonlite::fromJSON(
      content,
      simplifyVector = TRUE,
      simplifyDataFrame = TRUE,
      simplifyMatrix = TRUE,
      flatten = FALSE
    )

  return(PAList)

}
