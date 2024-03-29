#' @name APIKeys
#'
#' @title Work with API keys
#'
#' @param provider Character string used to identify an API key. Used as the
#' first argument to \code{setAPIKey()} and \code{getAPIKey()}.
#' @param key Character API key. Used as the second argument to \code{setAPIKey()}.
#'
#' @return An API key string or a list of \code{provider:key} pairs.
#'
#' @description This package maintains an internal set of API keys which
#' users can set using \code{setAPIKey()}. These keys will be remembered for
#' the duration of an R session. In functions that accept an API key argument,
#' if the passed in API key is \code{NULL}, code will look up an appropriate
#' named API key to see if that key has been set globally. Setting keys globally
#' is a convenience that simplifies scripts written by end users.
#'
#' Currently supported API keys include:
#'
#' \itemize{
#'   \item{\code{"PurpleAir-read"} -- PurpleAir \code{API_READ_KEY}}
#'   \item{\code{"PurpleAir-write"} -- PurpleAir \code{API_WRITE_KEY}}
#'   \item{\code{"Clarity-read"} -- Clarity \code{API_READ_KEY}}
#' }
#'
#' @details Three API key support functions are imported from the \pkg{MazamaCoreUtils}
#' package where they are described in more detail:
#' \itemize{
#'   \item{\code{MazamaCoreUtils::\link[MazamaCoreUtils:getAPIKey]{getAPIKey}}}
#'   \item{\code{MazamaCoreUtils::\link[MazamaCoreUtils:setAPIKey]{setAPIKey}}}
#'   \item{\code{MazamaCoreUtils::\link[MazamaCoreUtils:showAPIKeys]{showAPIKeys}}}
#' }
#'
#' @references \href{https://www2.purpleair.com}{PurpleAir}
#' @references \href{https://api.purpleair.com/}{PurpleAir API}
#' @references \href{https://www2.purpleair.com/policies/terms-of-service}{PurpleAir Terms of service}
#' @references \href{https://www2.purpleair.com/pages/license}{PurpleAir Data license}
#' @references \href{https://www2.purpleair.com/pages/attribution}{PurpleAir Data Attribution}
#'
#' @references \href{https://api-guide.clarity.io/getting-started/}{Clarity API Guide}
#'
#' @examples
#' library(AirSensor2)
#'
#' # Start out with no keys (unless the user has set them)
#' showAPIKeys()
#' getAPIKey("PurpleAir-read")
#'
#' # Set specific keys
#' setAPIKey("PurpleAir-read", "********-2A00-11EB-A8CD-42010A800126")
#' setAPIKey("PurpleAir-write", "********-2A00-11EB-A8CD-42010A800126")
#' showAPIKeys()
#' getAPIKey("PurpleAir-read")
#'
#' # Reset the read key
#' setAPIKey("PurpleAir-read", NULL)
#' showAPIKeys()
#' getAPIKey("PurpleAir-read")
#'
#' # Reset the write key
#' setAPIKey("PurpleAir-write", NULL)
#' showAPIKeys()
#'
NULL
