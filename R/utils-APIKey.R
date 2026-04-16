#' @name APIKeys
#'
#' @title Work with API keys
#'
#' @param provider Character string used to identify an API key. Used as the
#' first argument to `setAPIKey()` and `getAPIKey()`.
#' @param key Character API key. Used as the second argument to `setAPIKey()`.
#'
#' @return An API key string or a list of `provider:key` pairs.
#'
#' @description This package maintains an internal set of API keys which
#' users can set using `setAPIKey()`. These keys will be remembered for
#' the duration of an R session. In functions that accept an API key argument,
#' if the passed in API key is `NULL`, code will look up an appropriate
#' named API key to see if that key has been set globally. Setting keys globally
#' is a convenience that simplifies scripts written by end users.
#'
#' Currently supported API keys include:
#'
#' \itemize{
#'   \item{`"PurpleAir-read"` -- PurpleAir `API_READ_KEY`}
#'   \item{`"PurpleAir-write"` -- PurpleAir `API_WRITE_KEY`}
#'   \item{`"Clarity-read"` -- Clarity `API_READ_KEY`}
#' }
#'
#' @details Three API key support functions are imported from the \pkg{MazamaCoreUtils}
#' package where they are described in more detail:
#' \itemize{
#'   \item{[MazamaCoreUtils::getAPIKey()]}
#'   \item{[MazamaCoreUtils::setAPIKey()]}
#'   \item{[MazamaCoreUtils::showAPIKeys()]}
#' }
#'
#' @references [PurpleAir](https://www2.purpleair.com)
#' @references [PurpleAir API](https://api.purpleair.com/)
#' @references [PurpleAir Terms of service](https://www2.purpleair.com/policies/terms-of-service)
#' @references [PurpleAir Data license](https://www2.purpleair.com/pages/license)
#' @references [PurpleAir Data Attribution](https://www2.purpleair.com/pages/attribution)
#'
#' @references [Clarity API Guide](https://api-guide.clarity.io/getting-started/)
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
