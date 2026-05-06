# Work with API keys

This package maintains an internal set of API keys which users can set
using \`setAPIKey()\`. These keys will be remembered for the duration of
an R session. In functions that accept an API key argument, if the
passed in API key is \`NULL\`, code will look up an appropriate named
API key to see if that key has been set globally. Setting keys globally
is a convenience that simplifies scripts written by end users.

Currently supported API keys include:

- \`"PurpleAir-read"\` – PurpleAir \`API_READ_KEY\`

- \`"PurpleAir-write"\` – PurpleAir \`API_WRITE_KEY\`

- \`"Clarity-read"\` – Clarity \`API_READ_KEY\`

## Arguments

- provider:

  Character string used to identify an API key. Used as the first
  argument to \`setAPIKey()\` and \`getAPIKey()\`.

- key:

  Character API key. Used as the second argument to \`setAPIKey()\`.

## Value

An API key string or a list of \`provider:key\` pairs.

## Details

Three API key support functions are imported from the MazamaCoreUtils
package where they are described in more detail:

- \[MazamaCoreUtils::getAPIKey()\]

- \[MazamaCoreUtils::setAPIKey()\]

- \[MazamaCoreUtils::showAPIKeys()\]

## References

\[PurpleAir\](https://www2.purpleair.com)

\[PurpleAir API\](https://api.purpleair.com/)

\[PurpleAir Terms of
service\](https://www2.purpleair.com/policies/terms-of-service)

\[PurpleAir Data license\](https://www2.purpleair.com/pages/license)

\[PurpleAir Data
Attribution\](https://www2.purpleair.com/pages/attribution)

\[Clarity API Guide\](https://api-guide.clarity.io/getting-started/)

## Examples

``` r
library(AirSensor2)

# Start out with no keys (unless the user has set them)
showAPIKeys()
#>  list()
getAPIKey("PurpleAir-read")
#> NULL

# Set specific keys
setAPIKey("PurpleAir-read", "********-2A00-11EB-A8CD-42010A800126")
setAPIKey("PurpleAir-write", "********-2A00-11EB-A8CD-42010A800126")
showAPIKeys()
#> List of 2
#>  $ PurpleAir-read : chr "********-2A00-11EB-A8CD-42010A800126"
#>  $ PurpleAir-write: chr "********-2A00-11EB-A8CD-42010A800126"
getAPIKey("PurpleAir-read")
#> [1] "********-2A00-11EB-A8CD-42010A800126"

# Reset the read key
setAPIKey("PurpleAir-read", NULL)
showAPIKeys()
#> List of 1
#>  $ PurpleAir-write: chr "********-2A00-11EB-A8CD-42010A800126"
getAPIKey("PurpleAir-read")
#> NULL

# Reset the write key
setAPIKey("PurpleAir-write", NULL)
showAPIKeys()
#>  Named list()
```
