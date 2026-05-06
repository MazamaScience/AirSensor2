# Create an OpenAQ locations dataset

Downloads location metadata from OpenAQ and returns an enhanced data
frame suitable for analysis and mapping with MazamaLocationUtils.

## Usage

``` r
OpenAQ_createLocations(
  countryCodes = NULL,
  stateCodes = NULL,
  counties = NULL,
  lookbackDays = 1,
  providers = NULL,
  manufacturers = NULL,
  is_monitor = NULL,
  limit = 1000,
  api_key = NULL
)
```

## Arguments

- countryCodes:

  Optional ISO 3166-1 alpha-2 country codes used to subset the data.

- stateCodes:

  Optional ISO 3166-2 alpha-2 state codes used to subset the data.

- counties:

  Optional U.S. county names or 5-digit FIPS codes used to subset the
  data.

- lookbackDays:

  Number of days to look back for locations with recent data. Use
  \`lookbackDays = 0\` to include all historical locations.

- providers:

  Optional character vector of OpenAQ provider names or export prefixes
  used to subset the data.

- manufacturers:

  Optional character vector of manufacturer names used to subset the
  data.

- is_monitor:

  Optional logical used to filter results to regulatory monitors
  (\`TRUE\`) or air sensors (\`FALSE\`). If \`NULL\`, both are included.

- limit:

  Maximum number of locations to request per API call. Values greater
  than 1000 are reset to 1000.

- api_key:

  OpenAQ API read key. If \`NULL\`, the key is obtained with
  \`MazamaCoreUtils::getAPIKey("OPENAQ")\`.

## Value

A data frame of enhanced OpenAQ location metadata.

## Details

This function can filter locations by country, state, county, provider,
manufacturer, and monitor type. It can also limit results to locations
with recent data using \`lookbackDays\`. Returned data are enhanced with
standardized identifiers and spatial metadata such as timezone,
countryCode, and stateCode.

Valid values for \`countryCodes\`, \`providers\`, and \`manufacturers\`
can be explored with \[OpenAQ_getCountries()\],
\[OpenAQ_getProviders()\], and \[OpenAQ_getManufacturers()\].

## Examples

``` r
# \donttest{
try({
  if (interactive()) {
    initializeMazamaSpatialUtils()

    # NOTE:  Read environment vars from .env file with dotenv::load_dot_env()
    OPENAQ_API_KEY <- Sys.getenv("OPENAQ_API_KEY")

    locations <-
      OpenAQ_createLocations(
        countryCodes = "US",
        stateCodes = "IL",
        counties = "Cook",
        api_key = OPENAQ_API_KEY
      )

    table(locations$provider_name)

    clarity <- locations %>% dplyr::filter(provider_name == "Clarity")
    airnow <- locations %>% dplyr::filter(provider_name == "AirNow")
    airgradient <- locations %>% dplyr::filter(provider_name == "AirGradient")

    map <-
      MazamaLocationUtils::table_leaflet(
        clarity,
        extraVars = c("deviceDeploymentID", "start", "end", "owner_name", "provider_name"),
        radius = 5, fillColor = "blue"
      )

    map <- MazamaLocationUtils::table_leafletAdd(
      map,
      airnow,
      extraVars = c("deviceDeploymentID", "start", "end", "owner_name", "provider_name"),
      radius = 10, fillColor = "black"
    )

    map <- MazamaLocationUtils::table_leafletAdd(
      map,
      airgradient,
      extraVars = c("deviceDeploymentID", "start", "end", "owner_name", "provider_name"),
      radius = 5, fillColor = "red"
    )

    print(map)
  }
}, silent = FALSE)
# }
```
