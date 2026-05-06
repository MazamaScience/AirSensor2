# Download raw OpenAQ measurements

Downloads raw measurements from OpenAQ for a single location and returns
a wide data frame with one column per requested parameter.

## Usage

``` r
OpenAQ_downloadRawData(
  locations_id = NULL,
  parameters = c("pm25", "temperature", "relativehumidity"),
  data = c("hours", "measurements"),
  startdate = NULL,
  enddate = NULL,
  limit = 1000,
  maxPages = 10,
  sleepSeconds = 0.2,
  api_key = NULL
)
```

## Arguments

- locations_id:

  OpenAQ location ID.

- parameters:

  Specific parameters for which to download data.

- data:

  Character string specifying the data interval to return.

- startdate:

  Desired start datetime as POSIXct.

- enddate:

  Desired end datetime as POSIXct.

- limit:

  Maximum number of records to request per page.

- maxPages:

  Maximum number of pages to request automatically.

- sleepSeconds:

  Number of seconds to pause between additional requests.

- api_key:

  OpenAQ API key.

## Value

A data frame with columns \`datetime\` plus one column for each
requested parameter.

## Details

For each requested parameter, this function identifies matching sensors
at the requested location, downloads all available pages of measurements
up to \`maxPages\`, stacks measurements across sensors, sorts by
\`datetime\`, and deduplicates by keeping the record from the most
recently active sensor. The final data frame is created by joining
parameter-specific measurement tables by \`datetime\`.

## Examples

``` r
# \donttest{
try({
  if (interactive()) {
    initializeMazamaSpatialUtils()

    # NOTE:  Read environment vars from .env file with dotenv::load_dot_env()
    OPENAQ_API_KEY <- Sys.getenv("OPENAQ_API_KEY")

    raw_data <-
      OpenAQ_downloadRawData(
        locations_id = 1370216,
        startdate = "2026-04-01",
        enddate = "2026-04-15",
        api_key = OPENAQ_API_KEY
      )

    raw_data %>% plot()

  }
}, silent = FALSE)
# }
```
