# Download raw OpenAQ location data

Downloads raw location metadata from OpenAQ and optionally retrieves
additional pages when many matching locations are found.

## Usage

``` r
OpenAQ_downloadRawLocations(
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
)
```

## Arguments

- bbox:

  Bounding box passed to \[openaq::list_locations()\].

- providers_id:

  Provider IDs passed to \[openaq::list_locations()\].

- manufacturers_id:

  Manufacturer IDs passed to \[openaq::list_locations()\].

- monitor:

  Logical value passed to \[openaq::list_locations()\].

- mobile:

  Logical value passed to \[openaq::list_locations()\].

- countries_id:

  Country IDs passed to \[openaq::list_locations()\].

- limit:

  Maximum number of records to request per page.

- maxPages:

  Maximum number of pages to request automatically.

- sleepSeconds:

  Number of seconds to pause between additional requests.

- api_key:

  OpenAQ API key.

## Value

A dataframe of raw OpenAQ location metadata.

## Details

When more matching locations are available than can be returned in a
single request, this function can automatically request additional pages
up to \`maxPages\`. If more pages may still be available after
\`maxPages\` pages have been retrieved, a warning is issued suggesting
that the user narrow the request.
