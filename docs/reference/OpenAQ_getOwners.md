# Get OpenAQ owner metadata

Returns OpenAQ owner metadata as a tibble. Data are downloaded only once
per R session and then cached for re-use.

## Usage

``` r
OpenAQ_getOwners()
```

## Value

A tibble of OpenAQ owner metadata.

## Details

This function requires the optional package \`openaq\`.

For more information about the OpenAQ data access API, see:
<https://docs.openaq.org/about/about>
