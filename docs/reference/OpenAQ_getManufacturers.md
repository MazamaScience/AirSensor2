# Get OpenAQ manufacturer metadata

Returns OpenAQ manufacturer metadata as a tibble. Data are downloaded
only once per R session and then cached for re-use.

## Usage

``` r
OpenAQ_getManufacturers()
```

## Value

A tibble of OpenAQ manufacturer metadata.

## Details

This function requires the optional package \`openaq\`.

For more information about the OpenAQ data access API, see:
<https://docs.openaq.org/about/about>
