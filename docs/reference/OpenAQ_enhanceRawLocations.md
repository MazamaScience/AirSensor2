# Enhance OpenAQ locations data

Enhances raw location data returned by \[openaq::list_locations()\] to
create a standardized data frame compatible with MazamaLocationUtils and
AirSensor2 conventions.

## Usage

``` r
OpenAQ_enhanceRawLocations(
  locations_raw = NULL,
  countryCodes = NULL,
  stateCodes = NULL,
  counties = NULL
)
```

## Arguments

- locations_raw:

  Data frame returned by \[openaq::list_locations()\].

- countryCodes:

  Optional ISO 3166-1 alpha-2 country codes used to subset the data.

- stateCodes:

  Optional ISO 3166-2 alpha-2 state codes used to subset the data.

- counties:

  Optional U.S. county names or 5-digit FIPS codes used to subset the
  data.

## Value

A data frame of enhanced OpenAQ location data with standardized variable
names, spatial metadata (e.g., timezone, countryCode, stateCode,
countyName), and device metadata (e.g., sensorManufacturer, deviceID,
deviceDeploymentID, locationName).

## Details

This function renames variables for clarity, adds spatial and device
metadata, and optionally filters the data by country, state, or county.
The resulting data frame includes consistent identifiers and metadata
fields suitable for downstream analysis and mapping.
