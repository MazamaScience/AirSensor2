# Update a Clarity 'mts_monitor' object

Download, parse and enhance the last 3 hours of data from all Clarity
"open" sensors and create a new Clarity \*mts_monitor\* object for use
with the AirMonitor package.

## Usage

``` r
Clarity_createAllOpenMonitors(
  api_key = NULL,
  format = c("USFS2", "USFS"),
  parameter = c("pm2.5", "nowcast"),
  applyQC = TRUE,
  countryCodes = c("CA", "US", "MX")
)
```

## Arguments

- api_key:

  Clarity API READ Key. If \`api_key = NULL\`, it will be obtained using
  \`getAPIKey("Clarity-read")\`.

- format:

  Customized output format ("USFS2", "USFS").

- parameter:

  Parameter to use for data ("pm2.5" or "nowcast")

- applyQC:

  Logical specifying whether to use the Clarity QCFlag to invalidate
  data values.

- countryCodes:

  Vector of ISO 3166-1 alpha-2 country codes used to filter available
  data.

## Value

An AirMonitor package \*mts_monitor\* object.
