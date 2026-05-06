# Initialize MazamaSpatialUtils

Convenience function that loads the spatial datasets needed by any of
the "enhanceRaw" functions.

This function wraps the equivalent sequence:


      data("SimpleCountriesEEZ", package = "MazamaSpatialUtils")
      data("SimpleTimezones", package = "MazamaSpatialUtils")

      MazamaSpatialUtils::setSpatialDataDir("~/Data/Spatial")
      MazamaSpatialUtils::loadSpatialData("NaturalEarthAdm1.rda")
      MazamaSpatialUtils::loadSpatialData("USCensusCounties.rda")

It should be run before using \`pas_load()\`, because \`pas_load()\`
uses the spatial data loaded here to enhance raw synoptic data via
\`pas_enhanceData()\`.

If file logging is desired, the individual logging commands should be
run separately with output files specified in
\`MazamaCoreUtils::logger.setup()\`.

## Usage

``` r
initializeMazamaSpatialUtils(
  spatialDataDir = "~/Data/Spatial",
  stateCodeDataset = "NaturalEarthAdm1.rda",
  USCountiesDataset = "USCensusCounties.rda",
  logLevel = WARN
)
```

## Arguments

- spatialDataDir:

  Directory containing external MazamaSpatialUtils spatial datasets.

- stateCodeDataset:

  Name of the MazamaSpatialUtils dataset containing state or
  administrative-level boundaries and codes.

- USCountiesDataset:

  Name of the MazamaSpatialUtils dataset containing US county names.

- logLevel:

  Logging level to use if logging has not already been initialized.

## Value

Invisible \`NULL\`.
