% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\encoding{UTF-8}
\name{example_pas_pm25_raw}
\alias{example_pas_pm25_raw}
\title{Example raw PurpleAir Synoptic dataset}
\format{
A tibble with 39 rows and 18 columns of data.
}
\source{
https://www2.purpleair.com
}
\usage{
example_pas_pm25_raw
}
\description{
The \code{example_pas_raw} dataset provides a quickly loadable
version of raw PurpleAir synoptic data for practicing and code examples
This dataset contains data for sensors in a box around the Methow Valley
in Washington state and was generated on 2024-03-12 by running:

\preformatted{
library(AirSensor2)

initializeMazamaSpatialUtils()

source("global_vars.R") # contains PurpleAir_API_READ_KEY

example_pas_pm25_raw <-
  pas_downloadParseRawData(
    PurpleAir_API_READ_KEY,
    fields = PurpleAir_PAS_AVG_PM25_FIELDS,
    location_type = 0,
    modified_since = NULL,
    max_age = 3600 * 24,
    west = -120.5,
    east = -120,
    south = 48.2,
    north = 48.7
  )

save(example_pas_pm25_raw, file = "data/example_pas_pm25_raw.rda")
}

This dataset can be converted into a standard \emph{pas} object with:

\preformatted{
pas <-
  pas_enhanceRawData(
    example_pas_pm25_raw,
    countryCodes = "US",
    stateCodes = "WA"
  )
}
}
\seealso{
example_pas_pm25
}
\keyword{datasets}
