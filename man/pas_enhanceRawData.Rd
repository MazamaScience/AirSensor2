% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pas_enhanceRawData.R
\name{pas_enhanceRawData}
\alias{pas_enhanceRawData}
\title{Enhance synoptic data from PurpleAir}
\usage{
pas_enhanceRawData(
  pas_raw = NULL,
  countryCodes = "US",
  stateCodes = NULL,
  counties = NULL
)
}
\arguments{
\item{pas_raw}{Dataframe returned by \link{pas_downloadParseRawData}.}

\item{countryCodes}{ISO 3166-1 alpha-2 country codes used to subset the data.}

\item{stateCodes}{ISO-3166-2 alpha-2 state codes used to subset the data.}

\item{counties}{US county names or 5-digit FIPS codes used to subset the data.}
}
\value{
Enhanced dataframe of synoptic PurpleAir data.
}
\description{
Enhance raw synoptic data from PurpleAir to create an improved
dataframe compatible with the \pkg{MazamaLocationUtils} package.

Steps include:

1) Replace variable names with more consistent, human readable names.

2) Add spatial metadata for each sensor including:
\itemize{
  \item{timezone -- Olson timezone}
  \item{countryCode -- ISO 3166-1 alpha-2}
  \item{stateCode -- ISO 3166-2 alpha-2}
}

3) Convert data types from character to \code{POSIXct} and \code{numeric}.

4) Add additional metadata items:
\itemize{
\item{sensorManufacturer = "PurpleAir"}
}

Limiting spatial searches by country can greatly speed up the process of
enhancement. This is performed by providing a vector of ISO country codes to
the \code{countryCodes} argument. By default, no subsetting is performed.

Users may further improve performance by also specifying \code{stateCodes}
when \code{countryCodes} is limited to a single country.

When a single US state is specified, named \code{counties} may be specified
to further speed up performance.
}
\references{
\href{https://www2.purpleair.com}{PurpleAir}

\href{https://api.purpleair.com/}{PurpleAir API}

\href{https://www2.purpleair.com/policies/terms-of-service}{PurpleAir Terms of service}

\href{https://www2.purpleair.com/pages/license}{PurpleAir Data license}

\href{https://www2.purpleair.com/pages/attribution}{PurpleAir Data Attribution}
}
\seealso{
\link{pas_downloadParseRawData}
}
