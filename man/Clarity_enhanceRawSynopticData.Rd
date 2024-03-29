% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Clarity_enhanceRawSynopticData.R
\name{Clarity_enhanceRawSynopticData}
\alias{Clarity_enhanceRawSynopticData}
\title{Enhance synoptic data from Clarity}
\usage{
Clarity_enhanceRawSynopticData(rawSynoptic = NULL)
}
\arguments{
\item{rawSynoptic}{'synoptic' dataframe returned by \code{Clarity_getAllOpenHourly()}.}
}
\value{
Enhanced dataframe of synoptic Clarity data.
}
\description{
Enhance raw synoptic data from Clarity to create an improved
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
\item{sensorManufacturer = "Clarity"}
}
}
\seealso{
\link{Clarity_getAllOpenHourly}
}
