% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pat_applyCorrection.R
\name{pat_applyCorrection}
\alias{pat_applyCorrection}
\title{Apply correction to PurpleAir PM2.5 data.}
\usage{
pat_applyCorrection(pat = NULL, name = c("EPA_FASM"))
}
\arguments{
\item{pat}{Previously generated \emph{houly pat} object.}

\item{name}{Name of correction to apply. One of \code{"EPA_FASM"}}
}
\value{
A PurpleAir Timeseries \emph{pat} object with an additional
\code{"pm2.5_corrected"} variable.
}
\description{
A correction equation is applied to fields of the incoming
\emph{pat} object to generate a \code{"pm2.5_corrected"} time series which is
added to the returned \emph{pat} object.

The default, \code{"EPA_FASM"} correction is described on slide 37 of a
presentation on the correction of PurpleAir data for the
\href{https://www.epa.gov/sites/default/files/2021-05/documents/toolsresourceswebinar_purpleairsmoke_210519b.pdf}{EPA Fire and Smoke Map}.

This correction has two parts:

  Low Concentration (\eqn{pm2.5\_cf\_1 <= 343 \mu g/m^3}):

  \eqn{pm2.5\_corrected = 0.52 * pm\_2.5\_cf\_1 - 0.086 * humidity + 5.75}

High Concentration (\eqn{pm2.5\_cf\_1 > 343 \mu g/m^3}):

  \eqn{pm2.5\_corrected = 0.46 * pm2.5\_cf\_1 + 3.93 * 10^{-4} * pm2.5\_cf\_1^2 + 2.97}
}
