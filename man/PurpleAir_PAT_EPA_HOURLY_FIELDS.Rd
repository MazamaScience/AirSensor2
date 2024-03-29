% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PurpleAir_API.R
\docType{data}
\name{PurpleAir_PAT_EPA_HOURLY_FIELDS}
\alias{PurpleAir_PAT_EPA_HOURLY_FIELDS}
\title{Comma-separated list of fields needed for EPA correction}
\format{
String with comma-separated field names
}
\usage{
PurpleAir_PAT_EPA_HOURLY_FIELDS
}
\description{
Character string with a minimal set of PurpleAir field names
used in \code{PurpleAir_createNewMonitor()}. These fields are sufficient to
apply the EPA correction algorithm used in the AirNow
\href{https://fire.airnow.gov}{Fire & Smoke Map}.

Included fields:
\preformatted{
[1] "humidity"     "temperature"  "pm2.5_cf_1"   "pm2.5_cf_1_a"
[5] "pm2.5_cf_1_b"
}
}
\references{
\href{https://www.epa.gov/sites/default/files/2021-05/documents/toolsresourceswebinar_purpleairsmoke_210519b.pdf}{EPA PurpleAir Correction}.

\href{https://api.purpleair.com/#api-sensors-get-sensor-history-csv}{Get Sensor History API}
}
\seealso{
\link{pat_applyCorrection}
}
\keyword{datasets}
