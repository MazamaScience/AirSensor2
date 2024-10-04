# Tests while adding support for 'format=USFS2'

library(AirSensor2)

source("global_vars.R")

api_key <- Clarity_API_READ_KEY
baseUrl <- "https://clarity-data-api.clarity.io/v1/open/all-recent-measurement/pm25/hourly"
format <- "USFS2"

l <-
  Clarity_getAllOpenHourly(
    api_key = Clarity_API_READ_KEY,
    format = "USFS"
  )

dplyr::glimpse(l$synoptic)



l <-
  Clarity_getAllOpenIndividual(
    api_key = Clarity_API_READ_KEY,
    format = "USFS"
  )

l[3]

df <-
  Clarity_getOpenHourly(
    api_key = Clarity_API_READ_KEY,
    datasourceId = "DAABL1560",
    format = "USFS2"
  )

dplyr::glimpse(df)


df <-
  Clarity_getOpenIndividual(
    api_key = Clarity_API_READ_KEY,
    datasourceId = "DAABL1560",
    format = "USFS2"
  )

dplyr::glimpse(df)



