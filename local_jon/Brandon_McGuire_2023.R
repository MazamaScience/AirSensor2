library(AirSensor2)

source("global_vars.R")

logger.setup()
logger.setLevel(TRACE)

initializeMazamaSpatialUtils()

pas <-
  pas_createNew(
    api_key = PurpleAir_API_READ_KEY,
    show_only = '150470',
    countryCodes = "US"
  )
