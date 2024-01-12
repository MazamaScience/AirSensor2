#
# NOTE:  To watch expenditures, log in at https://develop.purpleair.com
# NOTE:  Detailed pricing information at https://develop.purpleair.com/pricing

library(AirSensor2)

source("global_vars.R")

logger.setup()
logger.setLevel(TRACE)

initializeMazamaSpatialUtils()

pas <-
  pas_createNew(
    api_key = PurpleAir_API_READ_KEY,
    show_only = '117239',
    countryCodes = "US"
  )

# Cost:
