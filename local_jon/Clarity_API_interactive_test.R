library(AirSensor2)

source("global_vars.R")

# ===== All ===================================================================

a <- Clarity_getAllOpenHourly(
  api_key = Clarity_API_READ_KEY,
  format = "USFS"
)

a <- Clarity_getAllOpenIndividual(
  api_key = Clarity_API_READ_KEY,
  format = "USFS"
)

# ===== Synoptic ===============================================================

syn <- Clarity_createSynoptic(
  api_key = Clarity_API_READ_KEY,
  format = "USFS"
)


