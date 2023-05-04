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

syn %>%
  dplyr::filter(countryCode == "US") %>%
  syn_leaflet(
    parameter = "pm2.5",
    extraVars = c("QCFlag", "nowcast")
  )

# ===== PurpleAir & Clarity on one map =========================================

pas <- pas_createNew(
  api_key = PurpleAir_API_READ_KEY,
  countryCodes = "US",
  stateCodes = "CA",
  counties = "Los Angeles"
)

map <- MazamaLocationUtils::table_leaflet(pas, extraVars = "pm2.5_60minute")

MazamaLocationUtils::table_leafletAdd(map, syn, extraVars="pm2.5", color = 'red')

