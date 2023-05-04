library(AirSensor2)
library(AirMonitor)

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

syn <- Clarity_createOpenSynoptic(
  api_key = Clarity_API_READ_KEY,
  format = "USFS"
)

syn %>%
  dplyr::filter(countryCode == "US") %>%
  syn_leaflet(
    parameter = "pm2.5",
    extraVars = c("QCFlag", "nowcast")
  )

syn %>%
  dplyr::filter(countryCode == "US") %>%
  MazamaLocationUtils::table_leaflet(
    radius = 4
  )

# ===== Timeseries =============================================================

# AirNow monitor at St. Patrick's Elementary
# 4b9668fcd925c7da_840060374010
#
# 6 blocks away:
#
# Clarity sensor at Oxnard St. Elementary
# 9q5f6qn84q_clarity.DMRGM1663

ca <-
  Clarity_createOpenSynoptic(
    api_key = Clarity_API_READ_KEY,
    format = "USFS"
  ) %>%
  dplyr::filter(stateCode == "CA")

mon <-
  monitor_loadLatest() %>%
  monitor_select("4b9668fcd925c7da_840060374010") %>%
  monitor_nowcast()

a <- Clarity_getOpenHourly(
  api_key = Clarity_API_READ_KEY,
  datasourceId = "DMRGM1663",
  format = "USFS"
)

plot(a$datetime, a$nowcast, ylab = "ug/m3", xlab = "UTC")
points(mon$data[,1], mon$data[,2], type = 'l', col = 'red')
legend('topright', legend = c("Clarity", "AirNow"), fill = c('black', 'red'), border = 'transparent')
title("North Hollywood Elementary Schools -- 6 blocks apart")

# ===== PurpleAir & Clarity on one map =========================================

pas <- pas_createNew(
  api_key = PurpleAir_API_READ_KEY,
  countryCodes = "US",
  stateCodes = "CA",
  counties = "Los Angeles"
)

map <- MazamaLocationUtils::table_leaflet(pas, extraVars = "pm2.5_60minute", radius = 4)

MazamaLocationUtils::table_leafletAdd(map, syn, extraVars="pm2.5", radius = 4, color = 'red')

