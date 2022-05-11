# Examples for the ASIC conference

library(AirSensor2)

initializeMazamaSpatialUtils("~/Data/Spatial")

setAPIKey("PurpleAir-read", "8C087B59-2A00-11EB-A8CD-42010A800126")

Colville_pas <-
  pas_createNew(
    countryCodes = "US",
    stateCodes = "WA",
    counties = c("Okanogan", "Ferry"),
    lookbackDays = 1,
    outsideOnly = TRUE
  )

Colville_pas %>% pas_leaflet("temperature")

# ===== DEBUG ==================================================================

if ( FALSE ) {


}


