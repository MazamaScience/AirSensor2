library(AirSensor2)

source("global_vars.R")

logger.setup()
logger.setLevel(TRACE)

initializeMazamaSpatialUtils()


show_only <-
  paste0(
    "1748,1860,1914,1922,2018,2020,2025,2169,2303,2307,2323,2336,2352,2356,2360,",
    "2372,2380,2452,2496,2504,2693,2713,2725,3487,3515,4666,4670,4708,4714,5218,",
    "6836,9180,9196,9206,9208,9214,9276,9314,9336,9340,9352,9364,9376,9382,9386,",
    "9390,9392,9398,9472,9500,12216,143602,22727,23241,23245,23253,23263,23291,",
    "23429,160633,69773,90253,90263,91095,104894,104900,104916,104926,104930,",
    "104932,104942"
  )

pas_SCAQMD <-
  pas_createNew(
    api_key = MY_API_READ_KEY,
    countryCodes = "US",
    stateCodes = "CA",
    lookbackDays = 1,
    location_type = 0,
    show_only = show_only
  )


pat_raw <-
  pat_downloadParseRawData(
    api_key = MY_API_READ_KEY,
    sensor_index = "2323",
    startdate = "2023-02-01",
    enddate = "2023-02-03",
    timezone = "UTC"
  )


# ===== OTHER STUFF ============================================================

if ( FALSE ) {

  pas_CA <-
    pas_createNew(
      api_key = MY_API_READ_KEY,
      countryCodes = "US",
      stateCodes = "CA",
      lookbackDays = 1,
      location_type = 0
    )

  pattern = "^[Ss][Cc].._..$"
  SouthCoast <- dplyr::filter(pas, stringr::str_detect(name, pattern))

  pas_leaflet(SouthCoast)

  SCAQMD_sensor_indices <-
    paste0('"', paste0(SouthCoast$sensor_index, collapse = ","), '"')

}
