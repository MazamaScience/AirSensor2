# Trying out different R packages:

# ------------------------------------------------------------------------------
# openaq-r

###install.packages("pak")

###pak::pkg_install("openaq/openaq-r@*release")

# Register for an account at: https://explore.openaq.org/register
#
# - Need to request an API key (no email sent)
# - Then try to log in and use "forgot password" (email sent)
# - Change password and then log in; go to user settings to see api key

library(dplyr)

library(dotenv)
dotenv::load_dot_env()

Sys.getenv("OPENAQ_API_KEY")

library(openaq)
openaq::set_api_key(Sys.getenv("OPENAQ_API_KEY"))

# Get reference dataframes
instruments <- openaq::list_instruments(limit = 1000)
licenses <- openaq::list_licenses(limit = 1000)
manufacturers <- openaq::list_manufacturers(limit = 1000)
parameters <- openaq::list_parameters(limit = 1000)
providers <- openaq::list_providers(limit = 1000)

manufacturers_id <-
  manufacturers %>%
  dplyr::filter(name == "AirGradient") %>%
  dplyr::pull(id)

providers_id <-
  providers %>%
  dplyr::filter(name == "AirGradient") %>%
  dplyr::pull(id)

locations <-
  openaq::list_locations(
    providers_id = providers_id,
    manufacturers_id = NULL,
    monitor = FALSE,
    mobile = FALSE,
    instruments_id = NULL,
    iso = "TH",
    limit = 1000
  )

MazamaLocationUtils::table_leaflet(
  locations,
  extraVars = c("id", "name", "datetime_first", "datetime_last")
)

locations_id <- 1236022

#
sensors <- openaq::list_location_sensors(locations_id)

starttime <- MazamaCoreUtils::parseDatetime("2026-04-01 00:00:00", timezone = "UTC")
endtime <- MazamaCoreUtils::parseDatetime("2026-04-08 00:00:00", timezone = "UTC")

pm25_sensors_id <- sensors %>% dplyr::filter(parameters_id == 2) %>% dplyr::pull(id)
rel_hum_sensors_id <- sensors %>% dplyr::filter(parameters_id == 98) %>% dplyr::pull(id)
temp_sensors_id <- sensors %>% dplyr::filter(parameters_id == 100) %>% dplyr::pull(id)

pm25_df <-
  list_sensor_measurements(
    pm25_sensors_id,
    data = "hours",
    datetime_from = starttime,
    datetime_to = endtime,
    limit = 1000
  )

rel_hum_df <-
  list_sensor_measurements(
    rel_hum_sensors_id,
    data = "hours",
    datetime_from = starttime,
    datetime_to = endtime,
    limit = 1000
  )

temp_df <-
  list_sensor_measurements(
    temp_sensors_id,
    data = "hours",
    datetime_from = starttime,
    datetime_to = endtime,
    limit = 1000
  )

pm25 <- pm25_df %>% dplyr::select(value, datetime_to) %>% dplyr::rename(pm25 = value, datetime = datetime_to)
rel_hum <- rel_hum_df %>% dplyr::select(value, datetime_to) %>% dplyr::rename(rel_hum = value, datetime = datetime_to)
temp <- temp_df %>% dplyr::select(value, datetime_to) %>% dplyr::rename(temp = value, datetime = datetime_to)

df <-
  pm25 %>%
  dplyr::full_join(rel_hum, by = "datetime") %>%
  dplyr::full_join(temp, by = "datetime") %>%
  dplyr::select(datetime, pm25, temp, rel_hum) %>%
  as_tibble()


location_meta <- locations %>% dplyr::filter(id == locations_id)




