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

countries <- openaq::list_countries()

# Thailand id is 111

locations <- openaq::list_locations(iso = "TH")

# > dplyr::glimpse(locations, width = 75)
# Rows: 100
# Columns: 15
# $ id             <int> 49, 57, 66, 94, 135, 138, 310, 418, 619, 701, 717,…
# $ name           <chr> "Phahol Yothin Rd., Khet Chatuchak", "Khlong Nueng…
# $ is_mobile      <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, F…
# $ is_monitor     <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TR…
# $ timezone       <fct> Asia/Bangkok, Asia/Bangkok, Asia/Bangkok, Asia/Ban…
# $ countries_id   <dbl> 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, …
# $ country_name   <chr> "Thailand", "Thailand", "Thailand", "Thailand", "T…
# $ country_iso    <fct> TH, TH, TH, TH, TH, TH, TH, TH, TH, TH, TH, TH, TH…
# $ latitude       <dbl> 13.79786, 14.04030, 13.68423, 13.77537, 13.66409, …
# $ longitude      <dbl> 100.55134, 100.60874, 100.44599, 100.56893, 100.54…
# $ datetime_first <dbl> NA, 1472454000, NA, 1541484000, 1664910000, 145411…
# $ datetime_last  <dbl> NA, 1589616000, NA, 1600153200, 1770148800, 160083…
# $ owner_name     <fct> Unknown Governmental Organization, Unknown Governm…
# $ providers_id   <dbl> 281, 281, 281, 281, 118, 281, 281, 118, 281, 118, …
# $ provider_name  <chr> "Thailand", "Thailand", "Thailand", "Thailand", "A…


locations <-
  locations %>%
  dplyr::mutate(
    datetime_first = lubridate::as_datetime(datetime_first),
    datetime_last = lubridate::as_datetime(datetime_last),
  )

MazamaLocationUtils::table_leaflet(
  locations,
  extraVars = c("id", "name", "datetime_first", "datetime_last")
)



# locations_list <- list_locations(
#   parameters_id = 2,
#   countries_id = 182,
#   as_data_frame = FALSE
# )[[1]]
#
#
# locations_df <- list_locations(
#   parameters_id = 2,
#   countries_id = 182,
#   as_data_frame = TRUE
# )


datetime_from <- MazamaCoreUtils::parseDatetime("2019-01-01", timezone = "UTC")
datetime_to <- MazamaCoreUtils::parseDatetime("2019-09-23", timezone = "UTC")

# DOESN'T WORK
#
# data <-
#   list_location_measurements(
#     locations_id = 7843,
#     datetime_from = datetime_from,
#     datetime_to = datetime_to
#   )

# Best to get sensors for a location

sensors <-
  list_location_sensors(
    locations_id = 7843
  )

sensor_id <- 22752

data_df <-
  list_sensor_measurements(
    sensors_id = 22752,
    datetime_from = datetime_from,
    datetime_to = datetime_to,
    limit = 1000
  )

# Looks like one sensor per paramter_id/name

# Function to take list of location IDs and
# - query list_location_sensors() for each location
# - create a combined DF, adding info from location
#
# With this we can filter down to the sensor ids we want.


# Function to
# - pull together an extended timeseries for a single sensor id
# - put on regular time axis
# - validate (hourly) measurements
# - remove all unused columns


locations <-
  openaq::list_locations(
    iso = "TH",
    parameters_id = 2,
    monitor = FALSE # sensors only
  ) %>%
  dplyr::mutate(
    datetime_first = lubridate::as_datetime(datetime_first),
    datetime_last = lubridate::as_datetime(datetime_last),
  )

MazamaLocationUtils::table_leaflet(
  locations,
  extraVars = c("id", "name", "datetime_first", "datetime_last")
)

list_location_sensors(
  locations_id = 2523394
)


data_a <-
  list_sensor_measurements(
    sensors_id = 7974127,
    datetime_from =  MazamaCoreUtils::parseDatetime("2026-01-01", timezone = "UTC"),
    datetime_to = MazamaCoreUtils::parseDatetime("2026-01-08", timezone = "UTC"),
    limit = 1000
  )

# Nice view of diurnal variation in a site in Thailand!

