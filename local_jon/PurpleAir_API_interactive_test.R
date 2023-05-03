library(AirSensor2)

source("global_vars.R")

# ===== Keys ===================================================================

PurpleAir_checkAPIKey(
  api_key = PurpleAir_API_READ_KEY
)

# ===== Sensors ================================================================

start <-
  MazamaCoreUtils::parseDatetime("2022-01-29 00:00:00", timezone = "UTC") %>%
  as.numeric()

end <-
  MazamaCoreUtils::parseDatetime("2022-01-30 00:00:00", timezone = "UTC") %>%
  as.numeric()

a <- PurpleAir_getSensorData(
  api_key = PurpleAir_API_READ_KEY,
  sensor_index = 896
)

a <- PurpleAir_getSensorHistoryCSV(
  api_key = PurpleAir_API_READ_KEY,
  sensor_index = 896,
  start_timestamp = start,
  end_timestamp = end,
  average = 0
)

a <- PurpleAir_getSensorHistory(
  api_key = PurpleAir_API_READ_KEY,
  sensor_index = 896,
  start_timestamp = start,
  end_timestamp = end,
  average = 0,
  fields = PurpleAir_HISTORY_PM25_FIELDS
)

a <- PurpleAir_getSensorsData(
  api_key = PurpleAir_API_READ_KEY,
  location_type = 0,
  max_age = 3600 * 3,
  nwlng = -125,
  nwlat = 49,
  selng = -117,
  selat = 42
)

# ===== Groups =================================================================

PurpleAir_createGroup(
  api_key = PurpleAir_API_WRITE_KEY,
  name = "Ballard"
)

PurpleAir_createMember(
  api_key = PurpleAir_API_WRITE_KEY,
  sensor_index = NULL
)

PurpleAir_deleteGroup(
  api_key = PurpleAir_API_WRITE_KEY,
  group_id = 1652
)

PurpleAir_deleteMember(
  api_key = PurpleAir_API_WRITE_KEY,
  group_id = 505,
  member_id = 3855
)

PurpleAir_getGroupsList(
  api_key = PurpleAir_API_READ_KEY
)

PurpleAir_getGroupDetail(
  api_key = PurpleAir_API_READ_KEY,
  group_id = 505
)

a <- PurpleAir_getMemberData(
  api_key = PurpleAir_API_READ_KEY,
  group_id = 505,
  member_id = 3856
)

a <- PurpleAir_getMemberHistory(
  api_key = PurpleAir_API_READ_KEY,
  group_id = 505,
  member_id = 3856,
  average = 0
)

a <- PurpleAir_getMemberHistory(
  api_key = PurpleAir_API_READ_KEY,
  group_id = 505,
  member_id = 3856,
  start_timestamp = start,
  end_timestamp = end,
  average = 0
)

a <- PurpleAir_getMembersData(
  api_key = PurpleAir_API_READ_KEY,
  group_id = 505
)

