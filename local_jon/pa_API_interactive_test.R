library(AirSensor2)

source("global_vars.R")

# ===== Keys ===================================================================

pa_checkAPIKey(
  api_key = MY_API_READ_KEY
)

# ===== Sensors ================================================================

start <-
  MazamaCoreUtils::parseDatetime("2022-01-29 00:00:00", timezone = "UTC") %>%
  as.numeric()

end <-
  MazamaCoreUtils::parseDatetime("2022-01-30 00:00:00", timezone = "UTC") %>%
  as.numeric()

a <- pa_getSensorData(
  api_key = MY_API_READ_KEY,
  sensor_index = 896
)

a <- pa_getSensorHistoryCSV(
  api_key = MY_API_READ_KEY,
  sensor_index = 896,
  start_timestamp = start,
  end_timestamp = end,
  average = 0
)

a <- pa_getSensorHistory(
  api_key = MY_API_READ_KEY,
  sensor_index = 896,
  start_timestamp = start,
  end_timestamp = end,
  average = 0,
  fields = SENSOR_HISTORY_PM25_FIELDS
)

a <- pa_getSensorsData(
  api_key = MY_API_READ_KEY,
  location_type = 0,
  max_age = 3600 * 3,
  nwlng = -125,
  nwlat = 49,
  selng = -117,
  selat = 42
)

# ===== Groups =================================================================

pa_createGroup(
  api_key = MY_API_WRITE_KEY,
  name = "Ballard"
)

pa_createMember(
  api_key = MY_API_WRITE_KEY,
  sensor_index = NULL
)

pa_deleteGroup(
  api_key = MY_API_WRITE_KEY,
  group_id = 1652
)

pa_deleteMember(
  api_key = MY_API_WRITE_KEY,
  group_id = 505,
  member_id = 3855
)

pa_getGroupsList(
  api_key = MY_API_READ_KEY
)

pa_getGroupDetail(
  api_key = MY_API_READ_KEY,
  group_id = 505
)

a <- pa_getMemberData(
  api_key = MY_API_READ_KEY,
  group_id = 505,
  member_id = 3856
)

a <- pa_getMemberHistory(
  api_key = MY_API_READ_KEY,
  group_id = 505,
  member_id = 3856,
  average = 0
)

a <- pa_getMemberHistory(
  api_key = MY_API_READ_KEY,
  group_id = 505,
  member_id = 3856,
  start_timestamp = start,
  end_timestamp = end,
  average = 0
)

a <- pa_getMembersData(
  api_key = MY_API_READ_KEY,
  group_id = 505
)

