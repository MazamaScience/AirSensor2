library(AirSensor2)

source("global_vars.R")

pa_checkAPIKey(
  api_key = MY_API_READ_KEY
)

a <- pat_getSensorData(
  api_key = MY_API_READ_KEY,
  sensor_index = 896
)

a <- pat_getSensorsData(
  api_key = MY_API_READ_KEY,
  location_type = 0,
  max_age = 3600 * 3,
  nwlng = -125,
  nwlat = 49,
  selng = -117,
  selat = 42
)

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

a <- pa_getMembersData(
  api_key = MY_API_READ_KEY,
  group_id = 505
)

