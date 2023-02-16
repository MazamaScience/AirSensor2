library(AirSensor2)

source("global_vars.R")

pa_checkAPIKey(
  api_key = MY_API_READ_KEY
)

pa_getGroupsList(
  api_key = MY_API_READ_KEY
)

pa_getGroupDetail(
  api_key = MY_API_READ_KEY,
  group_id = 505
)

a <- pa_getMembersData(
  api_key = MY_API_READ_KEY,
  group_id = 505
)

