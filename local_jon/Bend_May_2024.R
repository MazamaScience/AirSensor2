
library(AirMonitor)
or <- airnow_loadDaily() %>% monitor_filter(stateCode == "OR")
or %>% monitor_leaflet()

# Bend Pine Ridge Elementary
BPR <- or %>% monitor_select("061b13e7b3c985d4_840410170116")

Bend <- or %>% monitor_filterByDistance(BPR$meta$longitude, BPR$meta$latitude, radius = 10000)
Bend %>% monitor_leaflet()

Bend %>%
  monitor_filterDate(20240515, 20240601) %>%
  monitor_timeseriesPlot(addAQI = TRUE, shadedNight = TRUE)

Bend %>%
  monitor_filterDate(20240529, 20240605) %>%
  monitor_timeseriesPlot(
    main = "Bend, Oregon (6 monitors)",
    addAQI = TRUE,
    shadedNight = TRUE
  )

addAQILegend("topright")

Bend %>%
  monitor_filterDate(20240529, 20240605) %>%
  monitor_collapse(deviceID = "Bend") %>%
  AirMonitorPlots::monitor_ggDailyHourlyBarplot(
    id = "9rcdx9hq44_Bend"
  )


