#
# NOTE:  To watch expenditures, log in at https://develop.purpleair.com
# NOTE:  Detailed pricing information at https://develop.purpleair.com/pricing

library(AirSensor2)

source("global_vars.R")

logger.setup()
logger.setLevel(TRACE)

initializeMazamaSpatialUtils()

sensor_index <- "117239"

enddate <- lubridate::now(tzone = "UTC")
startdate <- enddate - lubridate::ddays(3)

# ----- PAS (full) -------------------------------------------------------------

pas <-
  pas_createNew(
    api_key = PurpleAir_API_READ_KEY,
    show_only = sensor_index,
    countryCodes = "US"
  )

# Cost: 5 + 36 = 41

# ----- PAT (full) -------------------------------------------------------------

# Create raw pat object
pat <-
  pat_createNew(
    api_key = PurpleAir_API_READ_KEY,
    pas = pas,
    sensor_index = sensor_index,
    startdate = startdate,
    enddate = enddate,
    timezone = "UTC",
    verbose = TRUE
  )

tbl <- pat$data

# Cost: ? + 2035 rows x 17 columns = 34,595

# ----- Plot -------------------------------------------------------------------


## Sensor Electronics

#plot(tbl[,c(1,2:5)], pch = 15, cex = 0.5, main = "Sensor Electronics")
layout(matrix(seq(4)))
plot(tbl[,c(1,3)], pch = 15, cex = 0.5, main = "uptime")
plot(tbl[,c(1,5)], pch = 15, cex = 0.5, main = "memory")
plot(tbl[,c(1,2)], pch = 15, cex = 0.5, main = "wifi strength")
plot(tbl[,c(1,4)], pch = 15, cex = 0.5, main = "latency")
layout(1)

## Atmospheric Variables

#plot(tbl[,c(1,6:8)], pch = 15, cex = 0.5, main = "Atmospheric Variables")
layout(matrix(seq(3)))
plot(tbl[,c(1,6)], pch = 15, cex = 0.5, main = "humidity")
plot(tbl[,c(1,7)], pch = 15, cex = 0.5, main = "temperature")
plot(tbl[,c(1,8)], pch = 15, cex = 0.5, main = "pressure")
layout(1)

## PM2.5 "atm"

#plot(tbl[,c(1,12:14)], pch = 15, cex = 0.5, main = "PM2.5 'atm'")
plot(tbl[,c(1,12)], pch = 15, cex = 0.5, col = "transparent", main = "pm2.5_atm")
points(tbl[,c(1,13)], pch = 15, cex = 0.5, col = adjustcolor("blue", 0.3))
points(tbl[,c(1,14)], pch = 15, cex = 0.5, col = adjustcolor("red", 0.3))

## PM2.5 AB Correlation

correlation <- cor(tbl$pm2.5_atm_a, tbl$pm2.5_atm_b) %>% round(3)
title <- sprintf("A/B Channel Correlation = %0.3f", correlation)
plot(tbl[,c(13:14)], pch = 15, cex = 0.5, main = title)




