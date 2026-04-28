# Trying out different R packages:

# ------------------------------------------------------------------------------
# openair

###install.packages(c("openair", "worldmet", "openairmaps"))

library(dplyr)
library(openair)
library(openairmaps)

m <- openair::importMeta(source = "all", all = TRUE)
# took a few seconds

# > dplyr::glimpse(m, width = 75)
# Rows: 19,029
# Columns: 34
# $ source            <chr> "aurn", "aurn", "aurn", "aurn", "aurn", "aurn",…
# $ code              <chr> "ABD", "ABD", "ABD", "ABD", "ABD", "ABD", "ABD"…
# $ site              <chr> "Aberdeen", "Aberdeen", "Aberdeen", "Aberdeen",…
# $ site_type         <chr> "Urban Background", "Urban Background", "Urban …
# $ latitude          <dbl> 57.15736, 57.15736, 57.15736, 57.15736, 57.1573…
# $ longitude         <dbl> -2.094278, -2.094278, -2.094278, -2.094278, -2.…
# $ variable          <chr> "O3", "NO", "NO2", "NOx", "SO2", "CO", "PM10", …
# $ Parameter_name    <chr> "Ozone", "Nitric oxide", "Nitrogen dioxide", "N…
# $ start_date        <dttm> 2003-08-01, 1999-09-18, 1999-09-18, 1999-09-18…
# $ end_date          <chr> "2021-09-20", "2021-09-20", "2021-09-20", "2021…
# $ ratified_to       <dttm> 2021-09-20, 2021-09-20, 2021-09-20, 2021-09-20…
# $ zone              <chr> "North East Scotland", "North East Scotland", "…
# $ agglomeration     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
# $ local_authority   <chr> "Aberdeen City", "Aberdeen City", "Aberdeen Cit…
# $ ... (mostly NA)


MazamaLocationUtils::table_leaflet(
  m,
  extraVars = c(
    "source", "site", "code", "site_type",
    "Parameter_name", "start_date", "end_date"
    ),
  jitter = 0
)

# Site in Reunion
d <-
  openair::importEurope(
    site = "fr38005",
    year = 2008,
    meta = TRUE
  )

# > dplyr::glimpse(d, width = 75)
# Rows: 8,665
# Columns: 9
# $ date      <dttm> 2008-01-01 00:00:00, 2008-01-01 01:00:00, 2008-01-01 0…
# $ code      <chr> "fr38005", "fr38005", "fr38005", "fr38005", "fr38005", …
# $ no2       <dbl> 11, 7, 14, 13, 10, 9, 9, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, …
# $ so2       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
# $ source    <chr> "europe", "europe", "europe", "europe", "europe", "euro…
# $ site      <chr> "CIRFIM", "CIRFIM", "CIRFIM", "CIRFIM", "CIRFIM", "CIRF…
# $ latitude  <dbl> -20.93183, -20.93183, -20.93183, -20.93183, -20.93183, …
# $ longitude <dbl> 55.30593, 55.30593, 55.30593, 55.30593, 55.30593, 55.30…
# $ site_type <chr> "industrial", "industrial", "industrial", "industrial",…

# Site in Grenoble
d <-
  openair::importEurope(
    site = "fr15639",
    year = 2010:2012,
    meta = TRUE
  )

# Lots of missing "europe" data and nothing past 2024

m %>%
  dplyr::filter(source != "europe") %>%
  MazamaLocationUtils::table_leaflet(
    extraVars = c(
      "source", "site", "code", "site_type",
      "Parameter_name", "start_date", "end_date"
    )
  )

# Site in London
d <-
  openair::importAURN(
    site = "HORS",
    year = 2025:2026,
    meta = TRUE
  )

# > dplyr::glimpse(d, width = 75)
# Rows: 9,552
# Columns: 15
# $ source    <chr> "aurn", "aurn", "aurn", "aurn", "aurn", "aurn", "aurn",…
# $ site      <chr> "London Westminster", "London Westminster", "London Wes…
# $ code      <chr> "HORS", "HORS", "HORS", "HORS", "HORS", "HORS", "HORS",…
# $ date      <dttm> 2025-01-01 00:00:00, 2025-01-01 01:00:00, 2025-01-01 0…
# $ nox       <dbl> 6.04101, 7.03510, 4.58811, 3.89989, 3.09697, 2.98227, 3…
# $ no2       <dbl> 5.58431, 5.89333, 3.78887, 3.27192, 2.58318, 2.24012, 3…
# $ no        <dbl> 0.29786, 0.74464, 0.52125, 0.40955, 0.33509, 0.48402, 0…
# $ o3        <dbl> 79.84551, 83.22941, 84.66936, 84.23738, 83.64340, 84.02…
# $ pm2.5     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
# $ ws        <dbl> 8.3, 8.5, 9.2, 8.7, 9.1, 9.2, 9.1, 9.3, 9.4, 9.5, 9.1, …
# $ wd        <dbl> 233.5, 235.7, 238.4, 237.5, 237.5, 234.9, 233.6, 236.9,…
# $ air_temp  <dbl> 10.3, 10.1, 10.6, 10.9, 11.0, 10.9, 10.9, 10.9, 11.3, 1…
# $ site_type <chr> "Urban Background", "Urban Background", "Urban Backgrou…
# $ latitude  <dbl> 51.49467, 51.49467, 51.49467, 51.49467, 51.49467, 51.49…
# $ longitude <dbl> -0.131931, -0.131931, -0.131931, -0.131931, -0.131931, …

d %>%
  selectByDate(
    start = "2026-01-01 00:00",
    end = "2026-02-01 00:00"
  ) %>%
  timePlot(pollutant = "pm2.5")

d %>%
  selectByDate(
    start = "2026-01-01 00:00",
    end = "2026-02-01 00:00"
  ) %>%
  calendarPlot(pollutant = "pm2.5")

d %>%
  selectByDate(
    start = "2026-01-01 00:00",
    end = "2026-02-01 00:00"
  ) %>%
  pollutionRose(pollutant = "pm2.5")

d %>%
  selectByDate(
    start = "2026-01-01 00:00",
    end = "2026-02-01 00:00"
  ) %>%
  polarPlot(pollutant = "pm2.5")

