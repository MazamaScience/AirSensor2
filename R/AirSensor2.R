#'
#' @docType package
#' @name AirSensor2
#' @title Air Quality Data Analysis for low-cost Sensors
#' @description
#' \code{
#' Utilities for working with air quality monitoring data from low-cost sensors
#' with a focus on small particulates (PM2.5). Initial focus is on sensors
#' produced by 'PurpleAir' <https://www2.purpleair.com>.
#' }

NULL


# ----- AQI categories ---------------------------------------------------------

# NOTE:  This section is a 2022-05-03 verbatim copy from AirMonitor/AirMonitor.R

#' AQI breaks and associated names and colors
#'
#' @export
#' @docType data
#' @name US_AQI
#' @title US EPA AQI Index levels, names, colors and action text
#' @format A list with named elements
#' @description
#' Official, US EPA AQI levels, names, colors and action text are provided in a
#' list for easy coloring and labeling.
#'
#' @section Breaks:
#'
#' Breakpoints are given in units reported for each parameter and include:
#' \itemize{
#' \item{\code{breaks_AQI}}
#' \item{\code{breaks_CO}}
#' \item{\code{breaks_NO2}}
#' \item{\code{breaks_OZONE_1hr}}
#' \item{\code{breaks_OZONE_8hr}}
#' \item{\code{breaks_PM2.5}}
#' \item{\code{breaks_PM10}}
#' }
#'
#' @section Colors:
#'
#' Several different color palettes are provided:
#' \itemize{
#' \item{\code{colors_EPA} -- official EPA AQI colors}
#' \item{\code{colors_subdued} -- subdued colors fo use with leaflet maps}
#' \item{\code{colors_deuteranopia} -- color vision impaired colors}
#' }
#'
#' @section Names:
#'
#' Names of AQI categories are provided in several languages identified by the
#' ISO 639-2 alpha-3 code:
#' \itemize{
#' \item{\code{names_eng}}
#' \item{\code{names_spa}}
#' }
#'
#' @section Actions:
#'
#' Text for "actions to protect yourself" are provided for each
#' category in several languages identified by the
#' ISO 639-2 alpha-3 code:
#' \itemize{
#' \item{\code{actions_eng}}
#' \item{\code{actions_spa}}
#' }
#'
#' Currently supported languages include English (eng) and Spanish (spa).
#'
#' AQI breaks are defined at
#' \url{https://www.airnow.gov/sites/default/files/2020-05/aqi-technical-assistance-document-sept2018.pdf}
#' and are given in units appropriate for each pollutant.
#'
#' AQI colors are defined at \url{https://docs.airnowapi.org/aq101}
#' @note
#' The low end of each break category is used as the breakpoint.
#'
#' @examples
#' print(US_AQI$breaks_AQI)
#' print(US_AQI$colors_EPA)
#' print(US_AQI$names_eng)
#' print(US_AQI$names_spa)

US_AQI <- list(

  # NOTE:  We must have default breaks with just the parameter name
  # Breaks for all supported parameters
  breaks_AQI = c(-Inf, 50, 100, 150, 200, 300, Inf),
  breaks_CO = c(-Inf, 4.5, 9.5, 12.5, 15.5, 30.5, Inf),
  breaks_NO2 = c(-Inf, 54, 101, 361, 650, 2501, Inf),
  breaks_OZONE = c(-Inf, 0, .125, .165, .205, .405, Inf),        # Using OZONE_1hr
  breaks_PM2.5 = c(-Inf, 12, 35.5, 55.5, 150.5, 250.5, Inf),     # Using PM2.5_24hr
  breaks_PM10 = c(-Inf, 55, 155, 255, 355, 425, Inf),

  # Special breaks
  breaks_OZONE_1hr = c(-Inf, 0, .125, .165, .205, .405, Inf),    # GOOD, MOD undefined at EPA
  breaks_OZONE_8hr = c(-Inf, .055, .071, .086, .106, .405, Inf), # HAZ undefined at EPA

  # Official EPA colors
  colors_EPA = c(
    grDevices::rgb(0,228/255,0),
    grDevices::rgb(255/255,255/255,0),
    grDevices::rgb(255/255,126/255,0),
    grDevices::rgb(255/255,0,0),
    grDevices::rgb(143/255,63/255,151/255),
    grDevices::rgb(126/255,0,35/255)
  ),
  # Subdued colors used by USFS AirFire Monitoring (Mv4) site
  colors_subdued = c("#2ecc71", "#f1c40f", "#e67e22", "#e74c3c", "#9b59b6", "#8c3a3a"),
  # Color vision impaired colors recommended by Mazama Science
  colors_deuteranopia = c("#8cddf5", "#ffef00", "#f7921f", "#ed1d24", "#a3064b", "#6d0526"),

  # Names in different languages
  names_eng = c('Good', 'Moderate', 'USG', 'Unhealthy', 'Very Unhealthy', 'Hazardous'),
  names_spa = c('Buena', 'Moderada', 'IGS', 'Insalubre', 'Muy insalubre', 'Peligrosa'),

  # Action text in different languages
  # NOTE:  R packages require that unicode characters be escaped.
  actions_eng = c(
    'None.',
    'Unusually sensitive individuals should consider limiting prolonged or heavy exertion.',
    'People within Sensitive Groups should reduce prolonged or heavy outdoor exertion.',
    'People within Sensitive Groups should avoid all physical outdoor activity.',
    'Everyone should avoid prolonged or heavy exertion.',
    'Everyone should avoid any outdoor activity.'
  ),
  actions_spa = c(
    'Ninguna.',
    'Personas inusualmente sensitivas deber\\u00edan considerar limitar la labor prolongada \\u00f3 intensa.',
    'Personas dentro de los grupos sensitivos deben reducir la labor prolongada \\u00f3 intensa al aire libre.',
    'Personas dentro de los grupos sensitivos deben evitar toda actividad f\\u00edsica al aire libre.',
    'Todos deben evitar la labor prolongada \\u00f3 intensa.',
    'Todos deben evitar cualquier actividad al aire libre.'
  )

)

