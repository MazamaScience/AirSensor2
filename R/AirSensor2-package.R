#' @keywords internal
"_PACKAGE"

#' @name AirSensor2
#' @title Air Quality Data Analysis for Low-Cost Sensors
#' @description
#' Utilities for working with air quality monitoring data from low-cost sensors
#' with a focus on small particulates (PM2.5). Initial focus is on sensors
#' produced by 'PurpleAir' <https://www2.purpleair.com>.
#'
#' @references [PurpleAir](https://www2.purpleair.com)
#' @references [PurpleAir API](https://api.purpleair.com)
#' @references [PurpleAir Terms of service](https://www2.purpleair.com/policies/terms-of-service)
#' @references [PurpleAir Data license](https://www2.purpleair.com/pages/license)
#' @references [PurpleAir Data Attribution](https://www2.purpleair.com/pages/attribution)
NULL


# ----- AQI categories ---------------------------------------------------------

#' @title US EPA AQI Index levels, names, colors and action text
#'
#' @description
#' Official US EPA AQI levels, names, colors, and action text are provided in a
#' list for easy coloring and labeling.
#'
#' See [AirMonitor::US_AQI()] for details.
#'
#' @name US_AQI
#' @rdname US_AQI
#' @export
#' @importFrom AirMonitor US_AQI
NULL
