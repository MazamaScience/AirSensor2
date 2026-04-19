#' #' Download raw OpenAQ measurements
#' #'
#' #' Downloads raw measurements from OpenAQ and optionally retrieves
#' #' additional pages when many measurements are found.
#' #'
#' #' When more measurements are available than can be returned in a single
#' #' request, this function can automatically request additional pages up to
#' #' `maxPages`. If more pages may still be available after `maxPages` pages have
#' #' been retrieved, a warning is issued suggesting that the user narrow the
#' #' request.
#' #'
#' #' @param locations_id OpenAQI location ID.
#' #' @param parameters Specific parameters for which to download data.
#' #' @param data A character string for the data interval to return.
#' #' @param startdate Desired start datetime (ISO 8601).
#' #' @param enddate Desired end datetime (ISO 8601).
#' #' @param timezone Olson timezone used to interpret dates.
#' #' @param limit Maximum number of records to request per page.
#' #' @param maxPages Maximum number of pages to request automatically.
#' #' @param sleepSeconds Number of seconds to pause between additional requests.
#' #' @param api_key OpenAQ API key.
#' #'
#' #' @return A dataframe of raw OpenAQ measurements.
#' #' @export
#'
#' OpenAQ_downloadRawData <- function(
#'     locations_id = NULL,
#'     parameters = c("pm2.5", "temperature", "humidity"),
#'     data = c("hours", "measurements"),
#'     startdate = NULL,
#'     enddate = NULL,
#'     timezone = NULL,
#'     limit = 1000,
#'     maxPages = 1,
#'     sleepSeconds = 0.2,
#'     api_key = NULL
#' ) {
#'
#'   # ----- Validate parameters --------------------------------------------------
#'
#'   if ( is.null(api_key) )
#'     api_key <- MazamaCoreUtils::getAPIKey("OpenAQ-read")
#'
#'   MazamaCoreUtils::stopIfNull(api_key)
#'
#'   MazamaCoreUtils::stopIfNull(locations_id)
#'   MazamaCoreUtils::stopIfNull(startdate)
#'   MazamaCoreUtils::stopIfNull(enddate)
#'   MazamaCoreUtils::stopIfNull(timezone)
#'
#'   data <- match.arg(data)
#'
#'   all_parameters <- OpenAQ_getParameters() %>% dplyr::pull(name)
#'   parameter <- match.arg(parameter, choices = all_parameters, several.ok = TRUE)
#'
#'   # ----- Load sensors ---------------------------------------------------------
#'
#'   sensors <-
#'     list_location_sensors(
#'       locations_id,
#'       as_data_frame = TRUE,
#'       dry_run = FALSE,
#'       rate_limit = FALSE,
#'       api_key = api_key
#'     )
#'
#'   # > openaq::list_location_sensors(1370216) %>% dplyr::glimpse(width = 75)
#'   # Rows: 10
#'   # Columns: 20
#'   # $ id                   <dbl> 6664822, 7971006, 7971260, 6664821, 7979159,…
#'   # $ name                 <chr> "temperature c", "pm10 µg/m³", "pm1 µg/m³", …
#'   # $ parameters_id        <dbl> 100, 1, 19, 2, 19, 125, 1, 98, 100, 2
#'   # $ datetime_first_utc   <dttm> 2023-08-07 13:00:00, 2024-03-06 17:00:00, 20…
#'   # $ datetime_first_local <dttm> 2023-08-07 08:00:00, 2024-03-06 11:00:00, 2…
#'   # $ datetime_last_utc    <dttm> 2026-04-19 18:00:00, 2024-11-16 19:00:00, 2…
#'   # $ datetime_last_local  <dttm> 2026-04-19 13:00:00, 2024-11-16 13:00:00, 2…
#'   # $ min                  <dbl> -23.9419114, 0.0000000, 0.0000000, 0.000000…
#'   # $ max                  <dbl> 38.70095, 310.64583, 170.78333, 338.77857, 1…
#'   # $ avg                  <dbl> 18.756687, 13.111081, 7.552080, 14.477025, 8…
#'   # $ expected_count       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, NA
#'   # $ expected_interval    <fct> 01:00:00, 01:00:00, 01:00:00, 01:00:00, 01:0…
#'   # $ observed_count       <dbl> 15407, 5642, 15143, 17805, 44, 15176, 44, 15…
#'   # $ observed_interval    <fct> 15407:00:00, 5642:00:00, 15143:00:00, 17805:…
#'   # $ percent_complete     <dbl> 1540700, 564200, 1514300, 1780500, 4400, 151…
#'   # $ percent_coverage     <dbl> 1540700, 564200, 1514300, 1780500, 4400, 151…
#'   # $ latest_value         <dbl> 10.000000, 22.742361, 2.200000, 4.400000, 1.…
#'   # $ latest_datetime      <dttm> 2026-04-19 18:00:00, 2024-11-16 19:00:00, 20…
#'   # $ latest_latitude      <dbl> 41.96243, 41.96243, 41.96243, 41.96243, 41.9…
#'   # $ latest_longitude     <dbl> -87.73747, -87.73747, -87.73747, -87.73747,…
#'
#'   # TODO:  Use OpenAQ_parameterToID to limit sensors to those with parameters_id
#'   # TODO:  values that match the incoming parameters
#'
#'   # TODO: For each sensor issue a request like:
#'
#'   dataList = list()
#'
#'   dataList[[parameter_name]] <-
#'     list_sensor_measurements(
#'       sensors_id,
#'       data = data,
#'       ###rollup = rollup,
#'       datetime_from = startdate,
#'       datetime_to = enddate,
#'       ###order_by = NULL,
#'       ###sort_order = NULL,
#'       limit = limit,
#'       page = page,
#'       as_data_frame = TRUE,
#'       ###dry_run = FALSE,
#'       ###rate_limit = FALSE,
#'       api_key = api_key
#'     )
#'
#'   #   > dplyr::glimpse(data, width = 75)
#'   #   Rows: 100
#'   #   Columns: 25
#'   #   $ value             <dbl> 20.87500, 22.10417, 22.78333, 24.25833, 25.1833…
#'   #   $ parameter_id      <dbl> 100, 100, 100, 100, 100, 100, 100, 100, 100, 10…
#'   #   $ parameter_name    <chr> "temperature", "temperature", "temperature", "t…
#'   #   $ parameter_units   <chr> "c", "c", "c", "c", "c", "c", "c", "c", "c", "c…
#'   #   $ period_label      <fct> raw, raw, raw, raw, raw, raw, raw, raw, raw, ra…
#'   #   $ period_interval   <fct> 01:00:00, 01:00:00, 01:00:00, 01:00:00, 01:00:0…
#'   #   $ datetime_from     <dttm> 2023-08-07 07:00:00, 2023-08-07 09:00:00, 2023…
#'   #   $ datetime_to       <dttm> 2023-08-07 08:00:00, 2023-08-07 10:00:00, 2023…
#'   #   $ latitude          <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#'   #   $ longitude         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#'   #   $ min               <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#'   #   $ q02               <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#'   #   $ q25               <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#'   #   $ median            <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#'   #   $ q75               <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#'   #   $ q98               <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#'   #   $ max               <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#'   #   $ avg               <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#'   #   $ sd                <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#'   #   $ expected_count    <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#'   #   $ expected_interval <fct> 01:00:00, 01:00:00, 01:00:00, 01:00:00, 01:00:0…
#'   #   $ observed_count    <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#'   #   $ observed_interval <fct> 01:00:00, 01:00:00, 01:00:00, 01:00:00, 01:00:0…
#'   #   $ percent_complete  <dbl> 100, 100, 100, 100, 100, 100, 100, 100, 100, 10…
#'   #   $ percent_coverage  <dbl> 100, 100, 100, 100, 100, 100, 100, 100, 100, 10…
#'
#'   # TODO:  We only want to save the 'value' (renamed to 'parameter_name') and
#'   # TODO: 'datetime_to' (renamed to 'datetime') fields
#'
#'   # TODO:  We then want to join each of resulting the 2-column dataframes by 'datetime'
#'
#'   # TODO:  The final, returned data frame will have column names:
#'   # TODO:    c("datetime", parameters)
#'
#' }
