#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.error logger.debug logger.isInitialized
#' @importFrom MazamaCoreUtils getAPIKey
#'
#' @title Create a new PurpleAir timeseries dataset.
#'
#' @param api_key PurpleAir API Read Key. If \code{api_key = NULL}, it
#' will be obtained using \code{getAPIKey("PurpleAir-read")}.
#' See \code{MazamaCoreUtils::\link[MazamaCoreUtils:setAPIKey]{setAPIKey}}.
#' @param pas Previously generated \emph{pas} object containing \code{sensor_index}.
#' @param sensor_index PurpleAir sensor unique identifier.
#' @param startdate Desired start time (ISO 8601) or \code{POSIXct}.
#' @param enddate Desired end time (ISO 8601) or \code{POSIXct}.
#' @param timezone Olson timezone used to interpret dates.
#' @param average Temporal averaging in minutes performed by PurpleAir. One of:
#' 0 (raw), 10, 30, 60 (hour), 360, 1440 (day).
#' @param fields Character string with PurpleAir field names for the Get Sensor Data API.
#' @param parallel Logical specifying whether to attempt simultaneous downloads
#' using \code{parallel::\link[parallel:mcparallel]{mcparallel}}. (Not available
#' on Windows.)
#' @param baseUrl Base URL for the PurpleAir API.
#' @param verbose Logical controlling the generation of warning and error messages.
#'
#' @return A PurpleAir Timeseries \emph{pat} object.
#'
#' @description Create a \code{pat} object for a specific \code{sensor_index}.
#'
#' @note Parallel processing using \code{parallel = TRUE} is not available on
#' Windows machines.
#'
#' @references \href{https://www2.purpleair.com}{PurpleAir}
#' @references \href{https://api.purpleair.com}{PurpleAir API}
#' @references \href{https://www2.purpleair.com/policies/terms-of-service}{PurpleAir Terms of service}
#' @references \href{https://www2.purpleair.com/pages/license}{PurpleAir Data license}
#' @references \href{https://www2.purpleair.com/pages/attribution}{PurpleAir Data Attribution}
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' # AirSensor2 package
#' library(AirSensor2)
#'
#' # Set user's PurpleAir_API_READ_KEY
#' source('global_vars.R')
#' setAPIKey("PurpleAir-read", PurpleAir_API_READ_KEY)
#'
#' # Initialize spatial datasets
#' initializeMazamaSpatialUtils()
#'
#' # Create a pat object
#' pat <-
#'   pat_createNew(
#'     pas = example_pas,
#'     sensor_index = "76545",
#'     startdate = "2023-01-01",
#'     enddate = "2023-01-08",
#'     timezone = "UTC",
#'     verbose = TRUE
#'   )
#'
#' str(pat$meta)
#'
#' # Run data download in parallel (faster)
#' pat <-
#'   pat_createNew(
#'     pas = example_pas,
#'     sensor_index = "76545",
#'     startdate = "2023-01-01",
#'     enddate = "2023-01-08",
#'     timezone = "UTC",
#'     parallel = TRUE,
#'     verbose = TRUE
#'   )
#'
#' }, silent = FALSE)
#' }

pat_createNew <- function(
    api_key = NULL,
    pas = NULL,
    sensor_index = NULL,
    startdate = NULL,
    enddate = NULL,
    timezone = "UTC",
    average = 0,
    fields = PurpleAir_HISTORY_PM25_FIELDS,
    parallel = FALSE,
    baseUrl = "https://api.purpleair.com/v1/sensors",
    verbose = FALSE
) {

  # ----- Validate parameters --------------------------------------------------

  if ( is.null(api_key) )
    api_key <- MazamaCoreUtils::getAPIKey("PurpleAir-read")

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(pas)
  MazamaCoreUtils::stopIfNull(sensor_index)
  MazamaCoreUtils::stopIfNull(timezone)
  MazamaCoreUtils::stopIfNull(average)
  MazamaCoreUtils::stopIfNull(fields)
  MazamaCoreUtils::stopIfNull(baseUrl)
  parallel <- MazamaCoreUtils::setIfNull(parallel, FALSE)
  verbose <- MazamaCoreUtils::setIfNull(verbose, FALSE)

  if ( !average %in% c(0, 10, 30, 60, 360, 1440, 10080, 44640, 53560) ) {
    stop("'average' must be one of: 0, 10, 30, 60, 360, 1440, 10080, 44640, 53560")
  }

  # ----- Determine date sequence ----------------------------------------------

  # NOTE:  In 2023, the PurpleAir API limits each request to two days of data so
  # NOTE:  we break up the time range into separate requests.

  # Find a single, parent record
  pas_single <-
    pas %>%
    dplyr::filter(sensor_index == !!sensor_index)

  # TODO:  This step can be removed when AirMonitor gets upgraded to replace
  # TODO:  'zip' with 'postalCode'.
  if ( !"zip" %in% names(pas_single) ) {
    pas_single$zip <- pas_single$postalCode
  }
  if ( !"postalCode" %in% names(pas_single) ) {
    pas_single$postalCode <- pas_single$zip
  }

  if ( nrow(pas_single) == 0 ) {
    stop(sprintf("'pas' has zero records with sensor_index: %s", sensor_index))
  } else if ( nrow(pas_single) > 1 ) {
    stop(sprintf("'pas' has multiple records with sensor_index: %s", sensor_index))
  }

  # Get the timezone associated with this sensor
  if ( is.null(timezone) ) {
    timezone <- pas_single$timezone
  }

  # Create a valid dateRange
  if ( !is.null(startdate) && !is.null(enddate) ) {
    # Don't require day boundaries
    dateRange <- MazamaCoreUtils::timeRange(
      starttime = startdate,
      endtime = enddate,
      timezone = timezone,
      unit = "sec",
      ceilingStart = FALSE,
      ceilingEnd = FALSE
    )
  } else {
    # Default to 2 days with day boundaries
    dateRange <- MazamaCoreUtils::dateRange(
      startdate = startdate,
      enddate = enddate,
      timezone = timezone,
      unit = "sec",
      ceilingStart = FALSE,
      ceilingEnd = FALSE,
      days = 2
    )
  }

  # Create a sequence of every-2-days POSIXct times
  dateSequence <- seq(dateRange[1], dateRange[2], by = lubridate::ddays(2))

  # Tack on the final data if needed
  if ( dateRange[2] > utils::tail(dateSequence, 1) ) {
    dateSequence <- c(dateSequence, dateRange[2])
  }

  # ----- Create data ----------------------------------------------------------

  if ( parallel ) {

    # * Parallel downloads -----

    jobList <- list()

    for ( i in 1:(length(dateSequence) - 1) ) {

      if ( verbose ) {
        message(sprintf("Requesting data for sensor_index %s from %s to %s",
                        sensor_index, dateSequence[i], dateSequence[i+1]))
      }

      jobList[[i]] <- parallel::mcparallel({
        pat_downloadParseRawData(
          api_key = api_key,
          sensor_index = sensor_index,
          startdate = dateSequence[i],
          enddate = dateSequence[i + 1],
          timezone = timezone,
          average = average,
          fields = fields,
          baseUrl = baseUrl
        )
      })

    }

    dataList <- parallel::mccollect(jobList)

  } else {

    # * Sequential downloads -----

    if ( verbose ) {
      message(sprintf("Requesting data for sensor_index %s from %s to %s",
                      sensor_index, dateSequence[1], dateSequence[2]))
    }

    dataList <- list()

    # Use more specific ID rather than the label
    dataList[[1]] <-
      pat_downloadParseRawData(
        api_key = api_key,
        sensor_index = sensor_index,
        startdate = dateSequence[1],
        enddate = dateSequence[2],
        timezone = timezone,
        average = average,
        fields = fields,
        baseUrl = baseUrl
      )

    if ( length(dateSequence) > 2 ) {

      for ( i in 2:(length(dateSequence) - 1) ) {

        if ( verbose ) {
          message(sprintf("Requesting data for sensor_index %s from %s to %s",
                          sensor_index, dateSequence[i], dateSequence[i+1]))
        }

        dataList[[i]] <-
          pat_downloadParseRawData(
            api_key = api_key,
            sensor_index = sensor_index,
            startdate = dateSequence[i],
            enddate = dateSequence[i + 1],
            timezone = timezone,
            average = average,
            fields = fields,
            baseUrl = baseUrl
          )

      }

    }

  } # END of sequential downloads

  data <-
    dplyr::bind_rows(dataList) %>%
    dplyr::rename(datetime = .data$time_stamp) %>%
    dplyr::arrange(.data$datetime) %>%
    dplyr::select(-c(sensor_index))

  # ----- Create meta ----------------------------------------------------------

  pat_metaNames <-
    c(
      "deviceDeploymentID",
      "deviceID",
      "locationID",
      "locationName",
      "longitude",
      "latitude",
      "elevation",
      "countryCode",
      "stateCode",
      "countyName",
      "timezone",
      "houseNumber",
      "street",
      "city",
      "postalCode",
      "sensor_index",
      "last_modified",
      "date_created",
      "privacy",
      "name",
      "location_type",
      "model",
      "hardware",
      "firmware_version",
      "firmware_upgrade",
      "sensorManufacturer"
    )

  meta <-
    pas_single %>%
    dplyr::select(dplyr::all_of(pat_metaNames))

  # TODO:  This step can be removed when AirMonitor gets upgraded to replace
  # TODO:  'zip' with 'postalCode'.
  if ( !"zip" %in% names(meta) ) {
    meta$zip <- meta$postalCode
  }

  # Remove "PurpleAir_synoptic" class
  attributes(meta)$class <- setdiff(attributes(meta)$class, "PurpleAir_synoptic")

  # ----- Return ---------------------------------------------------------------

  # Combine meta and data dataframes into a list
  pat <- list(meta = meta, data = data)
  class(pat) <- c("purple_air_timeseries", class(pat))

  return(pat)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  api_key = PurpleAir_API_READ_KEY
  pas = example_pas
  sensor_index = "76545"
  startdate = "2023-01-01"
  enddate = "2023-01-08"
  timezone = "America/Los_Angeles"
  average = 60
  fields = PurpleAir_HISTORY_PM25_FIELDS
  baseUrl = "https://api.purpleair.com/v1/sensors"
  parallel = TRUE
  verbose = TRUE

}
