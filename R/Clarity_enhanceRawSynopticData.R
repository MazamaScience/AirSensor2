#' @export
#' @importFrom rlang .data
#'
#' @title Enhance synoptic data from Clarity
#'
#' @description Enhance raw synoptic data from Clarity to create an improved
#' dataframe compatible with the \pkg{MazamaLocationUtils} package.
#'
#' Steps include:
#'
#' 1) Replace variable names with more consistent, human readable names.
#'
#' 2) Add spatial metadata for each sensor including:
#' \itemize{
#'   \item{timezone -- Olson timezone}
#'   \item{countryCode -- ISO 3166-1 alpha-2}
#'   \item{stateCode -- ISO 3166-2 alpha-2}
#' }
#'
#' 3) Convert data types from character to \code{POSIXct} and \code{numeric}.
#'
#' 4) Add additional metadata items:
#' \itemize{
#' \item{sensorManufacturer = "Clarity"}
#' }
#'
#' @param rawSynoptic 'synoptic' dataframe returned by \code{Clarity_getAllOpenHourly()}.
#'
#' @return Enhanced dataframe of synoptic Clarity data.
#'
#' @seealso \link{Clarity_getAllOpenHourly}
#'

Clarity_enhanceRawSynopticData <- function(
  rawSynoptic = NULL
) {

  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(rawSynoptic)

  if ( !is.data.frame(rawSynoptic) )
    stop("parameter 'rawSynoptic' parameter is not a dataframe")

  # ----- Harmonize table ------------------------------------------------------

  # > dplyr::glimpse(rawSynoptic, width = 75)
  # Rows: 724
  # Columns: 11
  # $ timestamp           <chr> "2024-08-21T12Z", "2024-08-21T12Z", "2024-08-…
  # $ pm2.5_QCFlag        <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
  # $ pm2.5               <dbl> 9.45, 112.00, 3.67, 3.54, 11.52, 13.06, 13.38…
  # $ nowcast_QCFlag      <dbl> 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
  # $ nowcast             <dbl> 8.96, 183.05, 3.88, 3.81, 11.20, 12.74, 13.32…
  # $ datasourceId        <chr> "DAABL1560", "DAAUZ8818", "DAAZI7074", "DABNS…
  # $ longitude           <dbl> -118.20581, 120.75050, -122.05722, -121.87298…
  # $ latitude            <dbl> 34.07283, 15.00045, 37.06706, 37.71934, 34.04…
  # $ calibrationId       <chr> "CAHVV2Z9CH", "CA43PUF7FW", "CAHVV2Z9CH", "CA…
  # $ calibrationCategory <chr> "global PM2.5 v2", "global PM2.5 v1", "global…
  # $ datetime            <dttm> 2024-08-21 12:00:00, 2024-08-21 12:00:00, 20…

  synoptic <-

    rawSynoptic %>%

    # * New columns -----
    dplyr::mutate(
      sensorManufacturer = "Clarity",
      deviceID = paste0("clarity.", .data$datasourceId),
      privacy = "public"
    ) %>%

    # * Add core metadata -----
    # NOTE:  precision = 9 results in a precision of ~2 meters
    MazamaLocationUtils::table_addCoreMetadata(precision = 9) %>%

    # Fill in new columns where possible
    dplyr::mutate(
      deviceDeploymentID = paste0(.data$locationID, "_", .data$deviceID),
      locationName = .data$datasourceId
    )

  # TODO:  This step can be removed when AirMonitor gets upgraded to replace
  # TODO:  'zip' with 'postalCode'.
  if ( !"zip" %in% names(synoptic) ) {
    synoptic$zip <- synoptic$postalCode
  }
  if ( !"postalCode" %in% names(synoptic) ) {
    synoptic$postalCode <- synoptic$zip
  }

  # Put 'deviceDeploymentID' and 'deviceID' in front
  startingIDs <- c("deviceDeploymentID", "deviceID", "locationID")
  otherColumns <- setdiff(names(synoptic), startingIDs)
  orderedColumns <- c(startingIDs, otherColumns)
  synoptic <- synoptic %>% dplyr::select(dplyr::all_of(orderedColumns))

  # ----- Add spatial metadata -------------------------------------------------

  # * countryCode -----

  synoptic$countryCode <-
    MazamaSpatialUtils::getCountryCode(
      longitude = synoptic$longitude,
      latitude = synoptic$latitude,
      allData = FALSE,
      useBuffering = FALSE            # No buffering needed with the EEZ dataset
    )

  # Limit to valid countryCodes
  synoptic <-
    synoptic %>%
    dplyr::filter(!is.na(.data$countryCode))

  # * stateCode -----

  # Suppress annoying 'Discarded datum Unknown' messages
  suppressWarnings({
    synoptic$stateCode <-
      MazamaSpatialUtils::getStateCode(
        longitude = synoptic$longitude,
        latitude = synoptic$latitude,
        countryCodes = unique(synoptic$countryCode),
        allData = FALSE,
        useBuffering = TRUE
      )
  })

  # * countyName -----

  synoptic_us <- synoptic %>% dplyr::filter(.data$countryCode == "US")
  synoptic_other <- synoptic %>% dplyr::filter(.data$countryCode != "US")

  # Suppress annoying 'Discarded datum Unknown' messages
  suppressWarnings({
    synoptic_us$countyName <-
      MazamaSpatialUtils::getUSCounty(
        longitude = synoptic_us$longitude,
        latitude = synoptic_us$latitude,
        stateCodes = unique(synoptic$stateCode),
        allData = FALSE,
        useBuffering = TRUE
      )
  })

  synoptic <- dplyr::bind_rows(synoptic_us, synoptic_other)

  # * timezone -----

  # Suppress annoying 'Discarded datum Unknown' messages
  suppressWarnings({
    synoptic$timezone <-
      MazamaSpatialUtils::getTimezone(
        longitude = synoptic$longitude,
        latitude = synoptic$latitude,
        countryCodes = unique(synoptic$countryCode),
        allData = FALSE,
        useBuffering = TRUE
      )
  })

  # ----- Return ---------------------------------------------------------------

  # Add the "Clarity_synoptic" class name
  class(synoptic) <- union("Clarity_synoptic", class(synoptic))

  return(synoptic)

}
