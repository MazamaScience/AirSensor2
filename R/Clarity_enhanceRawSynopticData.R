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
  # Rows: 601
  # Columns: 9
  # $ timestamp      <chr> "2023-06-08T23Z", "2023-06-08T23Z", "2023-06-08T23…
  # $ pm2.5_QCFlag   <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
  # $ pm2.5          <dbl> 5.45, 32.62, 5.03, 9.15, 5.22, 9.26, 3.09, 10.88, …
  # $ nowcast_QCFlag <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
  # $ nowcast        <dbl> 6.26, 58.09, 4.54, 9.54, 5.49, 9.41, 3.20, 13.85, …
  # $ datasourceId   <chr> "DAABL1560", "DAAUZ8818", "DAAZI7074", "DADKD2421"…
  # $ longitude      <dbl> -118.20581, 120.75026, -122.05722, 74.58188, -118.…
  # $ latitude       <dbl> 34.07283, 15.00259, 37.06706, 42.82760, 34.03556, …
  # $ datetime       <dttm> 2023-06-08 23:00:00, 2023-06-08 23:00:00, 2023-06…

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
