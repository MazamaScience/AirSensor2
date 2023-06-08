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
  # Rows: 604
  # Columns: 8
  # $ timestamp    <chr> "2023-05-03T18Z", "2023-05-03T18Z", "2023-05-03T18Z"…
  # $ QCFlag       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
  # $ pm2.5        <dbl> 5.00, 3.28, 22.05, 5.79, 7.70, 5.55, 23.59, 11.00, 1…
  # $ nowcast      <dbl> 4.58, 3.01, 19.63, 5.52, 9.89, 6.17, 22.75, 10.32, 2…
  # $ datasourceId <chr> "DAABL1560", "DAAZI7074", "DADKD2421", "DAENX0980", …
  # $ longitude    <dbl> -118.20581, -122.05722, 74.58188, -118.36449, 76.897…
  # $ latitude     <dbl> 34.07283, 37.06706, 42.82760, 34.03556, 43.17658, 37…
  # $ datetime     <dttm> 2023-05-03 18:00:00, 2023-05-03 18:00:00, 2023-05-0…

  synoptic <-
    rawSynoptic %>%

    # * New columns -----
    dplyr::mutate(
      sensorManufacturer = "Clarity",
      deviceID = paste0("clarity.", .data$datasourceId),
      privacy = "public"
    ) %>%

    # * Add core metadata -----
    MazamaLocationUtils::table_addCoreMetadata() %>%

    # TODO:  This step can be removed when MazamaLocationUtils gets upgraded to
    # TODO:  use geohashTools to create a locationID.
    # * Replace MLU version 0.3.8 locationID with geohash
    dplyr::mutate(
      locationID = MazamaCoreUtils::createLocationID(.data$latitude, .data$longitude, algorithm = "geohash")
    ) %>%

    # Fill in new columns where possible
    dplyr::mutate(
      deviceDeploymentID = paste0(.data$locationID, "_", .data$deviceID),
      locationName = .data$datasourceId
    )

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
