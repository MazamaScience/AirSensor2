# AirSensor2 0.5.4

- Updated various examples that referenced `example_pas` with the newer
`example_pas_pm25`.
- Added `example_pat_epa` with fields required to run `pat_applyCorrection()`.
- Fixed the bug in `pat_create()` that generated:
```
Elements `last_modified`, `privacy`, `model`, `hardware`, `firmware_version`, etc. don't exist.
```

# AirSensor2 0.5.3

Updated all Clarity functions to default to `format = "USFS2"` which returns
two extra fields: `calibrationId`, `calibrationCategory`.

# AirSensor2 0.5.2

Updated the of data that can be requested in a single call in `pat_create()` to
match updates to the PurpleAir API:

| Average Period | Previous Limit | New Limit |
| -------------- | -------------- | --------- |
| Real-Time | 2 Days | 30 Days |
| 10 Minutes | 3 Days | 60 Days |
| 30 Minutes | 7 Days | 90 Days |
| 1 Hour | 14 Days | 180 Days |
| 6 Hours | 90 Days | 1 Year |
| 1 Day | 1 Year | 2 Years |

# AirSensor2 0.5.1

* `pat_createNew()` now checks for and removes duplicate timesteps and guarantees
that the returned object has class name 'sts' for "SingleTimeSeries" so that it 
can work with functions from **MazamaTimeSeries**.
* Fixed bug in `pat_toMonitor()` that always complained about missing fields.
* Fixed a bug where the `sensor_index` field returned from PurpleAir was sometimes
interpreted as an integer instead of a character. (Identifiers should always 
be of character type as in "007" for James Bond.)
* Fixed a bug in `pat_downloadParseRawData()` where timestamps representing
number of seconds sometimes used exponential notation which the PurpleAir API
does not understand. Example of the bug: 

```
> MazamaCoreUtils::parseDatetime("2023-05-02 03:59:59", timezone = "UTC") %>% as.numeric()
[1] 1.683e+09
```

# AirSensor2 0.5.0

Version 0.5 introduces changes in the default parameters that are requested
when making data requests from the PurpleAir API. In the past, when data was 
free, functions download _as many_ potentially relevant parameters
as possible. Now that a payment system is in place, **AirSensor2** will be focused 
on downloading _as few_ parameters as required to accomplish a task. Many of
the low level PurpleAir data access functions will thus behave differently. 

Package dependency updates:

```
MazamaCoreUtils (>= 0.5.2)
MazamaLocationUtils (>= 0.4.3),
MazamaSpatialUtils (>= 0.8.6),
MazamaTimeSeries (>= 0.3.0),
AirMonitor (>= 0.4.0),
```

* Modified `pas_downloadParseRawData()` defaults to only query for non-measurement
fields defined in `PurplAir_SENSOR_METADATA_FIELDS`.
* Added `pas_filterDate()` to help find historical sensor data.
* Added `pas_filterNearMonitor()` to associate sensors with the closest available
monitor in a _mts_monitor_ object from the **AirMonitor** package.
* Renamed `pas_get()` to `pas_pull()`.
* Updated `pas_createNew()` and `pas_leaflet()` to default to working with
non-measurement fields defined in `PurplAir_SENSOR_METADATA_FIELDS`.
* Added `pas_lifespanPlot()` to show when PurpleAir sensors were producing data.
* Updated collections of fields used in creation of _pas_ and _pat_ objects:
  - `PurpleAir_PAS_MINIMAL_FIELDS`
  - `PurpleAir_PAS_METADATA_FIELDS`
  - `PurpleAir_PAS_AVG_PM25_FIELDS`
  - `PurpleAir_PAList_PM25_FIELDS`
  - `PurpleAir_PAT_QC_FIELDS`
  - `PurpleAir_PAT_EPA_HOURLY_FIELDS`
* Added a `sleep` parameter to `pat_create()` to avoid "rate limiting" errors
from the PurpleAir API.
* Removed `PurpleAir_createNewMonitor()`.
* Added `pat_toMonitor()`.
* Renamed `pat_createNew()` to `pat_create()`.
* Added `pat_createRaw()`.


# AirSensor2 0.4.0

Updated to handle changes in upstream packages:

- **MazamaCoreUtils** 0.4.15 => 0.5.1
- **MazamaLocationUtils** 0.3.8 => 0.4.1
- **MazamaSpatialUtils** 0.8.5 => 0.8.6
- **MazamaTimeSeries** 0.2.13 => 0.2.15
- **AirMonitor** 0.3.11 => 0.3.12

The breaking change that precipitated this was the change from `'zip'` to
`'postalCode'` in the core metadata used by **MazamaLocationUtils**. For 
backwards compatibility, all functions that work with metadata will guarantee 
that both `'zip'` and `'postalCode'` exist. These include:

- `Clarity_enhanceRawSynopticData()`
- `Clarity_createAllOpenMonitors()`
- `Clarity_createOpenMonitor()`
- `PurpleAir_createNewMonitor()`
- `pas_createNew()`
- `pas_enhanceRawData()`
- `pat_create()`

# AirSensor2 0.3.7

- Added `Clarity_createAllOpenMonitors()` to create a monitor object using the
most recent 3 hours of data. (This is useful in the next function.)
- Added `Clarity_updateAllOpenMonitors()` to append the most recent 3 hours of data
to an existing monitor object.

# AirSensor2 0.3.6

- Updated all documentation to match current functionality.
- Enabled faster downloading of _pat_ data with `parallel = TRUE`.
_(not available on Windows)_

# AirSensor2 0.3.5

- Updated Mazama package dependencies.
- Updated example datasets.
- Improved documentation and examples for `pas~()` functions.

# AirSensor2 0.3.4

- Renamed `PurpleAir_createMonitor()` to `PurpleAir_createNewMonitor()`. This
leaves room for a modified `PurpleAir_createMonitor()` that will accept a previously 
saved "hourly pat" object.
- Added "MVCAA Tutorial 1: Exploring PurpleAir Data" article.

# AirSensor2 0.3.3

- Added `pm2.5_cf_1, pm2.5_cf_1_a, pm2.5_ f_1_b` to `PurpleAir_PAT_EPA_HOURLY_FIELDS`
so that the original EPA correction factor can be used with `pm2.5_cf_1`.
- Added `pat_applyCorrection()` to apply a correction equation to "hourly pat"
data. Default correction uses the \code{"EPA_FASM"} correction as described in
the documentation.
- Exchanged the `parameter` argument for `correction_FUN` in `PurpleAir_createMonitor().

# AirSensor2 0.3.2

- Added `pat_createHourly()` as a convenience function for the common case of
obtaining hourly aggergated for the following parameters: 
`datetime, temperature, humidity, pm2.5_atm, pm2.5_atm_a, pm2.5_atm_b`. Using
this function greatly reduces data download volumes and allows for the most
common forms of QC and correction.
- Added `PurpleAir_createMonitor()` to directly create monitor objects suitable
for use with the **AirMonitor** package. This is a preliminary version of this
function that includes neither QC nor correction.

# AirSensor2 0.3.1

- Updated Clarity API functions to handle the addition of a `nowcast_QCFlag` in
the returned data.
- Removed **geohashTools** dependency.

# AirSensor2 0.3.0

This version introduces data ingest functions for Clarity "Open" sensor data. 
In preparation for additional data providers, many of the PurpleAir functions 
using `"pa"` as shorthand have been renamed with a more explicit `"PurpleAir"`:

- Renamed all `pa_~()` functions to `PurpleAir_~()`.
- Renamed `PURPLE_AIR_API_~` API keys to `PurpleAir_API_~`.
- Renamed `purple_air_synoptic` class to `PurpleAir_synoptic`.

Clarity "Open" datasets are available to those with an API key allowing access
to this data. The following functions provide access to Clarity "Open" data:

- `Clarity_getAllOpenHourly()`
- `Clarity_getAllOpenIndividual()`
- `Clarity_getOpenHourly()`
- `Clarity_getOpenIndividual()`
- `Clarity_createOpenSynoptic()`
- `Clarity_enhanceRawSynopticData()`
- `Clarity_createOpenMonitor()`
  
Generic synoptic data from non-PurpleAir providers (similar to PurpleAir 
_'pas'_ objects) is supported with:

- `synoptic_leaflet()`

# AirSensor2 0.2.1

* Changed examples to use `PurpleAir_API_READY_KEY` instead of `MY_API_READ_KEY`.
* Added "Working with PurpleAir API Keys" article.
* When `show_only` is used, spatial bounding information is ignored in: 
`PurpleAir_getSensorsData()`, `pas_downloadParseRawData()` and `pas_createNew()`.
* Added the following _pat_ functions for subsetting the data:
  - `pat_filter()`
  - `pat_filterDate()`
  - `pat_filiterDatetime()`
  - `pat_trimDate()`

# AirSensor2 0.2.0

Added full support for the PurpleAir API described at 
https://api.purpleair.com.

Improvements include wrapper functions for PurpleAir API endpoints:

* `PurpleAir_PurpleAir_checkApiKey()`
* `PurpleAir_getSensorData()`, `PurpleAir_getSensorHistoryCSV()`, `PurpleAir_getSensorHistory()`, `PurpleAir_getSensorsData()`
* `PurpleAir_createGroup()`, `PurpleAir_createMember()`, `PurpleAir_delteGroup()`, `PurpleAir_deleteMember()`,
`PurpleAir_getGroupDetail()`, `PurpleAir_getGroupsList()`, `PurpleAir_getMemberData()`, `PurpleAir_getMemberHistory()`,
`PurpleAir_getMembemrsData()`

Additional updates include:

* Default `fields` strings for `PurpleAir_DATA_PM25_FIELDS` and 
`PurpleAir_PAT_QC_FIELDS`
* Added `fields` argument to `pas_downloadParseRawData()`.
* Now using a 10-character geohash as the `locationID`. (See 
[geohashTools](https://github.com/MichaelChirico/geohashTools) for reasons
to use a geohash as a unique location identifier.)
* Updated `pas_downloadParseRawData()` to use the new wrapper functions.
* Updated `pas_enhanceRawData()` and `pas_createNew()` to harmonize argument
names. 
* Addded `pat_downloadParseRawData()` using the new wrapper functions.
* Added `pat_create()` and `example_pat` dataset.
* Added utility functions: `pat_check()`, `pat_isValid()`, `pat_isEmpty()`,
`pat_distinct()`, `pat_getData()`, `pat_getMeta()`.

# AirSensor2 0.1.4

Documentation improvements to include links to PurpleAir Terms of service and
Data license.

# AirSensor2 0.1.3

* Updated to use **sf** based **MazamaSpatialUtils** >= 0.8.
* Added **sf** dependency.

# AirSensor2 0.1.2

* Now _Depending_ on **MazamaCoreUtils**
* Added **AirMonitor** dependency.
* Removed **sp** dependency.
* Now using APIKey functionality.
* Added initial vignettes.

# AirSensor2 0.1.1

* Full _pas_ functionality and documentation. Passes checks. 

# AirSensor2 0.1.0

The **AirSensor2** package is a refactoring of the core, data access and 
manipulation functionality found in the **AirSensor** package. The goal in 
**AirSensor2** is to create a lean and generic package focused on data ingest
and basic data manipulation.

Initial functions to download and work with PurpleAir synoptic data.

* `initializeMazamaSpatialUtils()`
* `pas_createNew()`
* `pas_downloadParseRawData()`
* `pas_enhanceRawData()`
* `pas_leaflet()`
* `pas_filter()`
* `pas_filterArea()`
* `pas_filterNear()`
* `pas_palette()`


