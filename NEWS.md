
# AirSensor2 0.2.0

Added full support for the PurpleAir API described at 
https://api.purpleair.com.

Improvements include wrapper functions for PurpleAir API endpoints:

* `pa_pa_checkApiKey()`
* `pa_getSensorData()`, `pa_getSensorHistoryCSV()`, `pa_getSensorHistory()`, `pa_getSensorsData()`
* `pa_createGroup()`, `pa_createMember()`, `pa_delteGroup()`, `pa_deleteMember()`,
`pa_getGroupDetail()`, `pa_getGroupsList()`, `pa_getMemberData()`, `pa_getMemberHistory()`,
`pa_getMembemrsData()`

Additional updates include:

* Default `fields` strings for `SENSOR_DATA_PM25_FIELDS` and 
`SENSOR_HISTORY_PM25_FIELDS`
* Added `fields` argument to `pas_downloadParseRawData()`.
* Now using a 10-character geohash as the `locationID`. (See 
[geohashTools](https://github.com/MichaelChirico/geohashTools) for reasons
to use a geohash as a unique location identifier.)
* Updated `pas_downloadParseRawData()` to use the new wrapper functions.
* Updated `pas_enhanceRawData()` and `pas_createNew()` to harmonize argument
names. 
* Addded `pat_downloadParseRawData()` using the new wrapper functions.
* Added `pat_createNew()` and `example_pat` dataset.
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

Initial functions to download and work with Purple Air synoptic data.

* `initializeMazamaSpatialUtils()`
* `pas_createNew()`
* `pas_downloadParseRawData()`
* `pas_enhanceRawData()`
* `pas_leaflet()`
* `pas_filter()`
* `pas_filterArea()`
* `pas_filterNear()`
* `pas_palette()`


