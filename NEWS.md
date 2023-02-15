
# AirSensor2 0.1.5

* Added `fields` argument to `pas_downloadParseRawData()`.
* Added `pas_PM25_FIELDS` string containing all pas fields used by default.
* Now using a 10-character geohash as the `locationID`.

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


