# Workshop on R packages for Air Quality Data

_Last updated February 21, 2024_

----

This workshop will introduce participants to two open source R packages designed
specifically for
working with air quality data from regulatory monitors and low-cost sensors. The
[AirMonitor](https://mazamascience.github.io/AirMonitor) and
[AirSensor2](https://mazamascience.github.io/AirSensor2) packages
have been developed with funding from, and have been used by the US EPA, the US
Forest Service and the South Coast Air Quality Management District. These R
packages are part of a suite of R packages that provide core functionality for 
environmental monitoring at fixed locations. The full suite of packages
represent a decade of continuous development with a focus on
compact data formats, robust data analysis, compelling data visualization and a
simple, easy-to-learn coding style.

## Goals

Attendees will become familiar with the **AirMonitor** and **AirSensor2** R
packages and will be able to quickly download, process and visualize large
amounts of monitor and sensor data. Various analysis functions will be
introduced and users will be able to choose their own sensors and monitors to
create QC reports and end-user graphics.

## Audience

The R packages presented are designed for individuals who sometimes need to work
independently, without the support of IT staff, for data ingest and manipulation.
The target audience includes anyone who works with Air Quality data from
regulatory monitors and low cost sensors and who is a regular user of R/RStudio.
Attendees should have a basic understanding of R data types and common functions.
Familiarity with the [dplyr](https://dplyr.tidyverse.org) package will be
especially helpful.

# R Packages for Environmental Time Series

Over the last decade, Mazama Science created multiple open source R packages
focused on environmental monitoring time series. These are used operationally in
data processing, analysis and visualization systems at the
US Forest Service [AirFire Team](https://portal.airfire.org/home), in the
EPA AirNow [Fire & Smoke map](https://fire.airnow.gov) and elsewhere.

Jonathan Callahan currently maintains the following R packages:

- [MazamaRollUtils](https://github.com/MazamaScience/MazamaRollUtils) – Fast rolling functions for R written in C++
- [MazamaCoreUtils](https://github.com/MazamaScience/MazamaCoreUtils) – Utilities to help write production R code
- [MazamaSpatialUtils](https://github.com/MazamaScience/MazamaSpatialUtils) – Harmonized spatial datasets and spatial search functions
- [MazamaLocationUtils](https://github.com/MazamaScience/MazamaLocationUtils) – Utilities for working with monitoring site “known locations”
- [MazamaTimeSeries](https://github.com/MazamaScience/MazamaTimeSeries) – Core functionality for environmental time series data
- [AirMonitor](https://github.com/MazamaScience/AirMonitor) – Utilities for working with air quality monitoring data
- [AirMonitorPlots](https://github.com/MazamaScience/AirMonitorPlots) – Plotting functions for the AirMonitor package
- [AirSensor2](https://github.com/MazamaScience/AirSensor2) – Utilities for working with data from low-cost air quality sensors

Each package has a dedicated Slack channel for announcements, support and to
help build communities of practice around these shared tools. You may request
an invitation to join from jonathan.callahan@dri.com.

# Workshop Preparation

**Attendees are strongly encouraged to install the required packages and data 
files _before_ the workshop begins.**

As with any hands-on workshop, advanced preparation by individuals will allow
us to quickly dive into data analysis. Please ensure that the following software 
and data files have been installed on your laptop in advance of the workshop:

## R and RStudio

This workshop is designed for those who are already familiar with R and RStudio
and use them on a regular basis.

Ensure that you are running [R](https://www.r-project.org) version 4.0 or higher.

Ensure that you have installed [RStudio Desktop](https://posit.co/download/rstudio-desktop/)
version 2023.03 or higher.

## CRAN packages

Most _(but not all)_ of the packages we will be using are available on CRAN.
The easiest way to install these is to install the **AirMonitor** (>= 0.4.0) 
package which will recursively install all dependency packages.

At the RStudio Console prompt type:

```
install.packages("AirMonitor")
...
```

## Non-CRAN packages

A few of the packages have not yet made it to CRAN. For these, you will need
to install them directly from GitHub. Tools in the **devtools** pacakge make
this very easy.

At the RStudio Console prompt type:

```
install.packages("devtools")
...
devtools::install_github("mazamascience/AirMonitorPlots")
...
devtools::install_github("mazamascience/AirSensor2")
...
```

## Spatial data

The **MazamaSpatialUtils** package is used to enhance spatial metadata when
ingesting sensor data. Simplified spatial datasets for countries and timezones
are installed with the package but other, larger datasets must be installed
manually in a dedicated directory. Work with the **AirSensor2** package requires
installation of dastasets with state and county boundaries.

The default location for spatial data used in this workshop is underneath
your home directory in `~/Data/Spatial/`. The following chunk of code will
create this directory and populate it with the required datasets.

At the RStudio Console prompt type:

```
dir.create("~/Data/Spatial", recursive = TRUE)
MazamaSpatialUtils::setSpatialDataDir("~/Data/Spatial")
MazamaSpatialUtils::installSpatialData("NaturalEarthAdm1")
MazamaSpatialUtils::installSpatialData("USCensusCounties")
```

## Workshop repository

Finally, you should install all of the documentation, data and scripts for this
workshop by cloning this repository. 

### From RStudio

You can create a new project from a GitHub repository by using the RStudio
menubar:

1. File > New Project...
2. Version Control > Git
3. URL: https://github.com/MazamaScience/ASIC-2024.git

Choose a directory into which to clone the repository, check "Open in new session"
and then press "Create Project".

### From a terminal

If you prefer, you can use a separate terminal window or the RStudio _Terminal_ 
window to issue a shell command to clone the GitHub repository.

At the terminal prompt, type:

```
git clone git@github.com:mazamascience/ASIC-2024.git
...
```

If you don't have a git account or can't log in, you may need to use the
https location:

```
git clone https://github.com/MazamaScience/ASIC-2024.git
...
```

---

_Congratulations!_

You have finished installing all required software for this workshop.
