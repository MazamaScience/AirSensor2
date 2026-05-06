# Working with PurpleAir API Keys

The **AirSensor2** package allows users to work with the PurpleAir API
which requires API keys. This document describes how to automate work
with private API keys without including them in a code repository where
they might be seen by others.

The PurpleAir API is described at: <https://api.purpleair.com/>

When working with PurpleAir data, you should be aware of the following:

- [PurpleAir Terms of
  service](https://www2.purpleair.com/policies/terms-of-service)
- [PurpleAir Data license](https://www2.purpleair.com/pages/license)
- [PurpleAir Data
  Attribution](https://www2.purpleair.com/pages/attribution)

## Obtain an API key

PurpleAir maintains a community forum with a page dedicated to obtaining
an API Key: <https://community.purpleair.com/t/how-to-obtain-an-api-key>

## Create a `.env` file

It is useful to create a file where all API keys and passwords are kept.
I recommend keeping this information in a file named `.env` which can be
sourced at the RStudio prompt or at the beginning of a script. Here is
an example:

    # ----- Air Quality API keys ----------------------------------------------------------

    PurpleAir_API_READ_KEY=8C087B59-***-****-****-***

    OPENAQ_API_KEY=d010897***

    # global_vars.R file

## Update the `.gitignore` file

To avoid uploading your private API keys to GitHub where they might be
seen by others, you should create/modify the repository `.gitignore`
file so that it looks something like this:

    # .gitignore file

    # History files
    .Rhistory
    .Rapp.history

    # Session Data files
    .RData

    # User-specific files
    .Ruserdata

    # Example code in package build process
    *-Ex.R

    # Output files from R CMD build
    /*.tar.gz

    # Output files from R CMD check
    /*.Rcheck/

    # RStudio files
    .Rproj.user/

    # produced vignettes
    vignettes/*.html
    vignettes/*.pdf

    # OAuth2 token, see https://github.com/hadley/httr/releases/tag/v0.3
    .httr-oauth

    # knitr and R markdown default cache directories
    *_cache/
    /cache/

    # Temporary files created by R markdown
    *.utf8.md
    *.knit.md

    # R environment variables
    .Renviron

    # ----- Local customization -----

    # Private environment variables
    .env

## Use API keys in your scripts

The **AirSensor2** package maintains an internal set of API keys which
users can set using
[`setAPIKey()`](https://rdrr.io/pkg/MazamaCoreUtils/man/setAPIKey.html).
These keys will be remembered for the duration of an R session. In
functions that accept an API key argument, if the passed in API key is
`NULL`, code will look up a specific named API key to see if that key
has been set globally. Setting keys globally is a convenience that
simplifies the scripts written by end users.

Currently supported API keys and associated functions include:

- `"PurpleAir-read"` – PurpleAir api_key used in
  [`pas_downloadParseRawData()`](https://mazamascience.github.io/AirSensor2/reference/pas_downloadParseRawData.md)
  [`pat_downloadParseRawData()`](https://mazamascience.github.io/AirSensor2/reference/pat_downloadParseRawData.md),
  [`pat_create()`](https://mazamascience.github.io/AirSensor2/reference/pat_create.md).

Scripts can take advantage of this as seen below:

    # AirSensor2 package
    library(AirSensor2)

    # Set user's PurpleAir_API_READ_KEY
    library(dotenv)
    dotenv::load_dot_env()

    PurpleAir_API_READ_KEY <- Sys.getenv("PurpleAir_API_READ_KEY")
    setAPIKey("PurpleAir-read", PurpleAir_API_READ_KEY)

    # Initialize spatial datasets
    initializeMazamaSpatialUtils()

    ...

From this point on in the R session, all **AirSensor2** functions that
need to pull data from the PurpleAir API will send the appropriate API
key and everything should work.

------------------------------------------------------------------------

*Best of luck working with new PurpleAir API!*
