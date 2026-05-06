# General purpose filtering for PurpleAir Synoptic objects

A generalized data filter for \*pas\* objects to choose rows/cases where
conditions are true. Rows where the condition evaluates to NA are
dropped.

This function is essentially \[dplyr::filter()\] with an extra check on
the validity of the \*pas\* object.

## Usage

``` r
pas_filter(pas, ...)
```

## Arguments

- pas:

  PurpleAir Synoptic \*pas\* object.

- ...:

  Logical predicates defined in terms of the variables in the \`pas\`.
  Multiple conditions are combined with & or seperated by a comma. Only
  rows where the condition evaluates to TRUE are kept.

## Value

A subset of the incoming \*pas\* object.

## See also

\[pas_filterArea()\]

\[pas_filterNear()\]

\[pas_filterNearMonitor()\]

## Examples

``` r
library(AirSensor2)

nrow(example_pas_pm25)
#> [1] 2287

# Washington
WA_pas <-
  example_pas_pm25 %>%
  pas_filter(stateCode == "WA")

nrow(WA_pas)
#> [1] 1373

# Okanogan and Ferry Counties
Colville_Tribes_pas <-
  example_pas_pm25 %>%
  pas_filter(stateCode == "WA") %>%
  pas_filter(countyName %in% c("Okanogan", "Ferry"))

nrow(Colville_Tribes_pas)
#> [1] 80
```
