# Returns a column of data from a PurpleAir synoptic object

A simple wrapper around \`dplyr::pull()\` to help with readability of
user recipes.

## Usage

``` r
pas_pull(pas, name = NULL)
```

## Arguments

- pas:

  PurpleAir Synoptic \*pas\* object.

- name:

  Name of the data column to return.

## Value

A single column of data from the incoming \*pas\* object.

## Examples

``` r
library(AirSensor2)

# Lane (County) Regional Air Protection Agency
LRAPA_sensor_indices <-
  example_pas_pm25 %>%
  pas_filter(stringr::str_detect(name, "Ambassador")) %>%
  pas_pull("sensor_index")

print(LRAPA_sensor_indices)
#>  [1] "10166"  "10168"  "10182"  "10188"  "13659"  "13661"  "13675"  "13955" 
#>  [9] "15077"  "56639"  "95189"  "95227"  "95345"  "104610" "107952" "116223"
#> [17] "129783" "175767" "175765" "175769" "175779" "175887" "175885" "175915"
#> [25] "175913" "175919"
```
