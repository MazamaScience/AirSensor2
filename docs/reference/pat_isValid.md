# Test \*pat\* object for correct structure

The \`pat\` is checked for the presence of core \`meta\` and \`data\`
columns.

Core \`meta\` columns include:

- \`deviceDeploymentID\` – unique identifier (see MazmaLocationUtils)

- \`deviceID\` – device identifier

- \`locationID\` – location identifier (see MazmaLocationUtils)

- \`locationName\` – English language name

- \`longitude\` – decimal degrees E

- \`latitude\` – decimal degrees N

- \`elevation\` – elevation of station in m

- \`countryCode\` – ISO 3166-1 alpha-2

- \`stateCode\` – ISO 3166-2 alpha-2

- \`timezone\` – Olson time zone

Core \`data\` columns include:

- \`datetime\` – measurement time (UTC)

## Usage

``` r
pat_isValid(pat = NULL, verbose = FALSE)
```

## Arguments

- pat:

  \*pat\* object

- verbose:

  Logical specifying whether to produce detailed warning messages.

## Value

\`TRUE\` if \`pat\` has the correct structure, \`FALSE\` otherwise.

## Examples

``` r
library(AirSensor2)

pat_isValid(example_pat)
```
