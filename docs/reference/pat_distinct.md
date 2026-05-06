# Retain only distinct data records in \`pat\$data\`

Three successive steps are used to guarantee that the \`datetime\` axis
contains no repeated values:

1.  remove any duplicate records

2.  guarantee that rows are in \`datetime\` order

3.  average together fields for any remaining records that share the
    same \`datetime\`

## Usage

``` r
pat_distinct(pat)
```

## Arguments

- pat:

  \*pat\* object

## Value

An \*pat\* object where each record is associated with a unique time. (A
list with \`meta\` and \`data\` dataframes.)
