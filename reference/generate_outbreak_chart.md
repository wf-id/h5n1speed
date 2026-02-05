# Generate the outbreak chart

This function generates the outbreak chart from the
data-raw/2025-11-21-h5n1-dairy.csv file.

## Usage

``` r
generate_outbreak_chart(
  data = here::here("data-raw", "2025-11-21-h5n1-dairy.csv")
)
```

## Arguments

- data:

  The path to the data file.

## Value

A ggplot object.
