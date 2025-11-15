# Execute the Shiny App for bullet investigation

This shiny app would expect a tibble object called `shiny.tt` in the
user's working environment.

## Usage

``` r
runExample()
```

## Details

shiny.tt should have variables: `x3p`, `scan_id`, `crosscut`, `ccdata`,
`grooves`.

To have this shiny app run properly, `shiny.tt` should at least include
`grooves` as a variable

## Examples

``` r
if (FALSE) { # \dontrun{
bulletxtrctr::runExample()
} # }
```
