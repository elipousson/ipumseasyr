
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ipumseasyr

<!-- badges: start -->
<!-- badges: end -->

The goal of ipumseasyr is to …

## Installation

You can install the development version of ipumseasyr like so:

``` r
# pak::pkg_install("elipousson/ipumseasyr")
```

## Example

``` r
library(ipumseasyr)
## basic example code
```

``` r
state_population_extract <- define_nhgis_ts_extract(
  year = seq(1990, 2010, by = 10),
  tables = "CL8",
  geography = "state"
)

state_population_extract
#> Unsubmitted IPUMS NHGIS extract 
#> Description: 
#> 
#> Time Series Table: CL8
#>   Geog Levels: state
#>   Years: 1990, 2000, 2010
```

``` r
nhgis_extracts <- get_nhgis_extract_history()

state_population <- get_nhgis_ts_data(extract = nhgis_extracts[[1]],
                                      down)
#> Successfully submitted IPUMS NHGIS extract number 27
#> Checking extract status...
#> Waiting 10 seconds...
#> Checking extract status...
#> IPUMS NHGIS extract 27 is ready to download.
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#> Data file saved to /Users/elipousson/Projects/01_rpackages/ipumseasyr/nhgis0027_csv.zip
#> Use of data from NHGIS is subject to conditions including that users should cite the data appropriately. Use command `ipums_conditions()` for more details.
#> Rows: 153 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (3): GISJOIN, STATE, STATEA
#> dbl (5): GEOGYEAR, DATAYEAR, CL8AA, CL8AAL, CL8AAU
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

dplyr::glimpse(state_population)
#> Rows: 153
#> Columns: 9
#> $ filename <chr> "nhgis0027_csv/nhgis0027_ts_geog2010_state.csv", "nhgis0027_c…
#> $ GISJOIN  <chr> "G010", "G020", "G040", "G050", "G060", "G080", "G090", "G100…
#> $ GEOGYEAR <dbl> 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2…
#> $ DATAYEAR <dbl> 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1…
#> $ STATE    <chr> "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Co…
#> $ STATEA   <chr> "01", "02", "04", "05", "06", "08", "09", "10", "11", "12", "…
#> $ CL8AA    <dbl> 4040590, 550043, 3665228, 2350727, 29760021, 3294394, 3287116…
#> $ CL8AAL   <dbl> 4040461, 550043, 3665196, 2350407, 29760021, 3294319, 3287116…
#> $ CL8AAU   <dbl> 4040632, 550043, 3665228, 2350894, 29760021, 3294464, 3287217…
```
