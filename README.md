
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ipumseasyr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

The goal of ipumseasyr is to ease access to the
[IPUMS](https://www.ipums.org/) data (especially
[NHGIS](https://www.nhgis.org/) data) with easy-to-use wrappers for the
[`{ipumsr}`](https://tech.popdata.org/ipumsr/) package.

## Installation

You can install the development version of ipumseasyr from GitHub:

``` r
# pak::pkg_install("elipousson/ipumseasyr")
```

## Example

``` r
library(ipumseasyr)
```

Define an extract without submitting it:

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

Get an extract (using an extract from my extract history):

``` r
nhgis_extracts <- get_nhgis_extract_history()

withr::with_tempdir({
  state_population <- get_nhgis_ts_data(
    extract = nhgis_extracts[[1]]
  )

  dplyr::glimpse(state_population)
})
#> Successfully submitted IPUMS NHGIS extract number 30
#> Checking extract status...
#> Waiting 10 seconds...
#> Checking extract status...
#> Waiting 20 seconds...
#> Checking extract status...
#> IPUMS NHGIS extract 30 is ready to download.
#>   |                                                                              |                                                                      |   0%  |                                                                              |====                                                                  |   6%  |                                                                              |==========                                                            |  15%  |                                                                              |===================================                                   |  50%  |                                                                              |======================================================================| 100%
#> Data file saved to /private/var/folders/3f/50m42dx1333_dfqb5772j6_40000gn/T/RtmpUu23eI/file450974b29832/nhgis0030_csv.zip
#> Use of data from NHGIS is subject to conditions including that users should cite the data appropriately. Use command `ipums_conditions()` for more details.
#> Rows: 9419 Columns: 10
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (8): GISJOIN, STATE, STATEFP, STATENH, COUNTY, COUNTYFP, COUNTYNH, NAME
#> dbl (2): YEAR, A00AA
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Use of data from NHGIS is subject to conditions including that users should cite the data appropriately. Use command `ipums_conditions()` for more details.
#> 
#> Rows: 153 Columns: 7
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (5): GISJOIN, STATE, STATEFP, STATENH, NAME
#> dbl (2): YEAR, A00AA
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 9,572
#> Columns: 11
#> $ filename <chr> "nhgis0030_csv/nhgis0030_ts_nominal_county.csv", "nhgis0030_c…
#> $ GISJOIN  <chr> "G0100010", "G0100030", "G0100050", "G0100070", "G0100090", "…
#> $ YEAR     <dbl> 1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980, 1…
#> $ STATE    <chr> "Alabama", "Alabama", "Alabama", "Alabama", "Alabama", "Alaba…
#> $ STATEFP  <chr> "01", "01", "01", "01", "01", "01", "01", "01", "01", "01", "…
#> $ STATENH  <chr> "010", "010", "010", "010", "010", "010", "010", "010", "010"…
#> $ COUNTY   <chr> "Autauga County", "Baldwin County", "Barbour County", "Bibb C…
#> $ COUNTYFP <chr> "001", "003", "005", "007", "009", "011", "013", "015", "017"…
#> $ COUNTYNH <chr> "0010", "0030", "0050", "0070", "0090", "0110", "0130", "0150…
#> $ NAME     <chr> "AUTAUGA COUNTY", "BALDWIN COUNTY", "BARBOUR COUNTY", "BIBB C…
#> $ A00AA    <dbl> 32259, 78556, 24756, 15723, 36459, 10596, 21680, 119761, 3919…
```
