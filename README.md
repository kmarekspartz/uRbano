
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `uRbano` - functions for spatial sampling across built density gradients

<img src = "inst/figures/logo-city.png" align = "left" width = "20%"/>

`uRbano` is designed to support spatial sampling design across urban
environmental gradients, to guide users in building reproducible,
gradient-informed experimental designs for ecological research in urban
and human-modified landscapes.

<!-- badges: start -->
<!-- badges: end -->

## Installation

You can install the development version of uRbano from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
# devtools::install_github("user/uRbano")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(uRbano)
## basic example code
```

## Sources

`uRbano` pulls building footprints from various releases of Microsoft’s
[Open Building
Footprints](https://github.com/microsoft/GlobalMLBuildingFootprints/tree/main?tab=readme-ov-file)
depending on the region of interest

`uRbano` uses the `osmdata` package to access the OpenStreetMap Overpass
API to import road features for calculating road density metrics
