
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CUInetwork

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of CUInetwork is to generate an interactive shiny app that help
visualize the network of codified &/ NLP concepts.

## Installation

To install the latest development version from GitHub:

``` r
install.packages("remotes")
remotes::install_github("xinxiong0238/CUInetwork")
```

If errors like “Error: (converted from warning) package ‘shiny’ was
built under R version 4.0.5” occur, please set:

``` r
Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true")
```

You can refer to [this
website](https://github.com/r-lib/remotes/issues/403) to see more
discussion on that problem.

## Example

This is a basic example which shows you how to run the CUInetwork app.
Remember you need to get access to the data and save it to your local
computer. Remember in order to guarantee some dependencies are loaded,
you must use `library(CUInetwork)` beforehand, instead of directly
running CUInetwork::run\_app(data\_path)!

``` r
library(CUInetwork)
#> Loading required package: shiny
#> Warning: package 'shiny' was built under R version 4.0.5
#> Loading required package: shinyBS
#> Warning: package 'shinyBS' was built under R version 4.0.4
#> Loading required package: Matrix


# data_path = "the path to the app data (RDS format)"
# run_app(data_path)
```
