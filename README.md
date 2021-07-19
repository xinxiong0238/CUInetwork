
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
running CUInetwork::run\_app(data\_path)\!

``` r
library(CUInetwork)
data_path = "the path to the app data (RDS format)"
run_app(data_path)
```

## App instructions

To have a general understanding on how to get the CUInetwork app work,
we show some main steps here. If you want to know some detailed app
functionality, please refer to the ‘Get started’ tab.

### Step 1: choose your center node(s)

First of all, you need to specify your center node by clicking a row in
the `Possible inputs` table containing in the dropdown button. Multiple
inputs are acceptable. You can also use the search bar on the top or the
filtering bar for each column (`id`, `terms`, `group`) to spot on your
targets. A second round of searching will not mask the previous
selection, which will be kept unless you de-select the row or use the
“Unselect” button on the navigation bar. Once some rows are selected,
the app will automatically display them as selected CUIs or nonCUI codes
on the left.

![Step 1](img/step1.png)

### Step 2: show the connected network

When you select your interested center node(s), simply click the green
“Show” button to take a look at the connected network. Remember only
nodes directly linked to your center node(s) will be shown. For example
the following network only draws CUIs or codified concepts directly
connected to phecode 174.1 or phecode 174.2. In terms of nodes that have
indirect connection like A \<-\> intermediate node \<-\> center node,
they are not in the interests of this app.

![Step 1](img/step2.png)

### Step 3: explore network information
