
<!-- README.md is generated from README.Rmd. Please edit that file -->

# opiniontext

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

## Overview

opiniontext is an ongoing project to build a data package containing
text of US Supreme Court opinions available publicly at
SupremeCourt.gov. Text data is to be made readily available in the form
of text documents, csv, and R objects. Csv and R objects outputted will
contain the following columns:

  - Case Name
  - Date
  - Opinion Type
  - Author
  - Text

## Installation

You can install the development version of opiniontext from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("JCRascal/opiniontext")
```

## Data Collection Plan

  - Phase 1: Collect all available slip opinions for the 2019 session
  - Phase 2: Collect all available slip opinions from 2014 session to
    present
  - Phase 3: Collect official bound volumes
