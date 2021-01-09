
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

## Sample Usage

``` r
library(opiniontext)
library(tidytext)
library(dplyr)
library(ggplot2)

case_words <- opinions_2019 %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords(), by = "word") %>%
  count(author, word, sort = TRUE)
  
case_words <- case_words %>%
  group_by(author) %>%
  summarize(total = sum(n)) %>%
  right_join(case_words) %>%
  filter(nchar(word) > 2)

case_plot <- case_words %>%
  bind_tf_idf(word, author, n) %>%
  group_by(author) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, author))

ggplot(case_plot, aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip() +
  scale_x_reordered()
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

## Data Collection Plan

  - ~~Phase 1: Collect all available slip opinions for the 2019
    session~~
  - Phase 2: Collect all available slip opinions from 2014 session to
    present
  - Phase 3: Collect official bound volumes
