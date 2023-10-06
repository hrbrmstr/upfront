
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Signed
by](https://img.shields.io/badge/Keybase-Verified-brightgreen.svg)](https://keybase.io/hrbrmstr)
![Signed commit
%](https://img.shields.io/badge/Signed_Commits-100%25-lightgrey.svg)

![Minimal R
Version](https://img.shields.io/badge/R%3E%3D-4.0.0-blue.svg)
![License](https://img.shields.io/badge/License-MIT-blue.svg)

# upfront

Base R Convenience Wrappers That Put Data Up Front

## Description

A series of functions that help use the new built-in pipe operator in R
without the need for the heft of the tidyverse.

## What’s Inside The Tin

The following functions are implemented:

- `arrange`: Convenience data frame sorter
- `bind_cols`: More convenient cbind.data.frame
- `bind_rows`: More convenient do.call(rbind.data.frame, …)
- `capture_error`: More convenient capture.output
- `capture_output`: More convenient capture.output
- `compact`: Discard empty elements
- `count`: Convenience aggregate
- `dfilter`: Convenience subset
- `discard`: Discard elements based on condition
- `dselect`: Convenience extract
- `%l0%`: Infix length 0
- `is_empty`: Infix is empty
- `is_even`: Infix is even
- `is_false`: Infix is false
- `is_negative`: Infix is negative
- `is_odd`: Infix is odd
- `is_positive`: Infix is positive
- `is_true`: Infix is TRUE
- `is_zero`: Infix is zero
- `keep`: Keep elements based on condition
- `map_chr`: Typed Map
- `map_dbl`: Typed Map
- `map_df`: Typed more convenient Map
- `map_dfc`: Typed more convenient Map
- `map_dfr`: Typed more convenient Map
- `map_int`: Typed Map
- `map_lgl`: Typed Map
- `map`: More convenient Map
- `map2_chr`: Typed more convenient Map
- `map2_dbl`: Typed more convenient Map
- `map2_df`: Typed more convenient Map
- `map2_dfc`: Typed more convenient Map
- `map2_int`: Typed more convenient Map
- `map2_lgl`: Typed more convenient Map
- `map2`: More convenient mapply
- `possibly`: More convient try
- `quietly`: More convenient suppressWarmings
- `safely`: More convenient try
- `set_names`: Set object names \#’ @export
- `walk`: Less noisy lapply
- `walk2`: Less noisy, more convenient lapply

## Installation

``` r
remotes::install_github("hrbrmstr/upfront")
```

NOTE: To use the ‘remotes’ install options you will need to have the
[{remotes} package](https://github.com/r-lib/remotes) installed.

## Usage

``` r
library(upfront)

# current version
packageVersion("upfront")
## [1] '0.1.0'
```

## upfront Metrics

| Lang | \# Files |  (%) | LoC | (%) | Blank lines | (%) | \# Lines | (%) |
|:-----|---------:|-----:|----:|----:|------------:|----:|---------:|----:|
| R    |        3 | 0.38 | 291 | 0.5 |          75 | 0.5 |      103 | 0.5 |
| YAML |        1 | 0.12 |   1 | 0.0 |           0 | 0.0 |        0 | 0.0 |
| SUM  |        4 | 0.50 | 292 | 0.5 |          75 | 0.5 |      103 | 0.5 |

{cloc} 📦 metrics for upfront

## Code of Conduct

Please note that this project is released with a Contributor Code of
Conduct. By participating in this project you agree to abide by its
terms.
