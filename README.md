
<!-- README.md is generated from README.Rmd. Please edit that file -->

# orgcube

<!-- badges: start -->

[![R-CMD-check](https://github.com/helseprofil/orgcube/workflows/R-CMD-check/badge.svg)](https://github.com/helseprofil/orgcube/actions)
[![Codecov test
coverage](https://img.shields.io/codecov/c/github/helseprofil/orgcube?logo=codecov)](https://app.codecov.io/gh/helseprofil/orgcube?branch=main)
[![](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![GitHub R package version
(branch)](https://img.shields.io/github/r-package/v/helseprofil/orgcube/main)](https://github.com/helseprofil/orgcube)
[![GitHub R package version
(branch)](https://img.shields.io/github/r-package/v/helseprofil/orgcube/dev)](https://github.com/helseprofil/orgcube)
<!-- badges: end -->

Orgcube replaces
[KHfunctions](https://github.com/helseprofil/khfunctions) as an
extension to [orgdata](https://github.com/helseprofil/orgdata). In a
two-step process, the main functions `make_filegroup()/lag_filgruppe()`
and `make_cube()/lag_kube()` formats original data into publishable
CSV-files to be used for public health profiles.

## Installation

You can install the development version of orgcube from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("helseprofil/orgcube")
```

## Structure

### Main functions

The main functions are `make_filegroup()` and `make_cube()` located in
the files `cube` and `filegroup` in the `R`-folder

### Worker functions

The main functions are powered by a range of supporting functions. These
are located in files with prefix `cube-` and `filegroup-`. Each main
supporting function is powered by working functions with prefix `do_`
that does something to the main data files

### Helper functions

The worker functions are supported by helper functions with prefixes
`find_`/`get_`, `use_`, or `is_` which find/fetches or modify relevant
information, or checks conditions. In the files the helpers are
organized in two levels as `main helpers` (used by the worker functions)
and `other helpers` (used by the main helpers).
