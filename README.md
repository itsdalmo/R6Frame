
<!-- README.md is generated from README.Rmd. Please edit that file -->
R6Frame
-------

[![Linux/OSX](https://travis-ci.org/itsdalmo/R6Frame.svg?branch=master)](https://travis-ci.org/itsdalmo/R6Frame) [![Windows](https://ci.appveyor.com/api/projects/status/github/itsdalmo/R6Frame?branch=master&svg=true)](https://ci.appveyor.com/project/itsdalmo/R6Frame) [![Coverage](http://codecov.io/github/itsdalmo/R6Frame/coverage.svg?branch=master)](http://codecov.io/github/itsdalmo/R6Frame?branch=master)

R6Frame is a [R6](https://github.com/wch/R6) wrapper for R's `data.frame` and `data.table`. It supports most base operations for `data.frame`, as well as methods for [dplyr](https://github.com/hadley/dplyr) and [tidyr](https://github.com/hadley/tidyr). Each function call is passed to the `R6Frame$do()` method (`R6Frame$do_merge()` for joins/binds), which allows subclasses of `R6Frame` to have "auto-updating" fields.

Note: This is a work in progress.

Installation
------------

Development version:

``` r
devtools::install_github("itsdalmo/R6Frame")
```

CRAN:

``` r
# Not on CRAN yet.
```
