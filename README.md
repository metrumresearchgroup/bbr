
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bbr <a href='https:/metrumresearchgroup.github.io/bbr'><img src='man/figures/logo.png' align="right" height="120" /></a>

<!-- badges: start -->

[![Build
Status](https://github-drone.metrumrg.com/api/badges/metrumresearchgroup/bbr/status.svg)](https://github-drone.metrumrg.com/metrumresearchgroup/bbr)
[![codecov](https://codecov.io/gh/metrumresearchgroup/bbr/branch/main/graph/badge.svg)](https://codecov.io/gh/metrumresearchgroup/bbr)
<!-- badges: end -->

`bbr` is an R interface for running `bbi`. Together they provide a
solution for managing projects involving modeling and simulation with a
number of software solutions used in pharmaceutical sciences. Currently,
only NONMEM modeling is supported, though we are in the process of Stan
with plans for other modeling software as well. You can get more
detailed information on `bbi` (the underlying CLI tool)
[here](https://github.com/metrumresearchgroup/bbi).

`bbr` is intended to help scientists manage the entire modeling workflow
from within R. Users can submit models, consume outputs and diagnostics,
and iterate on models. Furthermore, workflow tools–like simple tagging
of models and model inheritence trees–make reproducibility and external
review much more streamlined.

## Installation

You can install the latest released version of `bbr` via [MPN
snapshots](https://mpn.metworx.com/docs/snapshots) from any snapshot
date in 2021 or later. (An earlier version of this package was available
under the name `rbabylon` in snapshot dates 2020-03-07 through
2020-12-21.)

You can also install development versions of `bbr` by downloading the
source files for the latest version from
`https://s3.amazonaws.com/mpn.metworx.dev/releases/bbr/` or get the
latest development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("metrumresearchgroup/bbr", ref = "main")
```

## Documentation

You can find documentation and a “Getting Started” vignette that shows
users how to set up `bbr` and demonstrates the basic modeling workflow
[here](http://metrumresearchgroup.github.io/bbr/).

There are several other vignettes, and more are being added as new
functionality is rolled out. A complete list can be found
[here](https://metrumresearchgroup.github.io/bbr/articles/).

### Cheat Sheet

<a href="https://metrumresearchgroup.github.io/cheatsheets/bbr_nonmem_cheat_sheet.pdf"><img src="https://metrumresearchgroup.github.io/cheatsheets/thumbnails/bbr_nonmem_cheat_sheet_thumbnail.png" width="700" height="395"/></a>

### Featured Vignettes

-   [Getting Started with
    bbr](https://metrumresearchgroup.github.io/bbr/articles/getting-started.html)
    – Some basic scenarios for modeling with NONMEM using `bbr`,
    introducing you to its standard workflow and functionality.
-   [Using the based_on
    field](https://metrumresearchgroup.github.io/bbr/articles/using-based-on.html)
    – How to use the `based_on` field to track a model’s ancestry
    through the model development process, as well how to leverage
    `config_log()` to check whether older models are still up-to-date.
-   [Creating a Model Summary
    Log](https://metrumresearchgroup.github.io/bbr/articles/using-summary-log.html)
    – How to use `summary_log()` to extract model diagnostics like the
    objective function value, condition number, and parameter counts.

## Development

`bbr` uses [pkgr](https://github.com/metrumresearchgroup/pkgr) to manage
development dependencies and [renv](https://rstudio.github.io/renv/) to
provide isolation. To replicate this environment,

1.  clone the repo

2.  install pkgr

3.  open package in an R session and run `renv::init(bare = TRUE)`

    -   install `renv` \> 0.8.3-4 into default `.libPaths()` if not
        already installed

4.  run `pkgr install` in terminal within package directory

5.  restart session

Then, launch R with the repo as the working directory (open the project
in RStudio). renv will activate and find the project library.
