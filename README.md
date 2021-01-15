
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rbabylon <a href='https:/metrumresearchgroup.github.io/rbabylon'><img src='man/figures/logo.png' align="right" height="120" /></a>

<!-- badges: start -->

[![Build
Status](https://github-drone.metrumrg.com/api/badges/metrumresearchgroup/rbabylon/status.svg)](https://github-drone.metrumrg.com/metrumresearchgroup/rbabylon)
[![codecov](https://codecov.io/gh/metrumresearchgroup/rbabylon/branch/develop/graph/badge.svg)](https://codecov.io/gh/metrumresearchgroup/rbabylon)
<!-- badges: end -->

`rbabylon` is an R interface for running `babylon`. `babylon` is (will
be) a complete solution for managing projects involving modeling and
simulation with a number of software solutions used in pharmaceutical
sciences. Currently, only NONMEM modeling is supported, though there are
plans to add Stan and other modeling software as well. You can get more
detailed information on `babylon` (the underlying CLI tool)
[here](https://github.com/metrumresearchgroup/babylon).

`rbabylon` is intended to help scientists manage the entire modeling
workflow from within R. Users can submit models, consume outputs and
diagnostics, and iterate on models. Furthermore, workflow tools–like
simple tagging of models and model inheritence trees–make
reproducibility and external review much more streamlined.

## Installation

You can install the latests released version of `rbabylon` via [MPN
snapshots](https://mpn.metworx.com/docs/snapshots) from any snapshot
date *after* 2020-03-07.

You can also install development versions of `rbabylon` by downloading
the source files for the latest version from
`https://s3.amazonaws.com/mpn.metworx.dev/releases/rbabylon/` or get the
latest development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("metrumresearchgroup/rbabylon", ref = "develop")
```

## Documentation

You can find documentation and a “Getting Started” vignette that shows
users how to set up `rbabylon` and demonstrates the basic modeling
workflow [here](http://metrumresearchgroup.github.io/rbabylon/).

There are several other vignettes, and more are being added as new
functionality is rolled out. A complete list can be found
[here](https://metrumresearchgroup.github.io/rbabylon/articles/).

### Featured Vignettes

  - [Getting Started with
    rbabylon](https://metrumresearchgroup.github.io/rbabylon/articles/getting-started.html)
    – Some basic scenarios for modeling with NONMEM using `rbabylon`,
    introducing you to its standard workflow and functionality.
  - [Using the based\_on
    field](https://metrumresearchgroup.github.io/rbabylon/articles/using-based-on.html)
    – How to use the `based_on` field to track a model’s ancestry
    through the model development process, as well how to leverage
    `config_log()` to check whether older models are still up-to-date.
  - [Creating a Model Summary
    Log](https://metrumresearchgroup.github.io/rbabylon/articles/using-summary-log.html)
    – How to use `summary_log()` to extract model diagnostics like the
    objective function value, condition number, and parameter counts.
    
## Development

`rbabylon` uses [pkgr](https://github.com/metrumresearchgroup/pkgr) to manage 
development dependencies and [renv](https://rstudio.github.io/renv/) to provide 
isolation. To replicate this environment, 

1. clone the repo

2. install pkgr

3. open package in an R session and run `renv::init()` 
   - install `renv` > 0.8.3-4 into default `.libPaths()` if not already installed

3. run `pkgr install` in terminal within package directory

4. restart session

Then, launch R with the repo as the working directory (open the project in 
RStudio). renv will activate and find the project library.
