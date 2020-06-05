
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rbabylon

<!-- badges: start -->

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
    through the model development process, as well how to leverage md5
    digests to check whether older models are still up-to-date.
  - [rbabylon Parameter
    Labels](https://metrumresearchgroup.github.io/rbabylon/articles/parameter-labels.html)
    – Extracting parameter estimates and labels into a table that can be
    used for diagnostics, or to generate reports.
