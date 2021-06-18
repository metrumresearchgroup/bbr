#' @title `bbi_stan_model` object
#'
#' @description This page describes the basic structure of the `bbi_stan_model`
#'   object which is used for defining and submitting models with Stan and
#'   `bbr`. The Details section contains information about the model structure
#'   and the necessary files that will exist on disk for any `bbi_stan_model`.
#'
#' @details
#'
#' **Model Structure**
#'
#' **`<run>`** - The "run" is, in some sense, the “name” of a given model.
#' Practically, it will correspond to the model directory name, the base name of
#' the bbr-created YAML (`<run>.yaml`), as well as the base name for some files
#' in that directory. Calling [get_model_id()] on a model object will return
#' `<run>` as a string. The `bbi_log_df` tibbles also all contain a run column
#' which is populated by calling `basename(.mod$absolute_model_path)` for each
#' model. Note: this is _not_ actually stored in the model object because it can
#' be unequivocally extracted as just described.
#'
#' **`absolute_model_path`** - Like the `bbi_nonmem_model`, the `bbi_stan_model`
#' will carry around only an absolute path to the model directory. This will
#' point to the model directory (named `<run>`) containing all of the files
#' described below, as well as a `<run>.yaml` file that `bbr` uses to persist
#' model metadata. A model is loaded or created by passing a relative path to
#' this directory to either [read_model()] or [new_model()], both of which
#' return the `bbi_stan_model` object. When this object is created, it checks
#' the model directory for the relevant files and populates
#' `absolute_model_path`.
#'
#' **Necessary Files**
#'
#' All of the files described below will exist inside the model directory named
#' `<run>`. If you call `new_model(..., .model_type = "stan")` without any of
#' these files, template "scaffold" files for all of them will be created in the
#' newly created model directory.
#'
#' **`<run>.stan`** - The Stan file.
#'
#' **`<run>-standata.R`** - This file contains all necessary R code to read in
#' any source data and transform them to a Stan-ready data object (list).
#'
#' * Contains only one function, called `make_standata(.dir)`, that takes a
#' single argument and returns the data list to pass to `cmdstanr::sample()`.
#'
#' * The `.dir` argument will be the directory containing the script. This is
#' used to find data files for loading, for example `read_csv(file.path(.dir,
#' “..”, “..”, “data”, “derived”, “my_data.csv”))`
#'
#' * Can be called (by `bbr::build_data()`) to generate the data for model
#' submission or to compare the resulting data to previously saved data on disk.
#'
#' **Other Files and Directories**
#'
#' There will be several other things created in the model directory, as the
#' model is run or as it prepares to run.
#'
#' **`<run>-init.R`** - This file contains all necessary R code to create the
#' initial values passed to `cmdstanr::sample()`. This file is a lot like
#' `<run>-standata.R` (discussed above) and a scaffold can be created with
#' [add_stan_init()]. However, this file is _not_ necessary. If it is missing,
#' Stan will fall back to the [default initial
#' values](https://mc-stan.org/docs/2_25/reference-manual/initialization.html#random-initial-values).
#'
#' * Contains only one function, called `make_init(.data)`, that takes a single
#' argument and returns something that can be passed to the `init` argument of
#' `cmdstanr::sample()`. There are several options; see `?cmdstanr::sample` for
#' details.
#'
#' * The object returned from `make_standata()` will be passed to the `.data`
#' argument of `make_init()`.
#'
#' * Will be called internally by `bbr` and the result passed to
#' `cmdstanr::sample(init)`
#'
#' * Note that one of the options `cmdstanr::sample(init)` is to pass _"A
#' function that returns a single list..."_. If you intend to use this option,
#' your `make_init()` function must return _the function_ described, _not_ the
#' "single list...".
#'
#' **`<run>-stanargs.R`** - A file to capture the arguments passed into
#' `cmdstanr::sample()` (`nChains`, etc.) through [bbr::submit_model()]. This
#' will be written to file with `dput()` so that it can be compared, and
#' possibly re-used, in the future.
#'
#' **`<run>`** - This is the binary file created when the `<run>.stan` file is
#' compiled by `cmdstan`. We `.gitignore` this automatically.
#'
#' **`<run>-output`** - This directory is created by `bbr`. It is where the
#' posteriors will be saved (currently as CSV’s) and also where the
#' `bbi_config.json` is saved when the model run finishes successfully. Note
#' that we _don’t_ call this `<run>` (as is done in NONMEM) for two primary
#' reasons:
#'
#' * It is more informative to call it `<run>-output` to distinguish it from all
#' the other files and directories that start with `<run>`.
#'
#' * There is also the binary called `<run>` (previously mentioned) that could
#' cause confusion. In fact, there was a bug in `cmdstanr` in February 2021
#' involving exactly this scenario.
#'
#' `<run>-output/bbi_config.json` - This file is created by `bbr` when a model
#' run finishes successfully. It stores some configuration information about the
#' run, as well as the md5 hashes of the necessary files. These hashes are later
#' used (by `bbr::check_up_to_date()` to check whether the files have changed
#' since the model was run, primarily for reproducibility purposes.
#'
#' **Some Helper Functions**
#'
#' * **[check_stan_model()]** (mentioned above) - Checks for the necessary files
#' before running or copying the model.
#'
#' * **[build_path_from_model()]** - Builds the absolute path a file in the
#' model folder from a model object and a suffix.
#'
#' * **[add_stan_file()], [add_standata_file()], [add_stan_init()]** - Helpers
#' for adding one of the necessary files to the model folder.
#'
#' * **[model_diff()]** - Compare necessary files between two models. Defaults
#' to comparing `<run>.stan` files.
#'
#' * Also has many of the same helpers as `bbi_nonmem_model` objects:
#' [tags_diff()], [add_tags()], [add_notes()], [get_model_path()],
#' [get_output_dir()], [get_model_id()]
#' @name bbi_stan_model
NULL
