# bbr 1.3.0

## New features

- Added `param_estimates_compare()` for comparing the result of `param_estimates_batch()` to a single model (or, more generally, for comparing a set of parameter estimates). (#457)

## Bug fixes

- `param_estimates_batch()` now transforms the parameter names that come from `bbi nonmem params ...`, replacing `_` with `,` to match the format that is used elsewhere and expected by downstream tools. (#457)


# bbr 1.2.1

## Docs

* New functions from the 1.2.0 release are now mentioned in the docs. (#440)

## Changes

* `use_bbi()` no longer depends on external tools such as `wget` and `tar`, hopefully improving its reliability across different systems. (#448)

## Bug fixes

* `use_bbi()` printed the wrong current release when an older version was requested. (#144)

* `use_bbi()` and `bbi_version()` crashed if the bbi executable path contained spaces. (#409)

* `model_summary()` failed with an unclear error message when a caller accidentally placed more than one `.lst` file in the model directory. (#449)

* `bbi_help()` had a longstanding regression that prevented it from emitting any output. (#447)

* `bbi nonmem summary` used to fail with an out-of-bounds error when fed `ONLYSIM` output, but, as of bbi v3.1.1, it sets the "only_sim" field.  `print.bbi_nonmem_summary()` and `param_estimates.bbi_nonmem_summary()` have been updated to check for the new field. (#443)


# bbr 1.2.0

This release adds a number of helper functions, primarily for use with NONMEM models.

## New features and changes

* Added `nm_file()`, `nm_grd()`, `nm_tab()`, `nm_par_tab()`, and `nm_data()` for reading in NONMEM files more easily. (#426)

* Added `nm_join()`, `nm_tables()`, and `nm_table_files()` for reading in NONMEM tables more easily. Notably, `nm_join()` can be used to get a single tibble containing your NONMEM input data joined against all of your table outputs. (#429 and #430)

* `param_estimates_batch()` for extracting a tibble of parameter estimates from a batch of NONMEM runs. Especially useful for large batches of runs created by something like a bootstrap. (#386)

* `cov_cor()` and `check_cor_threshold()` for pulling in covariance and correlation matrices from NONMEM `.cov` and `.cor` files. (#414)

* Passing the `.new_model` argument to `copy_model_from()` is now optional. By default, it now tries to increment to the next available integer in the destination directory (the directory containing the parent model). (#424)

* `update_model_id()` for updating mentions of the parent model in the child model's control stream. (#417)

* Per guidance in `bbr 1.0.0` release, `replace_tags()`, `replace_bbi_args()`, `replace_based_on()`, `add_decisions()`, and `replace_decisions()` have been removed.

* Deprecated `check_nonmem_table_output()`, `check_grd()`, and `check_ext()` and replaced them with `nm_file()` variants. These functions will warn about this for two more releases and then begin to error for two more releases before being removed altogether. (#426)

* Deprecated `plot_nonmem_table_df()`, `plot_grd()`, and `plot_ext()` in an effort to more tightly define the scope of `bbr`. These functions will warn about this for two more releases and then begin to error for two more releases before being removed altogether. (#426)

# bbr 1.1.4

## Docs

* Added "Running NONMEM in Parallel: bbr Tips and Tricks" vignette. (#407)

## Bug fixes

* Fixed bug where adding tags as a list instead of a character vector broke downstream functions like `collapse_to_string()` (#393)

# bbr 1.1.3

## Bug fixes

* Fixed a bug where submitting multiple models in a loop with `submit_model(.mode = "local", .wait = FALSE)` would cause the models to never finish because the `bbi` processes would get [killed by `processx`](https://processx.r-lib.org/reference/process.html#cleaning-up-background-processes) when the R objects were garbage collected. (#390)

* `bbr` now checks for a valid configuration file _before_ calling out to `bbi` to avoid the situation where `.wait = FALSE` and the "no config file" error from `bbi` is swallowed. Note, this check is skipped if `.dry_run = TRUE`. (#390)

# bbr 1.1.2

## New features and changes

* Added `options(bbr.bbi_exe_mode)` for globally setting `.mode` argument to `submit_model()` and `submit_models()`. (#377)

* Added `.bbi_args` argument to `bbi_init()` for passing through defaults to be stored in the created `bbi.yaml` file. (#378)

# bbr 1.1.1

## Bug fixes

* There was a bug where submitting more than roughly 250 models at time via `submit_models()` (e.g. for bootstrapping) would hang indefinitely. This had something to do with [a bug in processx](https://github.com/r-lib/processx/issues/286). It was fixed (in `bbr`) by routing the stdout and stderr to a temp file and then reading from it when necessary, instead of relying on `processx` to poll the process. (#374)

# bbr 1.1.0

## New features and changes

* Added `tags_diff()` function for comparing the tags between different models. (#337)

* Added `model_diff()` function for comparing the model files between different models. (#342)

* Added `check_up_to_date()` function for checking whether the model file(s) and data file(s) associated with a model have changed since the model was run.  (#338)

* Added more documentation about the heuristics returned from `model_summary.bbi_nonmem_model()` (#343)

## Bug fixes

* `param_estimates()` now correctly errors when a Bayesian method is used but is _not_ the final method. (#344)

## Developer-facing changes

* Added a `bbi_model` parent class to `bbi_nonmem_model` and `bbi_nonmem_summary` objects. Many of the helpers in `get-path-from-object.R` now dispatch on this class. This had been discussed in the past but was primarily done now in preparation for beginning development for Stan modeling, which will create `bbi_stan_model` and `bbi_stan_summary` objects that will also inherit from this parent class. (#332)


# bbr 1.0.0

This release is fairly small in terms of changes, but it increments to a new major release version primarily because of the name change which happened in `bbr 0.12.0`. The most significant change, from a user perspective, is to the default behavior of where `bbr` looks for `bbi` on the system. This change is described in [issue #321](https://github.com/metrumresearchgroup/bbr/issues/321) and a bit more detail is given below.

The `1.0.0` release also represents a stable feature set of basic NONMEM-related functionality. While there will be more features and development relevant to NONMEM in the future, for the immediate future we are shifting our attention towards building similar functionality to support Stan modeling with `bbr`.

## New features and changes

* The minimum compatible version of `bbi` is increased to `3.0.2`. This is because there was a breaking change in `bbi 3.0.2` which changed all references to `"Patients"` in the `bbi nonmem summary` output to `"Patients"` (discussed further below). Also because there was a bug where `bbi` could not parse the summary output from NONMEM 7.5 and this bug was fixed in `bbi 3.0.1`. 

* `options("bbr.bbi_exe_path")`, which tells `bbr` where to look for a `bbi` installation, now defaults to `"bbi"`. This means that, by default, `bbr` will look for a `bbi` installation in the user's `$PATH`. `options("bbr.bbi_exe_path")` can still be set manually by the user and, in fact, we encourage users to set this to an absolute path in their `.Rprofile` for their project because this explicitly guarantees the correct `bbi` installation is being used. (#322)

* The `use_bbi()` installer function first tries to install to whatever path is set in `options("bbr.bbi_exe_path")`, falling back to the `bbi` in my `$PATH` (accessed via `Sys.which("bbi")`) and then an OS-dependent default, in that order. See `?use_bbi()` for more details. (#322)

* Added print method for `bbi_nonmem_model` object. Similar to the `bbi_nonmem_summary` object, the `bbi_nonmem_model` object should print nicely in the console, and also look good in `.Rmd` chunks with the option `results = 'asis'`. (#307)

* Added `get_data_path()` helper function to extract the absolute path to the input data file from `bbi_nonmem_model` and `bbi_nonmem_summary` objects. (#314)

* Added `build_path_from_model()` helper function to extract the absolute path to various output files from `bbi_nonmem_model` and `bbi_nonmem_summary` objects. (#314)

* The output from `model_summary()` (and `model_summaries()` and `summary_log()`) will now refer to individuals in the data set as `"Subjects"` instead of `"Patients"`, in accordance with the terminology widely used in scientific and medical literature. (#320)

* Per guidance in `rbabylon 0.10.0` release, `replace_tags()`, `replace_bbi_args()`, `replace_based_on()`, `add_decisions()`, and `replace_decisions()` will now error instead of warn about their impending deprecation. These functions will be removed entirely in two more releases.

## Developer-facing changes

* We are no longer checking in either the `.Rprofile` or anything in the `renv` folder. As a result, the development workflow has changed slightly. This change is reflected in the README. (#307 and #308)

* Added an option to suppress the minimum `bbi` version constraint. **This is intended only for developers** who want to try out development (unreleased) version of `bbi` while developing on `bbr`. (#305)

* Our Drone CI system now uses a `.drone.yml` instead of `.drone.jsonette`. We have also switched the containers that we use for testing in CI to smaller containers which are more specialized for the purpose. (#309)

# bbr 0.12.0

**This package has been renamed to from `rbabylon` to `bbr`** and the accompanying command-line tool has been renamed from `babylon` to `bbi` (which was already its alias, used throughout the package). Any mentions of `babylon` and `rbabylon` throughout the package have been renamed accordingly. Mentions of either in the older parts of this `NEWS.md` document were left as is for historical purposes.

## New features and changes

* The minimum compatible version of `bbi` is increased to `3.0.0`.

# rbabylon 0.11.0

## New features and changes

* Added print methods for `bbi_nonmem_summary` and `bbi_process` objects. The `bbi_nonmem_summary` object should print nicely in the console, and also look good in `.Rmd` chunks with the option `results = 'asis'`. (#298 and #294)

* The `bbi_nonmem_summary` object now contains the `absolute_model_path` and we have added the following methods to work on that object (these previously only worked on `bbi_nonmem_model` objects): `get_model_id()`, `get_model_path()`, `get_output_dir()`, `get_yaml_path()`, `check_grd()`, `check_ext()`. (#297)

* For developers, there are now scripts in `data-raw` which regenerate the test reference files and re-run the test models. All test references and example files have now been consolidated in `inst` as well. Users will see these files in `extdata`, `model`, and `test-refs` when the package is installed. (#289)

* Also for developers, we are now using `pkgr` and `renv` to isolate dependencies when developing. The `README` has been updated with instructions for how to use this when developing. (#276)

## Bug fixes

* Previously `replace_model_field()`, which is called under the hood by `replace_tag()` and `replace_note()` did _not_ modify the YAML file as it should have. That has been fixed. (#281)

* Previously the `add_summary()` function would error if all the model summaries errored. Now it will return, passing through the model summary errors to the tibble, as it should. (#282)

# rbabylon 0.10.0

## New features and changes

* Deprecated `replace_tags()`, `replace_bbi_args()`, and `replace_based_on()`
and replaced them with `replace_all_` variants. These functions will warn about
this for two more releases and then begin to error for two more releases before
being removed altogether. (#264)

* Added `replace_tag()` (singular) which replaces a single tag with a new tag.
`replace_note()` (mentioned below) functions the same way. (#264)

* Added `collapse_to_string()` which collapses a list column in a tibble to a
character column containing a string representation of the column's previous
contents. For list columns containing character, numeric, or logical vectors,
this means collapsing with `paste(collapse = ', ')`. For other types `dput()` is
used. (#260)

* Added `run` column to all `bbi_log_df` tibbles. This is always the second
column and is equivalent to `basename(absolute_model_path)`. While,
`absolute_model_path` remains the primary key for the tibble, `run` is useful
for displaying tables when `absolute_model_path` becomes long and difficult to
look at. (#259)

* Added `notes` field to model object (and `bbi_run_log_df`) and associated
helpers `add_notes()`, `replace_note()`, `replace_all_notes()`,
`remove_notes()`. This will replace `decisions`, to reflect the fact that users
will want to use this field throughout the modeling process, not only at the end
once some "decisions" have been reached. `add_decisions()` and
`replace_decisions()` now print a warning telling the user that they will be
deprecated in the future and encouraging use of their `_notes` counterparts.
(#258)
  
* `.description` is no longer a required argument for `new_model()` or
`copy_model_from()`, nor is it a required element of a model object or YAML
file. The helper function `add_description()` has been added to fill the
`description` field. (#267)

* The `.update_model_file` argument to `copy_model_from()` no longer updates the
`$PROB` in the new control stream with the `.description` argument. Instead it
puts the following in `$PROB`: `From rbabylon: see
{get_model_id(.new_model)}.yaml for details` (#265)

* Added `remove_tags()`, `remove_notes()`, and `remove_based_on()` functions for
removing specific strings from the relevant model object field. (#252)
  
## Bug fixes

* `submit_models()` correctly handles the case where bbi arguments are neither
  present in the YAML file nor passed at runtime (#248).

* `config_log()` and `add_config()` are supposed to warn a user if models are
found that do _not_ have a corresponding `bbi_config.json` file (i.e. have not
yet been run, or did not finish successfully). However, if there is no output
directory at all for a given model (it has never been run) then these functions
would error. Now they correctly warn in that scenario. (#253)

# rbabylon 0.9.0

## Breaking changes

* `.base_dir` becomes a required argument to `config_log()`, `run_log()`, and 
  `summary_log()` (#227).

* The `.directory` argument is removed from `copy_model_from()`, 
  `model_summaries()`, `model_summary()`, `new_model()`, `read_model()`, 
  `submit_model()`, and `submit_models()` (#217, #222, #224, #225, #226).
  
* The `.new_model` argument to `copy_model_from()` can be either an absolute 
  path or relative to the location of `.parent_mod` (previously, the path was 
  constructed with `.directory`) (#222).

* The `.yaml_path` argument to `new_model()` becomes `.path`, and the 
  `.model_path` argument is removed, to reflect that a single path identifies 
  the output directory and the model and YAML files (without extension). 
  `new_model()` and `read_model()` throw an error if a model file is not found
  at `.path` plus any relevant file extension, e.g., `ctl` or `mod` for NONMEM 
  (#213, #217).
  
* The default value of the `.config_path` argument to `submit_model()` and 
  `submit_models()` changes to `NULL`, in which case the function will look for
  a `babylon.yaml` in the same directory as the model file (#228).
  
### Removed

* `get_model_directory()` and `set_model_directory()` have been removed because
  the `rbabylon.model_directory` option is no longer used (#229).

* `as_model()` has been removed because it was not used (#194).

* The `character` and `numeric` methods for `copy_model_from()`, 
  `model_summaries()`, `model_summary()`, `submit_model()`, and 
  `submit_models()` have been removed because they created an unnecessarily 
  complicated interface (#188).
  
* `get_path_from_object()` has been removed and replaced by equivalent 
  functionality, e.g., `get_model_path()` (#195).

* `yml_ext()` has been removed because support for the `yml` file extension has 
  been removed (#211).

## New features

* `get_model_path()`, `get_output_dir()`, `get_yaml_path()` return the paths to 
  the model file, the output directory, and the model YAML file, respectively,
  replacing `get_path_from_object()` (#195).

* The object returned by each of `add_config()`, `add_summary()`, 
  `config_log()`, `run_log()`, and `summary_log()` inherits from abstract class
  `bbi_log_df`. `get_model_path()`, `get_output_dir()`, and `get_yaml_path()` 
  gain methods for this new class (#192).

# rbabylon 0.8.0

* Added vignette demonstrating new `summary_log()` functionality.

* Added shrinkage column to the tibble output from `param_estimates.bbi_nonmem_summary()`. 

* Changed the column name containing the names of the parameters, in the table output from `param_estimates.bbi_nonmem_summary()`, from `names` to `parameter_names` to avoid confusion.

* `param_estimates.bbi_nonmem_summary()` now errors with a "not implemented" error for 
  models where the final estimation method is Bayesian.

* Added a `NEWS.md` file to track changes to the package.

* The minimum compatible version of babylon is increased to 2.3.0. 

* The list of arguments that can be passed via `.bbi_args` is updated to reflect
  the possible values as of babylon 2.3.0. Only arguments for `bbi nonmem run`, 
  arguments for `bbi nonmem summary`, and global arguments are included. Also,
  `print_nonmem_args()` is renamed to `print_bbi_args()` to better reflect its 
  purpose (#123).
  
* `config_log()` returns the versions of babylon and NONMEM (#115).

* `config_log()` indicates whether the model file or the data file has changed
  since the model was last run (#30). Updated "Using the based_on field" vignette
  to reflect the new feature and its usage.
  
* Model summaries no longer include information about covariance or correlation,
  because `bbi nonmem summary` no longer parses the relevant files (#128).
  
* Model summary logs, as obtained from `summary_log()` or `add_summary()`, 
  include the condition number, whether a PRDERR file was created, and whether
  any _p_-value is less than 0.05. In addition, the objective function value is
  now included _without_ the constant (#122).
  
* New function `summary_log()` combines summaries of multiple models, as 
  obtained from `model_summaries()`. Its companion `add_summary()` adds the
  summary information to an existing run log (#67).

* Multiple models can be summarized with a call to `model_summaries()` (#53).
