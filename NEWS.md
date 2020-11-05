# rbabylon (development)

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
