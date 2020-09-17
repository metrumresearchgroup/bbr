# rbabylon 0.8.0

* Added vignette demonstrating new `summary_log()` functionality.

* Added shrinkage column to the tibble output from `param_estimates.bbi_nonmem_summary()`. 

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
