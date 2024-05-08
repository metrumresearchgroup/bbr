ON_WINDOWS <- identical(.Platform$OS.type, "windows")
BBI_DEFAULT_PATH <- if (ON_WINDOWS) {
  "bbi.exe"
} else {
  "bbi"
}

BBI_VALID_MODES <- c("local", "sge")

CACHE_ENV <- new.env(parent = emptyenv())
CACHE_ENV$bbi_exe_paths <- list()

#' List of valid arguments bbi arguments
#'
#' These values can be passed via the `.bbi_args` argument of [submit_model()]
#' or [model_summary()]. This is exported for the benefit of RStudio users that
#' may prefer to inspect `View(bbr::BBI_ARGS)` instead of the [print_bbi_args()]
#' output.
#'
#' @export
#' @keywords internal
BBI_ARGS = list(
  additional_post_work_envs = list(
    type = "character",
    flag = "--additional_post_work_envs",
    description = "Any additional values (as ENV KEY=VALUE) to provide for the post execution environment"
  ),
  background = list(
    type = "logical",
    flag = "--background",
    description = "RAW NMFE OPTION - Tells nonmem not to scan StdIn for control characters"
  ),
  clean_lvl = list(
    type = "numeric",
    flag = "--clean_lvl",
    description = "clean level used for file output from a given (set of) runs (default 1)"
  ),
  config = list(
    type = "character",
    flag = "--config",
    description = "Path (relative or absolute) to another bbi.yaml to load"
  ),
  copy_lvl = list(
    type = "numeric",
    flag = "--copy_lvl",
    description = "copy level used for file output from a given (set of) runs"
  ),
  debug = list(
    type = "logical",
    flag = "--debug",
    description = "debug mode"
  ),
  delay = list(
    type = "numeric",
    flag = "--delay",
    description = "Selects a random number of seconds between 1 and this value to stagger / jitter job execution. Assists in dealing with large volumes of work dealing with the same data set. May avoid NMTRAN issues about not being able read / close files"
  ),
  ext_file = list(
    type = "character",
    flag = "--ext-file",
    description = "name of custom ext-file"
  ),
  git = list(
    type = "logical",
    flag = "--git",
    description = "whether git is used"
  ),
  json = list(
    type = "logical",
    flag = "--json",
    description = "json tree of output, if possible"
  ),
  licfile = list(
    type = "character",
    flag = "--licfile",
    description = "RAW NMFE OPTION - Specify a license file to use with NMFE (Nonmem)"
  ),
  log_file = list(
    type = "character",
    flag = "--log_file",
    description = "If populated, specifies the file into which to store the output / logging details from bbi"
  ),
  maxlim = list(
    type = "numeric",
    flag = "--maxlim",
    description = "RAW NMFE OPTION - Set the maximum values for the buffers used by Nonmem (if 0, don't pass -maxlim to nmfe) (default 2)",
    compatibility_note = "Default changed from unset to 2 with bbi v3.2.0"
  ),
  mpi_exec_path = list(
    type = "character",
    flag = "--mpi_exec_path",
    description = "The fully qualified path to mpiexec. Used for nonmem parallel operations (default '/usr/local/mpich3/bin/mpiexec')"
  ),
  nm_version = list(
    type = "character",
    flag = "--nm_version",
    description = "Version of nonmem from the configuration list to use"
  ),
  nm_qual = list(
    type = "logical",
    flag = "--nmqual",
    description = "Whether or not to execute with nmqual (autolog.pl)"
  ),
  nobuild = list(
    type = "logical",
    flag = "--nobuild",
    description = "RAW NMFE OPTION - Skips recompiling and rebuilding on nonmem executable"
  ),
  no_ext_file = list(
    type = "logical",
    flag = "--no-ext-file",
    description = "do not use ext file"
  ),
  no_grd_file = list(
    type = "logical",
    flag = "--no-grd-file",
    description = "do not use grd file"
  ),
  no_shk_file = list(
    type = "logical",
    flag = "--no-shk-file",
    description = "do not use shk file"
  ),
  overwrite = list(
    type = "logical",
    flag = "--overwrite",
    description = "Whether or not to remove existing output directories if they are present"
  ),
  parafile = list(
    type = "character",
    flag = "--parafile",
    description = "Location of a user-provided parafile to use for parallel execution"
  ),
  parallel = list(
    type = "logical",
    flag = "--parallel",
    description = "Whether or not to run nonmem in parallel mode"
  ),
  parallel_timeout = list(
    type = "numeric",
    flag = "--parallel_timeout",
    description = "The amount of time to wait for parallel operations in nonmem before timing out (default 2147483647)"
  ),
  post_work_executable = list(
    type = "character",
    flag = "--post_work_executable",
    description = "A script or binary to run when job execution completes or fails"
  ),
  prcompile = list(
    type = "logical",
    flag = "--prcompile",
    description = "RAW NMFE OPTION - Forces PREDPP compilation"
  ),
  prdefault = list(
    type = "logical",
    flag = "--prdefault",
    description = "RAW NMFE OPTION - Do not recompile any routines other than FSUBS",
    compatibility_note = "This option isn't available in bbi until v3.2.0"
  ),
  prsame = list(
    type = "logical",
    flag = "--prsame",
    description = "RAW NMFE OPTION - Indicates to nonmem that the PREDPP compilation step should be skipped"
  ),
  preview = list(
    type = "logical",
    flag = "--preview",
    description = "preview action, but don't actually run command"
  ),
  save_config = list(
    type = "logical",
    flag = "--save_config",
    description = "Whether or not to save the existing configuration to a file with the model (default true)"
  ),
  threads = list(
    type = "numeric",
    flag = "--threads",
    description = "number of threads to execute with locally or nodes to execute on in parallel (default 4)"
  ),
  tprdefault = list(
    type = "logical",
    flag = "--tprdefault",
    description = "RAW NMFE OPTION - Test if is okay to do -prdefault",
    compatibility_note = "This option isn't available in bbi until v3.2.0"
  ),
  verbose = list(
    type = "logical",
    flag = "--verbose",
    description = "verbose output"
  )
)

# S3 classes
#
# The underlying names for BBI_PARENT_CLASS and BBI_BASE_MODEL_CLASS are
# unfortunate. BBI_PARENT_CLASS ("bbi_model") came first and is used for regular
# models _and_ summary objects. BBI_BASE_MODEL_CLASS ("bbi_base_model") came
# later and applies only to regular models, not summary objects.
BBI_PARENT_CLASS <- "bbi_model" # SHARED with bbr.bayes
BBI_BASE_MODEL_CLASS <- "bbi_base_model"
NM_MOD_CLASS <- "bbi_nonmem_model" # SHARED with bbr.bayes
NM_SUM_CLASS <- "bbi_nonmem_summary" # SHARED with bbr.bayes
NMBOOT_MOD_CLASS <- "bbi_nmboot_model"
NMBOOT_SUM_CLASS <- "bbi_nmboot_summary"
NMSIM_MOD_CLASS <- "bbi_nmsim_model"
NMSIM_SUM_CLASS <- "bbi_nmsim_summary"
SL_CLASS <- "bbi_summary_list"
PROC_CLASS <- "bbi_process"
RUN_LOG_CLASS <- "bbi_run_log_df"
CONF_LOG_CLASS <- "bbi_config_log_df"
SUM_LOG_CLASS <- "bbi_summary_log_df"
LOG_DF_CLASS <- "bbi_log_df"

# YAML keys that are hard-coded
YAML_YAML_MD5 <- "yaml_md5"
YAML_DESCRIPTION <- "description" # SHARED with bbr.bayes
YAML_BASED_ON <- "based_on" # SHARED with bbr.bayes
YAML_TAGS <- "tags" # SHARED with bbr.bayes
YAML_NOTES <- "notes"
YAML_BBI_ARGS <- "bbi_args"
YAML_MOD_TYPE <- "model_type" # SHARED with bbr.bayes
YAML_STAR <- 'star'

YAML_REQ_INPUT_KEYS <- c(
  YAML_MOD_TYPE
)

ABS_MOD_PATH <- "absolute_model_path" # SHARED with bbr.bayes
RUN_ID_COL <- "run" # SHARED with bbr.bayes

# keys required to create a model object
MODEL_REQ_INPUT_KEYS <- c(
  ABS_MOD_PATH,
  YAML_MOD_TYPE
)

# keys required for a model object to have
MODEL_REQ_KEYS <- c(
  ABS_MOD_PATH,
  YAML_YAML_MD5,
  YAML_MOD_TYPE
)

# columns required for a run log df
RUN_LOG_REQ_COLS <- c(
  ABS_MOD_PATH,
  RUN_ID_COL,
  YAML_YAML_MD5,
  YAML_MOD_TYPE,
  YAML_DESCRIPTION,
  YAML_BBI_ARGS,
  YAML_BASED_ON,
  YAML_TAGS,
  YAML_NOTES,
  YAML_STAR
)


# keys that get erased when saving a model YAML on disk
YAML_ERASE_OUT_KEYS <- c(
  ABS_MOD_PATH,
  YAML_YAML_MD5
)

# keys that need to be coerced to arrays when saving a model YAML on disk
YAML_SCALAR_TO_LIST_KEYS <- c(
  YAML_BASED_ON,
  YAML_TAGS,
  YAML_NOTES,
  YAML_STAR
)

SUMMARY_DETAILS <- "run_details"
SUMMARY_HEURISTICS <- "run_heuristics"
SUMMARY_COND_NUM <- "condition_number"
SUMMARY_PARAM_NAMES <- "parameter_names"
SUMMARY_PARAM_DATA <- "parameters_data"
SUMMARY_EST_METHOD <- "estimation_method"
SUMMARY_SHRINKAGE <- "shrinkage_details"
SUMMARY_PARAM_DIAG <- "diag"
SUMMARY_PARAM_SHRINKAGE <- "shrinkage"
SUMMARY_SHRINKAGE_OMEGA <- "eta_sd"
SUMMARY_SHRINKAGE_SIGMA <- "eps_sd"
SUMMARY_PARAM_PVAL <- "pval"
SUMMARY_SHRINKAGE_PVAL <- "pval"
SUMMARY_PARAM_ETASIG <- "ETASIG"

CONFIG_MODEL_PATH <- "model_path"
CONFIG_MODEL_MD5 <- "model_md5" # SHARED with bbr.bayes
CONFIG_DATA_PATH <- "data_path" # SHARED with bbr.bayes
CONFIG_DATA_MD5 <- "data_md5" # SHARED with bbr.bayes

# keys required for a summary object to have
SUMMARY_REQ_KEYS <- c(
  ABS_MOD_PATH,
  SUMMARY_DETAILS,
  SUMMARY_HEURISTICS
)

OFV_COL <- "ofv"
PARAM_COUNT_COL <- "param_count"

DETAILS_ELEMENTS <- c(
  "estimation_method",
  "problem_text",
  "number_of_subjects",
  "number_of_obs"
)


ANY_HEURISTICS <- "any_heuristics"

BBI_NULL_NUM <- -999999999
BBI_NULL_STR <- "-999999999"

SL_SUMMARY <- "bbi_summary" # SHARED with bbr.bayes
SL_ERROR <- "error_msg"
SL_FAIL_FLAGS <- "needed_fail_flags"

SUMMARY_LIST_REQ_KEYS <- c(
  ABS_MOD_PATH,
  SL_SUMMARY,
  SL_ERROR,
  SL_FAIL_FLAGS
)

# columns required for a summary log df
SUMMARY_LOG_REQ_COLS <- c(
  ABS_MOD_PATH,
  RUN_ID_COL,
  SL_SUMMARY,
  SL_ERROR,
  SL_FAIL_FLAGS,
  DETAILS_ELEMENTS,
  PARAM_COUNT_COL,
  OFV_COL,
  ANY_HEURISTICS
)

# define json keys to keep as from bbi_config.json
CONFIG_KEEPERS <- c(
  "model_md5",
  CONFIG_DATA_PATH,
  "data_md5",
  "bbi_version"
)

CONFIG_LOG_REQ_COLS <- c(
  ABS_MOD_PATH,
  RUN_ID_COL,
  CONFIG_KEEPERS
)

TAGS_ADD <- "tags_added"
TAGS_REM <- "tags_removed"

# keys added when creating a process object
PROC_PROCESS <- "process"
PROC_STDOUT <- "stdout"
PROC_BBI <- "bbi"
PROC_CMD_ARGS <- "cmd_args"
PROC_WD <- "working_dir"
PROC_CALL <- "call"


# keys required for process object to have
PROCESS_REQ_KEYS <- c(
  PROC_PROCESS,
  PROC_STDOUT,
  PROC_BBI,
  PROC_CMD_ARGS,
  PROC_WD
)

# error messages that we grep for
NO_NONMEM_ERR_MSG <- "No version was supplied and no default value exists in the configset"
MOD_ALREADY_EXISTS_ERR_MSG <- "already exist, but we are configured not to overwrite"
NO_STAN_ERR_MSG <- "stan support not yet implemented."
PARAM_BAYES_ERR_MSG <- "param_estimates() is not currently implemented for Bayesian methods."
CHECK_UP_TO_DATE_ERR_MSG <- "Cannot check if up-to-date because model has not been run yet."
MODEL_DIFF_ERR_MSG <- "Please pass a single `bbi_model` object to the `.mod2` to compare models."
BBI_EXE_MODE_NULL_ERR_MSG <- paste(
  "Nothing was passed to `.mode` argument and `options('bbr.bbi_exe_mode')` is NULL. Please either pass or set to one of:",
  paste(BBI_VALID_MODES, collapse = ", ")
)
BBI_EXE_MODE_INVALID_ERR_MSG <- paste(
  "Invalid value passed to `.mode` argument. Please either pass or set `options('bbr.bbi_exe_mode')` to one of:",
  paste(BBI_VALID_MODES, collapse = ", ")
)
NONMEM_MODEL_TYPE_ERR_MSG <- "IF THIS IS NOT A NONMEM MODEL please pass the appropriate type to `.model_type`"

# Supported parameter estimates to inherit from parent model
BBR_ESTIMATES_INHERIT <- c("theta", "sigma", "omega")
