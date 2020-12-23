BBI_DEFAULT_PATH <- "/data/apps/bbi"

CACHE_ENV <- new.env(parent = emptyenv())
CACHE_ENV$bbi_exe_paths <- list()

BBI_ARGS = list(
  additional_post_work_envs = list(type = "character", flag="--additional_post_work_envs", description = "Any additional values (as ENV KEY=VALUE) to provide for the post execution environment"),
  background = list(type = "logical", flag="--background", description = "RAW NMFE OPTION - Tells nonmem not to scan StdIn for control characters"),
  clean_lvl = list(type = "numeric", flag="--clean_lvl", description = "clean level used for file output from a given (set of) runs (default 1)"),
  config = list(type = "character", flag="--config", description = "Path (relative or absolute) to another babylon.yaml to load"),
  copy_lvl = list(type = "numeric", flag="--copy_lvl", description = "copy level used for file output from a given (set of) runs"),
  debug = list(type = "logical", flag="--debug", description = "debug mode"),
  delay = list(type = "numeric", flag="--delay", description = "Selects a random number of seconds between 1 and this value to stagger / jitter job execution. Assists in dealing with large volumes of work dealing with the same data set. May avoid NMTRAN issues about not being able read / close files"),
  ext_file = list(type = "character", flag="--ext-file", description = "name of custom ext-file"),
  git = list(type = "logical", flag="--git", description = "whether git is used"),
  json = list(type = "logical", flag="--json", description = "json tree of output, if possible"),
  licfile = list(type = "character", flag="--licfile", description = "RAW NMFE OPTION - Specify a license file to use with NMFE (Nonmem)"),
  log_file = list(type = "character", flag="--log_file", description = "If populated, specifies the file into which to store the output / logging details from Babylon"),
  maxlim = list(type = "numeric", flag="--maxlim", description = "RAW NMFE OPTION - Set the maximum values set for the buffers used by Nonmem (default 100)"),
  mpi_exec_path = list(type = "character", flag="--mpi_exec_path", description = "The fully qualified path to mpiexec. Used for nonmem parallel operations (default '/usr/local/mpich3/bin/mpiexec')"),
  nm_version = list(type = "character", flag="--nm_version", description = "Version of nonmem from the configuration list to use"),
  nm_qual = list(type = "logical", flag="--nmqual", description = "Whether or not to execute with nmqual (autolog.pl"),
  nobuild = list(type = "logical", flag="--nobuild", description = "RAW NMFE OPTION - Skips recompiling and rebuilding on nonmem executable"),
  no_ext_file = list(type = "logical", flag="--no-ext-file", description = "do not use ext file"),
  no_grd_file = list(type = "logical", flag="--no-grd-file", description = "do not use grd file"),
  no_shk_file = list(type = "logical", flag="--no-shk-file", description = "do not use shk file"),
  overwrite = list(type = "logical", flag="--overwrite", description = "Whether or not to remove existing output directories if they are present"),
  parafile = list(type = "character", flag="--parafile", description = "Location of a user-provided parafile to use for parallel execution"),
  parallel = list(type = "logical", flag="--parallel", description = "Whether or not to run nonmem in parallel mode"),
  parallel_timeout = list(type = "numeric", flag="--parallel_timeout", description = "The amount of time to wait for parallel operations in nonmem before timing out (default 2147483647)"),
  post_work_executable = list(type = "character", flag="--post_work_executable", description = "A script or binary to run when job execution completes or fails"),
  prcompile = list(type = "logical", flag="--prcompile", description = "RAW NMFE OPTION - Forces PREDPP compilation"),
  prsame = list(type = "logical", flag="--prsame", description = "RAW NMFE OPTION - Indicates to nonmem that the PREDPP compilation step should be skipped"),
  preview = list(type = "logical", flag="--preview", description = "preview action, but don't actually run command"),
  save_config = list(type = "logical", flag="--save_config", description = "Whether or not to save the existing configuration to a file with the model (default true)"),
  threads = list(type = "numeric", flag="--threads", description = "number of threads to execute with (default 4)"),
  verbose = list(type = "logical", flag="--verbose", description = "verbose output")
)

# S3 classes
NM_MOD_CLASS <- "bbi_nonmem_model"
SUM_CLASS <- "bbi_nonmem_summary"
SL_CLASS <- "bbi_summary_list"
PROC_CLASS <- "babylon_process"
RUN_LOG_CLASS <- "bbi_run_log_df"
CONF_LOG_CLASS <- "bbi_config_log_df"
SUM_LOG_CLASS <- "bbi_summary_log_df"
LOG_DF_CLASS <- "bbi_log_df"

# YAML keys that are hard-coded
YAML_YAML_MD5 <- "yaml_md5"
YAML_DESCRIPTION <- "description"
YAML_BASED_ON <- "based_on"
YAML_TAGS <- "tags"
YAML_NOTES <- "notes"
YAML_DECISIONS <- "decisions"
YAML_BBI_ARGS <- "bbi_args"
YAML_MOD_TYPE <- "model_type"

YAML_REQ_INPUT_KEYS <- c(
  YAML_MOD_TYPE
)

ABS_MOD_PATH <- "absolute_model_path"
RUN_ID_COL <- "run"

# keys required to create a model object
MODEL_REQ_INPUT_KEYS <- c(
  ABS_MOD_PATH,
  YAML_MOD_TYPE
)

# keys required for a model object to have
MODEL_REQ_KEYS <- c(
  ABS_MOD_PATH,
  YAML_YAML_MD5,
  YAML_MOD_TYPE,
  YAML_BBI_ARGS
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
  YAML_DECISIONS
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
  YAML_DECISIONS
)


SUPPORTED_MOD_TYPES <- c("nonmem", "stan")

VALID_MOD_CLASSES <- purrr::map_chr(SUPPORTED_MOD_TYPES,
                                    function(.model_type) {
                                      as.character(glue::glue("bbi_{.model_type}_model"))
                                    })

VALID_SUM_CLASSES <- purrr::map_chr(SUPPORTED_MOD_TYPES,
                                    function(.model_type) {
                                      as.character(glue::glue("bbi_{.model_type}_summary"))
                                    })


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
  "number_of_patients",
  "number_of_obs"
)


ANY_HEURISTICS <- "any_heuristics"

BBI_NULL_NUM <- -999999999
BBI_NULL_STR <- "-999999999"

SL_SUMMARY <- "bbi_summary"
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
  "data_path",
  "data_md5",
  "bbi_version"
)

CONFIG_LOG_REQ_COLS <- c(
  ABS_MOD_PATH,
  RUN_ID_COL,
  CONFIG_KEEPERS
)

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
