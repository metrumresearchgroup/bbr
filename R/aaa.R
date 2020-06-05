CACHE_ENV <- new.env(parent = emptyenv())
CACHE_ENV$bbi_exe_paths <- list()

NONMEM_ARGS = list(
  cache_dir = list(type = "character", flag="--cache_dir", description = "directory path for cache of nonmem executables for NM7.4+"),
  cache_exe = list(type = "character", flag="--cache_exe", description = "name of executable stored in cache"),
  clean_lvl = list(type = "numeric", flag="--clean_lvl", description = "clean level used for file output from a given (set of) runs (default 1)"),
  config = list(type = "character", flag="--config", description = "config file (default is $HOME/babylon.yaml)"),
  copy_lvl = list(type = "numeric", flag="--copy_lvl", description = "copy level used for file output from a given (set of) runs"),
  debug = list(type = "logical", flag="--debug", description = "debug mode"),
  delay = list(type = "numeric", flag="--delay", description = "Selects a random number of seconds between 1 and this value to stagger / jitter job execution. Assists in dealing with large volumes of work dealing with the same data set. May avoid NMTRAN issues about not being able read / close files"),
  git = list(type = "logical", flag="--git", description = "whether git is used"),
  gitignore_lvl = list(type = "numeric", flag="--gitignoreLvl", description = "gitignore lvl for a given (set of) runs"),
  json = list(type = "logical", flag="--json", description = "json tree of output, if possible"),
  mpi_exec_path = list(type = "character", flag="--mpi_exec_path", description = "The fully qualified path to mpiexec. Used for nonmem parallel operations (default '/usr/local/mpich3/bin/mpiexec')"),
  nm_version = list(type = "character", flag="--nm_version", description = "Version of nonmem from the configuration list to use"),
  nm_qual = list(type = "logical", flag="--nmqual", description = "Whether or not to execute with nmqual (autolog.pl"),
  no_cor_file = list(type = "logical", flag="--no-cor-file", description = "do not use cor file"),
  no_cov_file = list(type = "logical", flag="--no-cov-file", description = "do not use cov file"),
  no_ext_file = list(type = "logical", flag="--no-ext-file", description = "do not use ext file"),
  no_grd_file = list(type = "logical", flag="--no-grd-file", description = "do not use grd file"),
  no_shk_file = list(type = "logical", flag="--no-shk-file", description = "do not use shk file"),
  nodes = list(type = "numeric", flag="--nodes", description = "The number of nodes on which to perform parallel operations (default 8)"),
  output_dir = list(type = "character", flag="--output_dir", description = "Go template for the output directory to use for storging details of each executed model (default '{{ .Name}}')"),
  overwrite = list(type = "logical", flag="--overwrite", description = "Whether or not to remove existing output directories if they are present"),
  parafile = list(type = "character", flag="--parafile", description = "Location of a user-provided parafile to use for parallel execution"),
  parallel = list(type = "logical", flag="--parallel", description = "Whether or not to run nonmem in parallel mode"),
  preview = list(type = "logical", flag="--preview", description = "preview action, but don't actually run command"),
  saveConfig = list(type = "logical", flag="--save_config", description = "Whether or not to save the existing configuration to a file with the model (default true)"),
  save_exe = list(type = "character", flag="--save_exe", description = "what to name the executable when stored in cache"),
  threads = list(type = "numeric", flag="--threads", description = "number of threads to execute with (default 4)"),
  timeout = list(type = "numeric", flag="--timeout", description = "The amount of time to wait for parallel operations in nonmem before timing out (default 2147483647)"),
  verbose = list(type = "logical", flag="--verbose", description = "verbose output")
)

# YAML keys that are hard-coded
YAML_MOD_PATH <- "model_path"
YAML_YAML_NAME <- "orig_yaml_file"
YAML_YAML_MD5 <- "yaml_md5"
YAML_DESCRIPTION <- "description"
YAML_BASED_ON <- "based_on"
YAML_TAGS <- "tags"
YAML_DECISIONS <- "decisions"
YAML_BBI_ARGS <- "bbi_args"
YAML_MOD_TYPE <- "model_type"
YAML_OUT_DIR <- "output_dir"

YAML_REQ_INPUT_KEYS <- c(
  YAML_MOD_TYPE,
  YAML_DESCRIPTION
)

WORKING_DIR <- "model_working_dir"
ABS_MOD_PATH <- "absolute_model_path"

# keys required to create a model object
MODEL_REQ_INPUT_KEYS <- c(
  WORKING_DIR,
  YAML_MOD_TYPE,
  YAML_DESCRIPTION
)

# keys required for a model object to have
MODEL_REQ_KEYS <- c(
  WORKING_DIR,
  YAML_YAML_NAME,
  YAML_YAML_MD5,
  YAML_MOD_TYPE,
  YAML_DESCRIPTION,
  YAML_MOD_PATH,
  YAML_BBI_ARGS,
  YAML_OUT_DIR
)


# columns required for a run log df
RUN_LOG_REQ_COLS <- c(
  ABS_MOD_PATH,
  YAML_YAML_MD5,
  YAML_MOD_TYPE,
  YAML_DESCRIPTION,
  YAML_BBI_ARGS,
  YAML_BASED_ON,
  YAML_TAGS,
  YAML_DECISIONS
)


# keys that get erased when saving a model YAML on disk
YAML_ERASE_OUT_KEYS <- c(
  WORKING_DIR,
  YAML_YAML_NAME,
  YAML_YAML_MD5,
  YAML_OUT_DIR
)

# keys that need to be coerced to arrays when saving a model YAML on disk
YAML_SCALER_TO_LIST_KEYS <- c(
  YAML_BASED_ON,
  YAML_TAGS,
  YAML_DECISIONS
)


SUPPORTED_MOD_TYPES <- c("nonmem", "stan")
VALID_MOD_CLASSES <- purrr::map_chr(SUPPORTED_MOD_TYPES,
                                    function(.model_type) {
                                      as.character(glue::glue("bbi_{.model_type}_model"))
                                    })

SUMMARY_DETAILS <- "run_details"

# keys required for a summary object to have
SUMMARY_REQ_KEYS <- c(
  SUMMARY_DETAILS
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
FIND_YAML_ERR_MSG <- "No file found at.+\\.yml.+OR.+\\.yaml"
