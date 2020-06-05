##############################
# Build or load model object
#############################

#' Create new model object by specifying relevant information as arguments
#' Also creates necessary YAML file for using functions like `add_tags()` and `run_log()` later.
#' @param .yaml_path Path to save resulting model YAML file to. MUST be either an absolute path, or a path relative to the `.directory` argument.
#' @param .description Description of new model run. This will be stored in the yaml (and can be viewed later in `run_log()`). By convention, it should match the $PROBLEM statement in the control stream, but this is not enforced.
#' @param .model_path Path to model (control stream) file. MUST be an absolute path, or the model path relative to the location of the YAML file. It recommended for the control stream and YAML to be in the same directory. If nothing is passed, the function will look for a file with the same path/name as your YAML, but with either .ctl or .mod extension.
#' @param .based_on Character scaler or vector of paths to other models that this model was "based on." These are used to reconstuct model developement and ancestry. \strong{Paths must be relative to `.new_model` path.}
#' @param .tags A character scaler or vector with any user tags to be added to the YAML file
#' @param .bbi_args A named list specifying arguments to pass to babylon formatted like `list("nm_version" = "nm74gf_nmfe", "json" = T, "threads" = 4)`. Run `print_nonmem_args()` to see valid arguments. These will be written into YAML file.
#' @param .model_type Character scaler to specify type of model being created (used for S3 class). Currently only `'nonmem'` is supported.
#' @param .directory Model directory which `.yaml_path` is relative to. Defaults to `options('rbabylon.model_directory')`, which can be set globally with `set_model_directory()`.
#' @importFrom yaml write_yaml
#' @importFrom fs file_exists
#' @return S3 object of class `bbi_{.model_type}_model` that can be passed to `submit_model()`, `model_summary()`, etc.
#' @export
new_model <- function(
  .yaml_path,
  .description,
  .model_path = NULL,
  .based_on = NULL,
  .tags = NULL,
  .bbi_args = NULL,
  .model_type = c("nonmem"),
  .directory = get_model_directory()
) {

  if (!is_valid_yaml_extension(.yaml_path)) {
    warning(glue("Did not pass a YAML extension to .yaml_path. Inferred path `{yaml_ext(.yaml_path)}` from `{.yaml_path}`"))
    .yaml_path <- yaml_ext(.yaml_path)
  }

  # check for .directory and combine with .yaml_path
  .yaml_path <- combine_directory_path(.directory, .yaml_path)

  # check if file already exists
  if (fs::file_exists(.yaml_path)) {
    stop(paste(glue("Passed {.yaml_path} to `new_model(.yaml_path)` but that file already exists."),
               "Either call `read_model()` to load model from YAML or delete the YAML file and try `new_model()` call again."))
  }

  # fill list from passed args
  .mod <- list()
  .mod[[WORKING_DIR]] <- normalizePath(dirname(.yaml_path))
  .mod[[YAML_YAML_NAME]] <- basename(.yaml_path)
  .mod[[YAML_DESCRIPTION]] <- .description
  .mod[[YAML_MOD_TYPE]] <- .model_type
  if (!is.null(.model_path)) .mod[[YAML_MOD_PATH]] <- .model_path
  if (!is.null(.based_on)) .mod[[YAML_BASED_ON]] <- safe_based_on(.mod[[WORKING_DIR]], .based_on)
  if (!is.null(.tags)) .mod[[YAML_TAGS]] <- .tags
  if (!is.null(.bbi_args)) .mod[[YAML_BBI_ARGS]] <- .bbi_args

  # write YAML to disk
  save_model_yaml(.mod, .out_path = .yaml_path)

  # make list into S3 object
  .mod[[YAML_YAML_MD5]] <- digest(file = .yaml_path, algo = "md5")
  .mod <- create_model_object(.mod)

  return(.mod)
}


#' Creates a model object from a YAML model file
#'
#' Parses a model YAML file into a list object that contains correctly formatted information from the YAML
#' and is an S3 object of class `bbi_{.model_type}_model` that can be passed to `submit_model()`, `model_summary()`, etc.
#' @param .path Path to the YAML file to parse. MUST be either an absolute path, or a path relative to the `.directory` argument.
#' @param .directory Model directory which `.path` is relative to. Defaults to `options('rbabylon.model_directory')`, which can be set globally with `set_model_directory()`.
#' @importFrom yaml read_yaml
#' @importFrom digest digest
#' @importFrom fs file_exists
#' @return S3 object of class `bbi_{.model_type}_model`
#' @rdname read_model
#' @export
read_model <- function(
  .path,
  .directory = get_model_directory()
) {

  # check for .directory and combine with .path
  .path <- combine_directory_path(.directory, .path)

  # If not YAML extension, convert to YAML and look for file
  .path <- tryCatch(
    {
      find_yaml_file_path(.path)
    },
    error = function(e) {
      if (str_detect(e$message, FIND_YAML_ERR_MSG)) {
        stop(glue("`read_model()` error: {e$message} -- Use `new_model()` to create the necessary YAML file."))
      }
      stop(e$message)
    }
  )

  # load from file
  yaml_list <- read_yaml(.path)
  yaml_list[[WORKING_DIR]] <- normalizePath(dirname(.path))
  yaml_list[[YAML_YAML_NAME]] <- basename(.path)
  yaml_list[[YAML_YAML_MD5]] <- digest(file = .path, algo = "md5")

  # parse model path
  if (!check_required_keys(yaml_list, .req = YAML_REQ_INPUT_KEYS)) {
    err_msg <- paste0(
      "Model yaml must have keys `", paste(YAML_REQ_INPUT_KEYS, collapse=", "), "` specified in it. ",
      "But `", paste(YAML_REQ_INPUT_KEYS[!(YAML_REQ_INPUT_KEYS %in% names(yaml_list))], collapse=", "), "` are missing. ",
      .path, " has the following keys: ", paste(names(yaml_list), collapse=", ")
    )
    strict_mode_error(err_msg)
  }

  .mod <- create_model_object(yaml_list)

  return(.mod)
}


#' Saves a model model object to a yaml file
#' @param .mod S3 object of class `bbi_{.model_type}_model`
#' @param .out_path Character scaler with path to save out YAML file. By default, sets it to the model file name, with a yaml extension.
#' @importFrom yaml write_yaml
#' @importFrom fs file_exists
#' @importFrom purrr compact
#' @return Output list as specified above.
#' @export
save_model_yaml <- function(.mod, .out_path = NULL) {
  # fill path if null
  if (is.null(.out_path)) {
    .out_path <- get_yaml_path(.mod, .check_exists = FALSE)
  }

  # erase keys that don't need to be saved out
  for (key in YAML_ERASE_OUT_KEYS) {
    .mod[[key]] <- NULL
  }

  # convert keys that need to be coerced to arrays
  for (.key in YAML_SCALER_TO_LIST_KEYS) {
    if (length(.mod[[.key]]) == 1) {
      .mod[[.key]] <- (list(.mod[[.key]]))
    }
  }

  # throw out empty and null keys
  .mod <- purrr::compact(.mod)

  # write to disk
  yaml::write_yaml(.mod, .out_path)
}


#' Convert object to `bbi_{.model_type}_model`
#' @param .obj Object to convert to `bbi_{.model_type}_model`
#' @rdname as_model
#' @export
as_model <- function(.obj) {
  UseMethod("as_model")
}

#' S3 dispatch for passing through `bbi_nonmem_model` object
#' @param .obj `bbi_nonmem_model` object that will be passed through
#' @rdname as_model
#' @export
as_model.bbi_nonmem_model <- function(.obj) {
  return(.obj)
}

#' S3 dispatch for converting `babylon_process` to corresponding `bbi_nonmem_model` object.
#' Only works if YAML and model file are in the same directory with the same name and different file extensions.
#' @param .obj `babylon_process` object to convert
#' @rdname as_model
#' @export
as_model.babylon_process <- function(.obj) {
  # construct path to YAML
  mod_file <- .obj[[PROC_CMD_ARGS]][4] # cmd_args will have c("run", "nonmem", .mode, .model_file)
  yaml_path <- file.path(.obj[[PROC_WD]], mod_file) %>% get_yaml_path()

  # read model from YAML
  .mod <- read_model(yaml_path)
  return(.mod)
}
