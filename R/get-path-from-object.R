
#' Get path from bbi object
#'
#' `bbi_{.model_type}_model` objects correspond to multiple files on disk. These functions
#' provide an easy way to retrieve the absolute path to these files.
#'
#' @details
#' **`get_model_path()`** returns the path to the model definition file.
#'   For NONMEM models, this is the control stream. For Stan models,
#'   this is the `.stan` file.
#'
#' **`get_output_dir()`** returns the path to the directory containing
#'   output files created when the model is run.
#'
#' **`get_yaml_path()`** returns the path to the YAML file created by bbr.
#'   This file contains metadata like tags, etc. and should, generally speaking,
#'   _not_ be interacted with directly. Use the helper functions mentioned in the
#'   [modify_model_field()] help page to modify this file for you.
#'
#' Note that [build_path_from_model()] and [get_data_path()] provide similar functionality, but have a slightly
#' different interface and implementation and so are documented separately.
#'
#' @param .bbi_object The object to query. Could be
#' a `bbi_{.model_type}_model` object,
#' `bbi_{.model_type}_summary` object,
#' or a tibble of class `bbi_log_df`.
#' @param .check_exists If `TRUE`, the default, will throw an error if the file does not exist
#' @name get_path_from_object
#' @seealso [get_model_id()] [get_data_path()] [build_path_from_model()]
NULL


#' @rdname get_path_from_object
#' @export
get_model_path <- function(.bbi_object, .check_exists = TRUE) {
  UseMethod("get_model_path")
}

#' @export
get_model_path.bbi_model <- function(.bbi_object, .check_exists = TRUE) {
  get_model_path_nonmem(.bbi_object, .check_exists)
}

#' @export
get_model_path.bbi_nonmem_summary <- function(.bbi_object, .check_exists = TRUE) {
  get_model_path_nonmem(.bbi_object, .check_exists)
}

#' @export
get_model_path.bbi_log_df <- function(.bbi_object, .check_exists = TRUE) {
  get_path_from_log_df(.bbi_object, get_model_path, .check_exists = .check_exists)
}


#' @rdname get_path_from_object
#' @export
get_output_dir <- function(.bbi_object, .check_exists = TRUE) {
  UseMethod("get_output_dir")
}

#' @export
get_output_dir.bbi_model <- function(.bbi_object, .check_exists = TRUE) {
  get_output_dir_nonmem(.bbi_object, .check_exists)
}

#' @export
get_output_dir.bbi_nonmem_summary <- function(.bbi_object, .check_exists = TRUE) {
  get_output_dir_nonmem(.bbi_object, .check_exists)
}

#' @export
get_output_dir.bbi_log_df <- function(.bbi_object, .check_exists = TRUE) {
  get_path_from_log_df(.bbi_object, get_output_dir, .check_exists = .check_exists)
}


#' @rdname get_path_from_object
#' @export
get_yaml_path <- function(.bbi_object, .check_exists = TRUE) {
  UseMethod("get_yaml_path")
}

#' @export
get_yaml_path.bbi_model <- function(.bbi_object, .check_exists = TRUE) {
  get_yaml_path_nonmem(.bbi_object, .check_exists = .check_exists)
}

#' @export
get_yaml_path.bbi_nonmem_summary <- function(.bbi_object, .check_exists = TRUE) {
  get_yaml_path_nonmem(.bbi_object, .check_exists = .check_exists)
}

#' @export
get_yaml_path.bbi_log_df <- function(.bbi_object, .check_exists = TRUE) {
  get_path_from_log_df(.bbi_object, get_yaml_path, .check_exists = .check_exists)
}


#' @rdname get_path_from_object
#' @export
get_config_path <- function(.bbi_object, .check_exists = TRUE) {
  UseMethod("get_config_path")
}

#' @export
get_config_path.bbi_model <- function(.bbi_object, .check_exists = TRUE) {
  get_config_path_nonmem(.bbi_object, .check_exists = .check_exists)
}

#' @export
get_config_path.bbi_nonmem_summary <- function(.bbi_object, .check_exists = TRUE) {
  get_config_path_nonmem(.bbi_object, .check_exists = .check_exists)
}

#' @export
get_config_path.bbi_log_df <- function(.bbi_object, .check_exists = TRUE) {
  get_path_from_log_df(.bbi_object, get_config_path, .check_exists = .check_exists)
}

#' @export
get_config_path.bbi_nmboot_model <- function(.bbi_object, .check_exists = TRUE) {
  get_spec_path(.bbi_object, .check_exists = .check_exists)
}


#' Get the relevant specification file path
#'
#' Get the bootstrap specification path from a `bbi_nmboot_model` object or the
#' simulation specification path from a `bbi_nmsim_model` _or_ `bbi_nonmem_model`
#' object.
#'
#' @param .mod a `bbi_{.model_type}_model` object.
#' @param .check_exists If `TRUE`, the default, will throw an error if the file
#'  does not exist.
#' @note this is not the configuration file generated by bbi
#' @returns a file path
#' @keywords internal
get_spec_path <- function(.mod, .check_exists = TRUE) {
  UseMethod("get_spec_path")
}


#' @describeIn get_spec_path Get the bootstrap specification file path from a
#' `bbi_nmboot_model` object
#' @keywords internal
get_spec_path.bbi_nmboot_model <- function(.mod, .check_exists = TRUE) {
  # Spec file saved to modeling directory of bootstrap run
  .path <- file.path(
    get_output_dir(.mod, .check_exists = .check_exists),
    "bbr_boot_spec.json")

  if (isTRUE(.check_exists)) {
    checkmate::assert_file_exists(.path)
  }

  return(.path)
}


#' @describeIn get_spec_path Get the simulation specification file path from a
#' `bbi_nmsim_model` or `bbi_nonmem_model` object
#' @keywords internal
get_spec_path.bbi_base_model <- function(.mod, .check_exists = TRUE) {
  # Spec file saved to output directory of based_on model
  if(inherits(.mod, NMSIM_MOD_CLASS)){
    .path <- file.path(get_model_working_directory(.mod), "bbr_sim_spec.json")
  }else if(inherits(.mod, NM_MOD_CLASS)){
    output_dir <- get_output_dir(.mod, .check_exists = .check_exists)
    .path <- file.path(output_dir, "bbr_sim_spec.json")
  }else{
    rlang::abort(
      glue("Unsupported model type. Expected {NMSIM_MOD_CLASS} or {NM_MOD_CLASS}")
    )
  }

  if (isTRUE(.check_exists)) {
    checkmate::assert_file_exists(.path)
  }

  return(.path)
}


# Register private S3 methods for development purposes
.S3method("get_spec_path", "bbi_nmboot_model", get_spec_path.bbi_nmboot_model)
.S3method("get_spec_path", "bbi_base_model", get_spec_path.bbi_base_model)

#' Get model identifier
#'
#' Helper to strip path and extension from model file to get only model identifier
#' (i.e. the model file name without extension)
#' @param .mod Model to use, either a `bbi_{.model_type}_model` or
#' `bbi_{.model_type}_summary`, or a file path to a model.
#' @importFrom tools file_path_sans_ext
#' @return Character scalar with only model identifier
#' @export
get_model_id <- function(.mod) {
  UseMethod("get_model_id")
}

#' @describeIn get_model_id Takes file path to model.
#' @export
get_model_id.character <- function(.mod) {
  return(basename(tools::file_path_sans_ext(.mod)))
}

#' @describeIn get_model_id Takes `bbi_{.model_type}_model` or `bbi_{.model_type}_summary` object
#' @export
get_model_id.bbi_model <- function(.mod) {
  return(get_model_id(.mod[[ABS_MOD_PATH]]))
}


#' Get path to data file
#'
#' Helper to extract the path to the input data from a bbi object.
#'
#' @param .bbi_object The object to query. Could be
#' a `bbi_{.model_type}_model` object,
#' `bbi_{.model_type}_summary` object,
#' or a tibble of class `bbi_log_df`.
#' @param .check_exists If `TRUE`, the default, will throw an error if the file
#'  does not exist
#' @param ... Arguments passed through to methods. (Currently none, but may have
#'   some modifier arguments other model types in the future.)
#'
#' @template nonmem-mod-ext
#' @seealso [get_model_id()] [get_path_from_object()] [build_path_from_model()]
#'
#' @return Absolute path to input data file
#' @export
get_data_path <- function(
    .bbi_object,
    .check_exists = TRUE,
    ...
){
  UseMethod("get_data_path")
}


#' @export
get_data_path.bbi_model <- function(
    .bbi_object,
    .check_exists = TRUE,
    ...
){
  data_path <- get_data_path_from_json(.bbi_object)
  if(isTRUE(.check_exists)){
    if(!fs::file_exists(data_path)){
      rlang::abort(
        c(
          "x" = "Input data file does not exist or cannot be opened",
          "i" = glue("Referenced input data path: {data_path}")
        )
      )
    }
  }

  return(data_path)
}

#' @export
get_data_path.bbi_nonmem_model <- function(
    .bbi_object,
    .check_exists = TRUE,
    ...
){
  get_data_path_nonmem(.bbi_object, .check_exists, ...)
}

#' @export
get_data_path.bbi_nmboot_model <- function(
    .bbi_object,
    .check_exists = TRUE,
    ...
){
  # nmboot models do not have a config file, so we can only extract from the
  # $DATA record in a control stream file
  data_path <- get_data_path_from_ctl(.bbi_object)

  if(isTRUE(.check_exists)){
    if(!fs::file_exists(data_path)){
      rlang::abort(
        c(
          "x" = "Input data file does not exist or cannot be opened",
          "i" = glue("Referenced input data path: {data_path}")
        )
      )
    }
  }

  return(data_path)
}

#' @export
get_data_path.bbi_nmsim_model <- function(
    .bbi_object,
    .check_exists = TRUE,
    ...
){
  get_data_path_nonmem(.bbi_object, .check_exists, ...)
}


#' @export
get_data_path.bbi_nonmem_summary <- function(
    .bbi_object,
    .check_exists = TRUE,
    ...
){
  .mod <- read_model(.bbi_object$absolute_model_path)
  get_data_path(.mod, .check_exists, ...)
}


#' @export
get_data_path.bbi_log_df <- function(
    .bbi_object,
    .check_exists = TRUE,
    ...
){
  data_paths <- map_chr(.bbi_object[[ABS_MOD_PATH]], function(.path) {
    .mod <- read_model(.path)
    get_data_path(.mod, .check_exists, ...)
  })

  return(data_paths)
}


get_data_path_nonmem <- function(
    .bbi_object,
    .check_exists = TRUE,
    ...
){
  # Get data path as defined in control stream
  data_path_ctl <- get_data_path_from_ctl(.bbi_object)
  # Get data path as defined in bbi_config.json
  data_path_config <- tryCatch(
    get_data_path_from_json(.bbi_object),
    error = function(.e) {
      if(stringr::str_detect(.e$message, "Cannot extract data path")){
        return(NULL)
      }else{stop(.e)}
    }
  )

  if(is.null(data_path_config)) {
    data_path <- data_path_ctl
  }else{
    # if they're not the same, warn
    if(data_path_config != data_path_ctl){
      mod_path <- get_model_path(.bbi_object)
      rlang::warn(
        c(
          paste("Data path referenced in `bbi_config.json` does not match the",
                "one defined in the control stream:"),
          "i" = glue("Data path extracted from {basename(mod_path)}: {data_path_ctl}"),
          "i" = glue("Data path extracted from bbi_config.json: {data_path_config}"),
          "Preferring config (`bbi_config.json`) path..."
        )
      )
    }
    data_path <- data_path_config
  }


  if(isTRUE(.check_exists)){
    if(!fs::file_exists(data_path)){
      # The first error message line is what NM-TRAN would return in this situation
      rlang::abort(
        c(
          "x" = "Input data file does not exist or cannot be opened",
          "i" = glue("Referenced input data path: {data_path}")
        )
      )
    }
  }

  return(data_path)
}



#' Get the data path from a control stream file
#' @noRd
get_data_path_from_ctl <- function(.mod, normalize = TRUE){
  check_model_object(.mod, c(NM_MOD_CLASS, NMBOOT_MOD_CLASS, NMSIM_MOD_CLASS))
  mod_path <- get_model_path(.mod)
  ctl <- nmrec::read_ctl(mod_path)

  # Get data record
  data_recs <- nmrec::select_records(ctl, "data")
  n_data <- length(data_recs)
  if(n_data !=1){
    recs_fmt <- purrr::map_chr(data_recs, function(rec) rec$format())
    rlang::abort(
      c(
        glue::glue("Expected a single data record, but found {n_data}:\n\n"),
        recs_fmt
      )
    )
  }

  # Get file path and remove quotes
  data_path_ctl <- nmrec::get_record_option(data_recs[[1]], "filename")$value
  data_path_ctl <- unquote_filename(data_path_ctl)

  if(isTRUE(normalize)){
    # Adjust for model extension
    data_path_ctl_adj <- adjust_data_path_ext(data_path_ctl, mod_path)
    # Normalize to current model directory if not an absolute path
    model_dir <- dirname(get_model_path(.mod, .check_exists = FALSE))
    if(!fs::is_absolute_path(data_path_ctl_adj)){
      data_path <- fs::path_norm(file.path(model_dir, data_path_ctl_adj)) %>%
        as.character()
    }else{
      data_path <- data_path_ctl_adj
    }
  }else{
    data_path <- data_path_ctl
  }

  return(data_path)
}


#' Get data path as defined in bbi_config.json
#' @noRd
get_data_path_from_json <- function(.mod, normalize = TRUE){
  cfg_path <- get_config_path(.mod, .check_exists = FALSE)

  if (!fs::file_exists(cfg_path)) {
    rlang::abort(
      c(
        glue("Cannot extract data path from config file because none exist at `{cfg_path}`"),
        "i" = "This likely means the model has not successfully been run yet."
      )
    )
  }

  cfg <- jsonlite::fromJSON(cfg_path)
  data_path_config <- cfg[[CONFIG_DATA_PATH]]

  if(isTRUE(normalize)){
    # Normalize to output directory
    output_dir <- get_output_dir(.mod, .check_exists = FALSE)
    data_path <- fs::path_norm(file.path(output_dir, data_path_config)) %>%
      as.character()
  }else{
    data_path <- data_path_config
  }

  return(data_path)
}

#' Adjust the data path based on the model extension
#'
#' @param data_path a file path to an input data set
#' @param mod_path a relative or absolute model path. Must include the extension.
#' @param reverse logical (T/F). Should be `FALSE` if the `data_path` was **extracted**
#'  from a control stream file, and `TRUE` if using to **set** the data path in a
#'  control stream file.
#'
#' @noRd
adjust_data_path_ext <- function(data_path, mod_path, reverse = FALSE){
  checkmate::assert_true(is_valid_nonmem_extension(mod_path))

  if(isFALSE(reverse)){
    # potentially remove parent directory if extracting from control stream file
    if(identical(fs::path_ext(mod_path), "ctl")){
      path_elements <- fs::path_split(data_path)[[1]]
      if (path_elements[1] == "..") {
        data_path_adj <- fs::path_join(path_elements[-1])
      }else{
        data_path_adj <- data_path
      }
    }else{
      data_path_adj <- data_path
    }
  }else{
    # potentially add parent directory if using to create a data path for use in
    # a control stream file
    if(identical(fs::path_ext(mod_path), "ctl")){
      data_path_adj <- file.path("..", data_path)
    }else{
      data_path_adj <- data_path
    }
  }

  return(as.character(data_path_adj))
}


#' Build path to output file
#'
#' Builds the absolute path to a file in the output directory from components of the `bbi_{.model_type}_model` object
#'
#' @return Returns an absolute path to `{.mod$absolute_model_path}/{get_model_id(.mod)}{.suffix}`.
#'   Does _not_ check whether the file exists.
#'
#' @param .mod Model to use, either a `bbi_{.model_type}_model` or `bbi_{.model_type}_summary` object.
#' @param .suffix Character vector to append the end of the absolute model path.
#'   Will be appended _as is_ so, if passing a file extension, be sure to included the leading `"."`.
#'   See examples.
#' @param ... arguments passed through to methods. (Currently none.)
#'
#' @examples
#' .mod <- read_model(
#'   system.file("model", "nonmem", "basic", "1", package = "bbr")
#' )
#' build_path_from_model(.mod, ".lst")
#' build_path_from_model(.mod, "-standata.R")
#'
#' @export
build_path_from_model <- function(.mod, .suffix, ...) {
  UseMethod("build_path_from_model")
}

#' @rdname build_path_from_model
#' @export
build_path_from_model.bbi_model <- function(.mod, .suffix, ...) {
  file.path(
    # Note: Avoid using get_output_dir() here so that get_output_dir() methods
    # can use build_path_from_model() without triggering infinite recursion
    # (see, for example, get_output_dir.bbi_stan_model() in bbr.bayes).
    .mod[[ABS_MOD_PATH]],
    paste0(get_model_id(.mod), .suffix)
  )
}


###################################################################
# Model-specific get path from bbi object implementation functions
###################################################################

#' @keywords internal
get_model_path_nonmem <- function(.bbi_object, .check_exists = TRUE) {
  find_nonmem_model_file_path(.bbi_object[[ABS_MOD_PATH]], .check_exists)
}


#' @keywords internal
get_output_dir_nonmem <- function(.bbi_object, .check_exists = TRUE) {
  .path <- .bbi_object[[ABS_MOD_PATH]]

  if (isTRUE(.check_exists)) {
    checkmate::assert_directory_exists(.path)
  }

  return(.path)
}

#' @keywords internal
get_yaml_path_nonmem <- function(.bbi_object, .check_exists = TRUE) {
  .path <- paste0(.bbi_object[[ABS_MOD_PATH]], ".yaml")

  if (isTRUE(.check_exists)) {
    checkmate::assert_file_exists(.path)
  }

  return(.path)
}

#' @keywords internal
get_config_path_nonmem <- function(.bbi_object, .check_exists = TRUE) {
  .path <- file.path(
    get_output_dir(.bbi_object, .check_exists = .check_exists),
    "bbi_config.json")

  if (isTRUE(.check_exists)) {
    checkmate::assert_file_exists(.path)
  }

  return(.path)
}

###########################
# Get path private helpers
###########################

#' Private helper function to extract paths from bbi_log_df
#' @param .log_df The `bbi_log_df` object
#' @param .get_func The getter function to call
#' @inheritParams get_path_from_object
#' @importFrom purrr map_chr
#' @keywords internal
get_path_from_log_df <- function(.log_df, .get_func, .check_exists) {
  .out_paths <- map_chr(.log_df[[ABS_MOD_PATH]], function(.path) {
    .mod <- read_model(.path)
    .get_func(.mod, .check_exists = .check_exists)
  })
  return(.out_paths)
}

# SHARED: get_model_working_directory() is used by bbr.bayes, so any changes
# here should be compatible with its use there.

#' Get the working directory of a model object
#'
#' The working directory of a model object is the parent directory of the
#' absolute model path.
#'
#' @param .mod A bbi model or summary object.
#'
#' @return The path to the working directory.
#' @keywords internal
get_model_working_directory <- function(.mod) {
  dirname(.mod[[ABS_MOD_PATH]])
}


###############################
# Manipulating file extensions
###############################

#' Private helper to check if file has a valid NONMEM control stream extension
#' @param .path File path to check
#' @keywords internal
is_valid_nonmem_extension <- function(.path) {
  tools::file_ext(.path) %in% c("ctl", "mod")
}

#' Private helper to check if file has a valid YAML extension
#' @param .path File path to check
#' @keywords internal
is_valid_yaml_extension <- function(.path) {
  tools::file_ext(.path) %in% c("yaml", "yml")
}

#' Change file extension
#'
#' Helpers that strip file extension then add a different one
#' @param .x file path to modify
#' @param .ext new file extension to apply
#' @export
new_ext <- function(.x, .ext) {
  sprintf("%s.%s", tools::file_path_sans_ext(.x), .ext)
}

#' @rdname new_ext
#' @export
ctl_ext <- function(.x) {
  sprintf("%s.ctl", tools::file_path_sans_ext(.x))
}

#' @rdname new_ext
#' @export
mod_ext <- function(.x) {
  sprintf("%s.mod", tools::file_path_sans_ext(.x))
}

#' @rdname new_ext
#' @export
yaml_ext <- function(.x) {
  sprintf("%s.yaml", tools::file_path_sans_ext(.x))
}

###################################
# Other Assorted file path helpers
###################################


#' Find the path to a NONMEM model file
#'
#' NONMEM recognizes two model file extensions, `.ctl` and (less common) `.mod`.
#' Look for these at `.path` and if exactly one exists, return it. If both
#' exist, throw an error, because there is no way to know which should be used.
#' If neither exists and `.check_exists = TRUE`, throw an error.
#'
#' @importFrom fs path_norm
#'
#' @param .path File path to a NONMEM model file (control stream) with either
#'   `.ctl` or `.mod` extension.
#' @inheritParams get_model_path
#'
#' @return The path to the model file, if it exists. If the model file does not
#'   exist and `.check_exists = FALSE`, a string that is `.path` with the `.ctl`
#'   extension.
#' @keywords internal
find_nonmem_model_file_path <- function(.path, .check_exists = TRUE) {
  checkmate::assert_string(.path)
  checkmate::assert_logical(.check_exists, any.missing = FALSE, len = 1L)

  maybe_paths <- paste0(.path, c(".ctl", ".mod"))
  exists_idx <- purrr::map_lgl(maybe_paths, fs::file_exists)

  if (all(exists_idx)) {
    stop(
      glue::glue(
        "Both `.ctl` and `.mod` files found at {.path}. Remove one to continue."
      )
    )
  }

  if (!any(exists_idx)) {
    if (isTRUE(.check_exists)) {
      stop(paste(
        glue(
          "No model file found at {maybe_paths[1L]} or {maybe_paths[2L]}.",
          "Please put relevant model file in that location.",
          .sep = " "
        ),
        NONMEM_MODEL_TYPE_ERR_MSG,
        sep = "\n"
      ))
    } else {
      res <- maybe_paths[1L]
    }
  } else {
    res <- maybe_paths[which(exists_idx)]
  }

  res %>%
    fs::path_norm() %>%
    as.character()
}


#' Read in and format the bootstrap specification file.
#'
#' Tabulates all relevant `bbi_nonmem_model` model files from a bootstrap
#' specification file.
#' @param .boot_run Either a `bbi_nmboot_model` or `bbi_nmboot_summary` object
#' @returns a list
#' @keywords internal
get_boot_spec <- function(.boot_run){
  check_model_object(.boot_run, c(NMBOOT_MOD_CLASS, NMBOOT_SUM_CLASS))
  if(inherits(.boot_run, NMBOOT_SUM_CLASS)){
    .boot_run <- read_model(.boot_run[[ABS_MOD_PATH]])
  }

  spec_path <- get_spec_path(.boot_run, .check_exists = FALSE)
  if(!fs::file_exists(spec_path)) return(NULL)

  boot_spec <- jsonlite::read_json(spec_path, simplifyVector = TRUE)
  boot_dir <- .boot_run[[ABS_MOD_PATH]]

  # Format individual bootstrap model runs if not cleaned up
  if(!is.null(boot_spec$bootstrap_runs)){
    boot_runs <- boot_spec$bootstrap_runs

    boot_mod_files <- data.frame(
      matrix(unlist(boot_runs), nrow=length(boot_runs), byrow = TRUE),
      stringsAsFactors = FALSE
    ) %>% stats::setNames(names(boot_runs[[1]])) %>%
      tibble::as_tibble()

    spec_df <- boot_mod_files %>% dplyr::mutate(
      run = names(boot_runs),
      mod_path_abs = file.path(boot_dir, fs::path_ext_remove(.data$mod_path))
    ) %>% dplyr::relocate("run")

    spec <- c(boot_spec$bootstrap_spec, list(bootstrap_runs = spec_df))
  }else{
    spec <- boot_spec$bootstrap_spec
  }

  return(spec)
}


#' Read in and format the simulation specification file.
#'
#' @param .mod A `bbi_nonmem_model`, `bbi_nmsim_model`, or `bbi_nonmem_summary`
#'  object
#' @returns a list
#' @keywords internal
get_sim_spec <- function(.mod){
  check_model_object(.mod, c(NM_MOD_CLASS, NMSIM_MOD_CLASS, NM_SUM_CLASS))
  if(inherits(.mod, NM_SUM_CLASS)){
    .mod <- read_model(.mod[[ABS_MOD_PATH]])
  }

  spec_path <- get_spec_path(.mod, .check_exists = FALSE)
  if(!fs::file_exists(spec_path)) return(NULL)
  sim_spec <- jsonlite::read_json(spec_path, simplifyVector = TRUE)

  return(sim_spec$sim_spec)
}


#' Get path to `MSF` file path
#'
#' Extract `MSF` file path as defined in a `$ESTIMATION` record in a `NONMEM`
#'  control stream file, and construct absolute file path
#'
#' @param .mod a `bbi_model` object
#' @param .check_exists If `TRUE`, the default, will throw an error if the file does not exist
#' @returns absolute file path to `MSF` file, or `NULL` if one doesnt exist.
#' @keywords internal
get_msf_path <- function(.mod, .check_exists = TRUE){
  ctl <- get_model_ctl(.mod)
  ests <- nmrec::select_records(ctl, "est")

  msf_path <- purrr::map(ests, function(est){
    opt <- nmrec::get_record_option(est, "msf")
    if(!is.null(opt)) opt$value else opt
  }) %>% purrr::list_c()

  if(!is.null(msf_path)){
    if(length(msf_path) != 1){
      msf_txt <- paste0(msf_path, collapse = ", ")
      rlang::abort(
        c(glue("Multiple MSF files found: {msf_txt}"))
      )
    }
    msf_path <- file.path(.mod[[ABS_MOD_PATH]], msf_path)
  }

  if(isTRUE(.check_exists)){
    checkmate::assert_file_exists(msf_path)
  }

  return(msf_path)
}
