#' Create new model spec by specifying relevant information as arguments
#' Also creates necessary YAML file for using `create_run_log()` later.
#' @param .yaml_path Path to save resulting model YAML file to
#' @param .description Description of new model run. This will be stored in the yaml (to be used later in `create_run_log()`) and optionally passed into the `$PROBLEM` of the new control stream.
#' @param .model_path Path to control stream file for the model. MUST be an absolute path, or the model path relative to the location of the YAML file. It recommended for the control stream and YAML to be in the same directory.
#' @param .based_on A character scaler or vector of run id's (model names) that this model was "based on." These are used to reconstuct model developement and ancestry.
#' @param .tags A character scaler or vector with any user tags to be added to the YAML file
#' @param .bbi_args A named list specifying arguments to pass to babylon formatted like `list("nm_version" = "nm74gf_nmfe", "json" = T, "threads" = 4)`. Run `print_nonmem_args()` to see valid arguments. These will be written into YAML file.
#' @param .model_type Character scaler to specify type of model being created (used for S3 class). Currently only `'nonmem'` is supported.
#' @importFrom yaml write_yaml
#' @return S3 object of class `bbi_{.model_type}_spec` that can be passed to `submit_model()`
#' @export
create_model <- function(
  .yaml_path,
  .description,
  .model_path = NULL,
  .based_on = NULL,
  .tags = NULL,
  .bbi_args = NULL,
  .model_type = c("nonmem")) {

  # fill list from passed args
  .spec <- list()
  .spec[[WORKING_DIR]] <- normalizePath(dirname(.yaml_path))
  .spec[[YAML_YAML_NAME]] <- basename(.yaml_path)
  .spec[[YAML_DESCRIPTION]] <- .description
  .spec[[YAML_MOD_TYPE]] <- .model_type
  if (!is.null(.model_path)) .spec[[YAML_MOD_PATH]] <- .model_path
  if (!is.null(.based_on)) .spec[[YAML_BASED_ON]] <- scaler_to_list(.based_on)
  if (!is.null(.tags)) .spec[[YAML_TAGS]] <- scaler_to_list(.tags)
  if (!is.null(.bbi_args)) .spec[[YAML_BBI_ARGS]] <- .bbi_args

  # make list into S3 object
  .spec <- create_spec(.spec)

  # write YAML to disk
  save_mod_yaml(.spec, .out_path = .yaml_path)

  return(.spec)
}


#' Create new model spec from a model yaml
#' @param .yaml_path Character scaler for the path to the YAML model file that will be used
#' @importFrom yaml read_yaml
#' @importFrom purrr list_modify
#' @return S3 object of class `bbi_{.model_type}_spec` that can be passed to `submit_model()`
#' @export
create_model_from_yaml <- function(.yaml_path) {
  # read in yaml and create spec object
  .spec <- parse_mod_yaml(.yaml_path)

  return(.spec)
}


#' Parses a model yaml file into a list object that contains correctly formatted information from the yaml
#' @param .path Path to the yaml file to parse.
#' @importFrom yaml read_yaml
#' @importFrom fs file_exists
#' @return Output list as specified above.
#' @export
parse_mod_yaml <- function(.path) {
  if (!fs::file_exists(.path)) {
    stop(glue("Cannot find yaml file at path {.path}"))
  }

  # load from file
  yaml_list <- read_yaml(.path)
  yaml_list[[WORKING_DIR]] <- normalizePath(dirname(.path))
  yaml_list[[YAML_YAML_NAME]] <- basename(.path)

  # parse model path
  if (!check_required_keys(yaml_list, .req = YAML_REQ_INPUT_KEYS)) {
    paste0(
      "Model yaml must have keys `", paste(YAML_REQ_INPUT_KEYS, collapse=", "), "` specified in it. ",
      "But `", paste(YAML_REQ_INPUT_KEYS[!(YAML_REQ_INPUT_KEYS %in% names(yaml_list))], collapse=", "), "` are missing. ",
      .path, " has the following keys: ", paste(names(yaml_list), collapse=", ")
    )
    strict_mode_error(err_msg)
  }

  .spec <- create_spec(yaml_list)

  return(.spec)
}


#' Saves a model spec object to a yaml file
#' @param .spec S3 object of class `bbi_{.model_type}_spec`
#' @param .out_path Character scaler with path to save out YAML file. By default, sets it to the model file name, with a yaml extension.
#' @importFrom yaml write_yaml
#' @importFrom fs file_exists
#' @return Output list as specified above.
#' @export
save_mod_yaml <- function(.spec, .out_path = NULL) {
  # fill path if null
  if (is.null(.out_path)) {
    .out_path <- file.path(.spec[[WORKING_DIR]], yaml_ext(.spec[[YAML_MOD_PATH]]))
  }

  # erase keys that don't need to be saved out
  for (key in YAML_ERASE_OUT_KEYS) {
    .spec[[key]] <- NULL
  }

  # write to disk
  yaml::write_yaml(.spec, .out_path)
}


#' Creates S3 object of class `bbi_{.model_type}_spec` from list with `SPEC_REQ_INPUT_KEYS`
#' @param .mod_list List with the required information to create a spec object
#' @return S3 object of class `bbi_{.model_type}_spec` that can be passed to `submit_model`
create_spec <- function(.mod_list) {
  # check that necessary keys are present
  if (!check_required_keys(.mod_list, .req = SPEC_REQ_INPUT_KEYS)) {
    err_msg <- paste0(
      "Model list must have keys `", paste(SPEC_REQ_INPUT_KEYS, collapse=", "), "` specified in order to create spec. ",
      "But `", paste(SPEC_REQ_INPUT_KEYS[!(SPEC_REQ_INPUT_KEYS %in% names(.mod_list))], collapse=", "), "` are missing. ",
      "List has the following keys: ", paste(names(.mod_list), collapse=", ")
    )
    strict_mode_error(err_msg)
  }

  # by default, if no model defined, will set it to the yaml file name with extension ctl
  if (is.null(.mod_list[[YAML_MOD_PATH]])) {
    if (!is.null(.mod_list[[YAML_YAML_NAME]])) {
      .mod_list[[YAML_MOD_PATH]] <- ctl_ext(.mod_list[[YAML_YAML_NAME]])
    } else {
      stop("Must specify either a YAML_MOD_PATH or YAML_YAML_NAME to create a spec. User should never see this error.")
    }
  } else if (.mod_list[[YAML_MOD_TYPE]] == "nonmem" && (!is_valid_nonmem_extension(.mod_list[[YAML_MOD_PATH]]))) {
    stop(glue::glue("model_path defined in yaml at {.path} must have either a .ctl or .mod extension, not {.mod_list[[YAML_MOD_PATH]]}"))
  }

  # check babylon args and add an empty list if missing
  if (is.null(.mod_list[[YAML_BBI_ARGS]])) {
    .mod_list[[YAML_BBI_ARGS]] <- list()
  } else {
    # check that unique named list was passed
    tryCatch(
      checkmate::assert_list(.mod_list[[YAML_BBI_ARGS]], names="unique"),
      error = function(e) { stop(glue("`{YAML_BBI_ARGS}` must be a unique, named list: {e}")) }
    )
  }

  # add class and return
  .model_type <- .mod_list[[YAML_MOD_TYPE]]
  if (!(.model_type %in% SUPPORTED_MOD_TYPES)) {
    stop(glue("Invalid {YAML_MOD_TYPE} `{.model_type}`. Valid options include: `{SUPPORTED_MOD_TYPES}`"))
  }

  .mod_list <- assign_spec_class(.mod_list, .model_type)

  return(.mod_list)
}


# S3 dispatch for iterating on models

#' Generic S3 method from iterating on models.
#' @param .x Object to copy from
#' @param ... arguments to pass through
#' @export
#' @rdname copy_model_from
copy_model_from <- function(.x, ...) {
  UseMethod("copy_model_from", .x)
}

#' S3 dispatch for passing `bbi_nonmem_spec` object to `copy_model_from()`
#' @param ... arguments to pass through to `copy_nonmem_model_from()`
#' @export
#' @rdname copy_model_from
copy_model_from.bbi_nonmem_spec <- function(.parent_spec, .new_model, .description, ...) {
  .spec <- copy_nonmem_model_from(
    .parent_spec,
    .new_model,
    .description,
    ...)

  return(.spec)
}

#' S3 dispatch for passing `bbi_nonmem_spec` object to copy_model_from()
#' @param .parent_path Path to YAML file to copy model from
#' @param ... arguments to pass through to implementation method
#' @export
#' @rdname copy_model_from
copy_model_from.character <- function(.parent_path, .new_model, .description, ...) {
  # check for erroneous file extension
  if (!is_valid_yaml_extension(.parent_path)) {
    warning(paste(glue("If passing a file path to `copy_model_from()`, path must be to a valid model YAML file."),
                  glue("(Got path `{.parent_path}`.)"),
                  "Alternatively, pass a valid `bbi{.model_type}_spec` S3 object from the output of `create_model` or another `create_model_from` call."))
  }

  # create spec from YAML path
  .parent_spec <- create_model_from_yaml(.parent_path)
  .model_type <- .parent_spec[[YAML_MOD_TYPE]]

  # copy from spec
  if (.model_type == "nonmem") {
    # create new model
    .spec <- copy_nonmem_model_from(
      .parent_spec,
      .new_model,
      .description,
      ...)
  } else if (.model_type == "stan") {
    stop(NO_STAN_ERR_MSG)
  } else {
    stop(glue("Passed `{.model_type}`. Valid options: `{SUPPORTED_MOD_TYPES}`"))
  }
  return(.spec)
}


#' Create new .mod/ctl and new .yaml files based on a previous model. Used for iterating on model development.
#' Also fills in necessary YAML fields for using `create_run_log()` later.
#' @param .parent_spec S3 object of class `bbi_nonmem_spec` to be used as the basis for copy.
#' @param .new_model Path to write new model files to WITHOUT FILE EXTENSION. Function will create both `{.new_model}.yaml` and `{.new_model}.[mod|ctl]` based on this path.
#' @param .description Description of new model run. This will be stored in the yaml (to be used later in `create_run_log()`) and optionally passed into the `$PROBLEM` of the new control stream.
#' @param .based_on_additional The run id for the `.parent_model` will automatically be added to the `based_on` field but this argument can contain a character scaler or vector of additional run id's (model names) that this model was "based on." These are used to reconstuct model developement and ancestry.
#' @param .add_tags A character scaler or vector with any new tags to be added to `{.new_model}.yaml`
#' @param .inherit_tags Boolean for whether to inherit any tags from `.parent_model.yaml`
#' @param .update_mod_file Boolean for whether to update the `$PROBLEM` line in the new control stream. By default it is TRUE, but if FALSE is passed `{.new_model}.[mod|ctl]` will be an exact copy of it's parent control stream.
#' @importFrom fs file_copy
#' @importFrom readr read_file write_file
#' @importFrom stringr str_replace
#' @importFrom yaml write_yaml
#' @importFrom purrr list_modify
#' @return S3 object of class `bbi_nonmem_spec` that can be passed to `submit_nonmem_model()`
#' @rdname copy_model_from
#' @export
copy_nonmem_model_from <- function(
  .parent_spec,
  .new_model,
  .description,
  .based_on_additional = NULL,
  .add_tags = NULL,
  .inherit_tags = FALSE,
  .update_mod_file = TRUE
) {
  # Check spec for correct class and then copy it
  if (!("bbi_nonmem_spec" %in% class(.parent_spec))) {
    stop(paste(
      "copy_nonmem_model_from() requires a spec object of class `bbi_nonmem_spec`. Passed object has the following classes:",
      paste(class(.parent_spec), collapse = ", "),
      "Consider creating a spec with `create_model()` or `create_model_from_yaml()`",
      sep = "\n"))
  }
  .new_spec <- .parent_spec

  # reset model working directory
  .new_spec[[WORKING_DIR]] <- normalizePath(dirname(.new_model))

  # build new model path
  .file_ext <- tools::file_ext(.parent_spec[[YAML_MOD_PATH]])
  if (.file_ext == "mod") {
    new_mod_path <- mod_ext(.new_model)
  } else if (.file_ext == "ctl") {
    new_mod_path <- ctl_ext(.new_model)
  } else {
    stop(glue("copy_nonmem_model_from() requires a spec object with a `{YAML_MOD_PATH}` pointing to either a .ctl or .mod file. Got `{YAML_MOD_PATH} = {.parent_spec[[YAML_MOD_PATH]]}`"))
  }
  .new_spec[[YAML_MOD_PATH]] <- basename(new_mod_path) # path should be relative to YAML location

  # copy control steam to new path
  if (.update_mod_file) {
    # read parent control stream
    mod_str <- .parent_spec %>% get_model_path() %>% read_file()

    # replace the $PROBLEM line(s)
    mod_str <- str_replace(mod_str,
                "\\$PROB(.|\n)*?\\$",
                as.character(glue("$PROBLEM {get_mod_id(.new_model)} {.description}\n\n$")))

    # read parent control stream
    write_file(mod_str, get_model_path(.new_spec))
  } else {
    fs::file_copy(get_model_path(.parent_spec), get_model_path(.new_spec))
  }

  # fill based_on
  .new_spec[[YAML_BASED_ON]] <- get_mod_id(.parent_spec[[YAML_MOD_PATH]]) %>% c(.based_on_additional) %>% scaler_to_list()

  # fill description
  .new_spec[[YAML_DESCRIPTION]] <- .description

  # fill tags
  if (.inherit_tags && !is.null(.parent_spec[[YAML_TAGS]])) {
    .new_spec[[YAML_TAGS]] <- .parent_spec[[YAML_TAGS]] %>% c(.add_tags) %>% scaler_to_list()
  } else {
    .new_spec[[YAML_TAGS]] <- .add_tags %>% scaler_to_list()
  }

  # write .new_spec out
  new_yaml_path <- yaml_ext(.new_model)
  save_mod_yaml(.new_spec, .out_path = new_yaml_path)

  return(.new_spec)
}

#####################
# Modifying models
#####################


reconcile_mod_yaml <- function(.spec, .yaml_path) {
  # load spec from yaml on disk
  .loaded_spec <- parse_mod_yaml(.yaml_path)

  # overwrite values in memory from the ones on disk
  .new_spec <- combine_list_objects(.loaded_spec, .spec) # this would overwrite the in-memory with the yaml. Is that right?
  return(.new_spec)
}


modify_spec_field <- function(.spec, .field, .value, .append = TRUE) {
  # extract path to yaml
  .yaml_path <- get_yaml_path(.spec)

  # update .spec with any changes from yaml on disk
  .spec <- reconcile_mod_yaml(.spec, .yaml_path)

  # Either append new value or overwrite with new value
  if (.append) {
    .spec[[.field]] <- c(.spec[[.field]], .value)
  } else {
    .spec[[.field]] <- .value
  }

  # overwrite the yaml on disk with modified spec
  save_mod_yaml(.spec, .yaml_path)

  return(.spec)
}

add_tags <- function(.spec, .tags) {
  .spec <- modify_spec_field(.spec = .spec,
                             .field = YAML_TAGS,
                             .value = .tags,
                             .append = TRUE)
  return(.spec)
}

replace_tags <- function(.spec, .tags) {
  .spec <- modify_spec_field(.spec = .spec,
                             .field = YAML_TAGS,
                             .value = .tags,
                             .append = FALSE)
  return(.spec)
}




##################### LOGS, maybe should go in a different file

#' Parses model YAML files (and optionally model outputs) into a log tibble
#' @param .base_dir Directory to look in for YAML model files
#' @param .recurse Boolean for whether to search recursively in subdirectories
#' @importFrom stringr str_subset
#' @importFrom fs dir_ls
#' @importFrom purrr map map_lgl transpose
#' @importFrom dplyr as_tibble mutate_at mutate select everything
#' @importFrom yaml read_yaml
#' @return tibble with information on each run
#' @export
run_log <- function(
  .base_dir = ".",
  .recurse = TRUE
) {
  # get yaml files
  yaml_files <- .base_dir %>% dir_ls(recurse = .recurse) %>% str_subset("\\.ya?ml$") %>% str_subset("babylon\\.yaml$", negate = TRUE)

  # read in all candidate yaml's
  all_yaml <- map(yaml_files, function(.x) {read_yaml(.x)})

  # filter to only model yaml's
  mod_yaml_bool <- map_lgl(all_yaml, function(.x) {check_required_keys(.x, .req = YAML_REQ_INPUT_KEYS)})
  not_mod <- yaml_files[!mod_yaml_bool]
  if (length(not_mod) > 0) {
    warning(glue("Found {length(not_mod)} YAML files that do not contain required keys for a model YAML. Ignoring the following files: `{paste(not_mod, collapse='`, `')}`"))
  }
  mod_yaml <- all_yaml[which(mod_yaml_bool)]

  # transpose yaml list to tibble
  .col_names <- map(mod_yaml, function(.x) {return(names(.x))}) %>% unlist() %>% unique()
  df <- mod_yaml %>% transpose(.names = .col_names) %>% as_tibble() %>%
    mutate_at(c(YAML_MOD_PATH, YAML_DESCRIPTION, YAML_MOD_TYPE), unlist) %>%
    mutate(run_id = get_mod_id(.data[[YAML_MOD_PATH]])) %>%
    select(.data$run_id, everything())

  # add yaml path
  df$yaml_path <- yaml_files[mod_yaml_bool]

  class(df) <- c("bbi_nonmem_summary_df", class(df))
  return(df)
}


#' Parses model config.json outputs into a log tibble
#' @param .base_dir Base directory to look from model YAML files in
#' @param .recurse Boolean for whether to search recursively in subdirectories
#' @importFrom stringr str_subset
#' @importFrom fs dir_ls
#' @importFrom purrr map_df
#' @importFrom dplyr mutate select everything
#' @importFrom jsonlite fromJSON
#' @return tibble with information on each run
#' @export
config_log <- function(
  .base_dir = ".",
  .recurse = TRUE
) {
  # define json keys to keep as constant
  KEEPERS = c(
    "model_name",
    "model_md5",
    "data_path",
    "data_md5",
    "output_dir"
  )

  # get json files and parse to df
  json_files <- .base_dir %>% dir_ls(recurse = .recurse) %>% str_subset("bbi_config.json$")
  if (length(json_files) == 0) {
    warning(glue("Found no bbi_config.json files in {.base_dir}"))
    return(NULL)
  } else {
    df <- json_files %>%
      map_df(function(.path) fromJSON(.path)[KEEPERS]) %>%
      mutate(run_id = get_mod_id(.data$model_name)) %>%
      select(.data$run_id, everything(), -.data$model_name)

    return(df)
  }
}

#' Joins config log tibble onto a matching run log tibble
#' @param .log_df the output tibble from `run_log()`
#' @param ... arguments passed through to `config_log()`
#' @importFrom dplyr left_join
#' @export
add_config <- function(.log_df, ...) {
  # get config log
  .conf_df <- config_log(...)

  # check for missing rows
  if (is.null(.conf_df)) {
    warning(paste(glue("add_config() found {nrow(.log_df)} runs but found {nrow(.conf_df)} configs."),
                  "Check if your runs are still in progress.",
                  sep = "\n"))
    return(.log_df)
  } else if (nrow(.log_df) != nrow(.conf_df)) {
    warning(paste(glue("add_config() found {nrow(.log_df)} runs but found {nrow(.conf_df)} configs."),
                  "Check for rows with `is.null('model_md5')` in tibble. Some runs may have failed or still be in progress.",
                  sep = "\n"))
  }

  # join to log df
  df <- left_join(
    .log_df,
    .conf_df,
    by = "run_id",
    suffix = c(".log", ".config")
  )

  return(df)
}

