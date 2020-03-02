# Functions to manage model iteration and workflow

#####################
# Spec ("specification") object
#####################

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
    .out_path <- get_yaml_path(.spec, .check_exists = FALSE)
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

#' Copy model from an existing model
#'
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
  .update_mod_file = TRUE,
  .open_mod_file = FALSE
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

  # reset model working directory and yaml path
  .new_spec[[WORKING_DIR]] <- normalizePath(dirname(.new_model))
  .new_spec[[YAML_YAML_NAME]] <- basename(yaml_ext(.new_model))

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
    parent_mod_id <- .parent_spec %>% get_model_path() %>% get_mod_id()
    new_mod_id <- .new_model %>% get_mod_id()

    # read parent control stream
    mod_str <- .parent_spec %>% get_model_path() %>% read_file()

    # replace the $PROBLEM line(s)
    mod_str <- str_replace(mod_str,
                "\\$PROB(.|\n)*?\\$",
                as.character(glue("$PROBLEM {new_mod_id} {.description}\n\n$")))

    # boilerplate code from Sam to replace other occurances of model id #### !!!!!!
    # need to test and refactor to use stringr
    mod_str <- mod_str %>%
      gsub(paste0('RUN# ', parent_mod_id), paste0('RUN# ', new_mod_id), .) %>%
      gsub(paste0(parent_mod_id, '.MSF'), paste0(new_mod_id, '.MSF'), .) %>%
      gsub(paste0(parent_mod_id, '.ext'), paste0(new_mod_id, '.ext'), .) %>%
      gsub(paste0(parent_mod_id, '.tab'), paste0(new_mod_id, '.tab'), .) %>%
      gsub(paste0(parent_mod_id, 'par.tab'), paste0(new_mod_id, 'par.tab'),.)
    ###### !!!!!!

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

  # optionally open the control stream for editing
  if (isTRUE(.open_mod_file)) {
    file.edit(new_yaml_path)
  }

  return(.new_spec)
}


#####################
# Result object
#####################

#' Create a `bbi_nonmem_result` object from a file path (should work with either path to a model file, yaml file, and output folder)
#' @param .path Character scaler with the file path to use
#' @export
import_result <- function(.path) {
  # check for the required files
  .output_dir <- tools::file_path_sans_ext(.path)
  .working_dir <- normalizePath(dirname(.output_dir))
  if (!fs::dir_exists(.output_dir)) {
    stop(glue("No directory exists at {.output_dir} -- Must pass path to a valid output directory."))
  }
  check_lst_file(.output_dir)

  .potential_yaml_file <- .output_dir %>% get_mod_id() %>% yaml_ext()
  .yaml_path <- file.path(.working_dir, .potential_yaml_file)
  if (fs::file_exists(.yaml_path)) {
    .spec <- parse_mod_yaml(.yaml_path)
  } else {
    .spec <- list()
    .spec[[WORKING_DIR]] <- .working_dir
    .spec[[YAML_MOD_TYPE]] <- "nonmem"
    .spec[[YAML_DESCRIPTION]] <- as.character(glue("Results object created from {.output_dir} with `import_result()`"))
    .spec[[YAML_MOD_PATH]] <- find_model_file_path(.path)
    .spec[[YAML_BBI_ARGS]] <- list()

  }

  # fill with results info
  .spec[[YAML_OUT_DIR]] <- basename(.output_dir)
  .spec[[RES_CMD_ARGS]] <- ""

  # assign class and return
  .res <- assign_result_class(.spec, .model_type = "nonmem")

  return(.res)
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


#' Modify field in spec object
#'
#' Implementation function for updating fields in a `bbi_{.model_type}_spec` object
#' Also reconciles the object with the corresponding YAML before modifying and then writes the modified object back to the YAML
#' @param .spec The `bbi_{.model_type}_spec` object to modify
#' @param .field Character scaler of the name of the component to modify
#' @param .value Whatever is to be added to `.spec[[.field]]`, typically a character scaler or vector
#' @param .append Boolean for whether to concatenate new values with currently present values. TRUE by default. If FALSE, new values will overwrite old values.
#' @param .unique Boolean for whether to de-duplicate `.spec[[.field]]` after adding new values. TRUE by default.
#' @rdname modify_spec_field
modify_spec_field <- function(.spec, .field, .value, .append = TRUE, .unique = TRUE) {
  # extract path to yaml
  .yaml_path <- get_yaml_path(.spec)

  # update .spec with any changes from yaml on disk
  .spec <- reconcile_mod_yaml(.spec, .yaml_path)

  # Either append new value or overwrite with new value
  if (isTRUE(.append)) {
    .spec[[.field]] <- c(.spec[[.field]], .value)
  } else {
    .spec[[.field]] <- .value
  }

  # de-duplicate values
  if (isTRUE(.unique)) {
    .spec[[.field]] <- .spec[[.field]] %>% unique(fromLast = TRUE)
  }

  # overwrite the yaml on disk with modified spec
  save_mod_yaml(.spec, .yaml_path)

  return(.spec)
}

#' Add tags to a spec object and corresponding YAML
#' @param .spec The `bbi_{.model_type}_spec` object to modify
#' @param .tags Character scaler or vector of tags to add
#' @export
#' @rdname modify_spec_field
add_tags <- function(.spec, .tags) {
  .spec <- modify_spec_field(.spec = .spec,
                             .field = YAML_TAGS,
                             .value = .tags,
                             .append = TRUE)
  return(.spec)
}

#' Replaces tags on a spec object and corresponding YAML with new tags
#' @param .spec The `bbi_{.model_type}_spec` object to modify
#' @param .tags Character scaler or vector of tags use as replacement
#' @export
#' @rdname modify_spec_field
replace_tags <- function(.spec, .tags) {
  .spec <- modify_spec_field(.spec = .spec,
                             .field = YAML_TAGS,
                             .value = .tags,
                             .append = FALSE)
  return(.spec)
}

#' Append new decisions to the one(s) in a spec object and corresponding YAML
#' @param .spec The `bbi_{.model_type}_spec` object to modify
#' @param .decisions Character scaler or vector of text to add to `decisions` field
#' @export
#' @rdname modify_spec_field
add_decisions <- function(.spec, .decisions) {
  .spec <- modify_spec_field(.spec = .spec,
                             .field = YAML_DECISIONS,
                             .value = .decisions,
                             .append = TRUE)
  return(.spec)
}

#' Replaces `decisions` field in a spec object and corresponding YAML with new values
#' @param .spec The `bbi_{.model_type}_spec` object to modify
#' @param .decisions Character scaler or vector to use as replacement
#' @export
#' @rdname modify_spec_field
replace_decisions <- function(.spec, .decisions) {
  .spec <- modify_spec_field(.spec = .spec,
                             .field = YAML_DECISIONS,
                             .value = .decisions,
                             .append = FALSE)
  return(.spec)
}


#' Append new `based_on` tag to the one in a spec object and corresponding YAML
#' @param .spec The `bbi_{.model_type}_spec` object to modify
#' @param .based_on Character scaler or vector of model id's to add to `based_on` field
#' @export
#' @rdname modify_spec_field
add_based_on <- function(.spec, .based_on) {
  .spec <- modify_spec_field(.spec = .spec,
                             .field = YAML_BASED_ON,
                             .value = .based_on,
                             .append = TRUE)
  return(.spec)
}

#' Replaces `based_on` field in a spec object and corresponding YAML with new values
#' @param .spec The `bbi_{.model_type}_spec` object to modify
#' @param .based_on Character scaler or vector to use as replacement
#' @export
#' @rdname modify_spec_field
replace_based_on <- function(.spec, .based_on) {
  .spec <- modify_spec_field(.spec = .spec,
                             .field = YAML_BASED_ON,
                             .value = .based_on,
                             .append = FALSE)
  return(.spec)
}


#' Replaces description field in a spec object and corresponding YAML with new description
#' @param .spec The `bbi_{.model_type}_spec` object to modify
#' @param .description Character scaler to use as replacement
#' @export
#' @rdname modify_spec_field
replace_description <- function(.spec, .description) {
  .spec <- modify_spec_field(.spec = .spec,
                             .field = YAML_DESCRIPTION,
                             .value = .description,
                             .append = FALSE)
  return(.spec)
}


#######################
# Generating run logs
#######################

#' Parses model yaml and outputs into a tibble that serves as a run log. Future releases will incorporate more diagnostics and parameter estimates, etc. from the runs into this log.
#' @param .base_dir Directory to search for model yaml files. Only runs with a corresponding yaml will be included.
#' @param .recurse Boolean for whether to search subdirectories recursively for additional yaml files. Defaults to TRUE.
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

  # stop if no valid model YAML found
  if (length(mod_yaml) == 0) {
    warning(glue("Found no valid model YAML files in {.base_dir}"))
    return(NULL)
  }

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

