# Functions to manage model iteration and workflow

##############################
# Build or load model object
#############################

#' Create new model object by specifying relevant information as arguments
#' Also creates necessary YAML file for using functions like `add_tags()` and `run_log()` later.
#' @param .yaml_path Path to save resulting model YAML file to. MUST be either an absolute path, or a path relative to the `.directory` argument.
#' @param .description Description of new model run. This will be stored in the yaml (to be used later in `run_log()`) and optionally passed into the `$PROBLEM` of the new control stream.
#' @param .model_path Path to model (control stream) file. MUST be an absolute path, or the model path relative to the location of the YAML file. It recommended for the control stream and YAML to be in the same directory. If nothing is passed, the function will look for a file with the same path/name as your YAML, but with either .ctl or .mod extension.
#' @param .based_on A character scaler or vector of run id's (model names) that this model was "based on." These are used to reconstuct model developement and ancestry.
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
  .directory = getOption("rbabylon.model_directory")
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
  if (!is.null(.based_on)) .mod[[YAML_BASED_ON]] <- .based_on
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
  .directory = getOption("rbabylon.model_directory")
) {

  # check for .directory and combine with .path
  .path <- combine_directory_path(.directory, .path)

  # If not YAML extension, infer and look for file
  if (!is_valid_yaml_extension(.path)) {
    .path <- .path %>% get_yaml_path()
  }

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
  .mod <- .mod %>% purrr::compact()

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


#######################
# Iterating on models
#######################

#' Generic S3 method from iterating on models.
#' @param .parent_mod Model to copy from
#' @param .new_model Path to write new model files to WITHOUT FILE EXTENSION. Function will create both `{.new_model}.yaml` and a new model file based on this path.
#' @param .description Description of new model run. This will be stored in the yaml (to be used later in `create_run_log()`).
#' @param .based_on_additional The run id for the `.parent_model` will automatically be added to the `based_on` field but this argument can contain a character scaler or vector of additional run id's (model names) that this model was "based on." These are used to reconstuct model developement and ancestry.
#' @param .add_tags A character scaler or vector with any new tags to be added to `{.new_model}.yaml`
#' @param .inherit_tags Boolean for whether to inherit any tags from `.parent_model.yaml`
#' @param .update_mod_file Boolean for whether to update the newly created model file. By default it is TRUE, but if FALSE is passed new model file will be an exact copy of its parent.
#' @param .directory Model directory which `.new_model` is relative to. Defaults to `options('rbabylon.model_directory')`, which can be set globally with `set_model_directory()`.
#' @export
#' @rdname copy_model_from
copy_model_from <- function(
  .parent_mod,
  .new_model,
  .description,
  .based_on_additional = NULL,
  .add_tags = NULL,
  .inherit_tags = FALSE,
  .update_mod_file = TRUE,
  .directory = getOption("rbabylon.model_directory")
) {
  UseMethod("copy_model_from")
}

#' S3 dispatch for passing `bbi_nonmem_model` object to `copy_model_from()`
#' @param .parent_mod `bbi_nonmem_model` object to use as a basis for the copy.
#' @export
#' @rdname copy_model_from
copy_model_from.bbi_nonmem_model <- function(
  .parent_mod,
  .new_model,
  .description,
  .based_on_additional = NULL,
  .add_tags = NULL,
  .inherit_tags = FALSE,
  .update_mod_file = TRUE,
  .directory = getOption("rbabylon.model_directory")
) {

  # check for .directory and combine with .new_model
  .new_model <- combine_directory_path(.directory, .new_model)

  .mod <- copy_nonmem_model_from(
    .parent_mod = .parent_mod,
    .new_model = .new_model,
    .description = .description,
    .based_on_additional = .based_on_additional,
    .add_tags = .add_tags,
    .inherit_tags = .inherit_tags,
    .update_mod_file = .update_mod_file
  )

  return(.mod)
}

#' S3 dispatch for passing a file path to copy_model_from()
#' @param .parent_mod Path to parent model to use as a basis for the copy. Ideally a YAML path, but can also pass control stream or output directory.
#' @param .directory Model directory which BOTH `.parent_model` and `.new_model` are relative to. Defaults to `options('rbabylon.model_directory')`, which can be set globally with `set_model_directory()`.
#' @export
#' @rdname copy_model_from
copy_model_from.character <- function(
  .parent_mod,
  .new_model,
  .description,
  .based_on_additional = NULL,
  .add_tags = NULL,
  .inherit_tags = FALSE,
  .update_mod_file = TRUE,
  .directory = getOption("rbabylon.model_directory")
) {

  # check for .directory and combine with both .parent_mod and .new_model
  .parent_mod <- combine_directory_path(.directory, .parent_mod)
  .new_model <- combine_directory_path(.directory, .new_model)

  # If not YAML extension, infer and look for file
  if (!is_valid_yaml_extension(.parent_mod)) {
    .parent_mod <- .parent_mod %>% get_yaml_path()
  }

  # create model from YAML path
  .parent_mod <- read_model(.parent_mod)
  .model_type <- .parent_mod[[YAML_MOD_TYPE]]

  # copy from model
  if (.model_type == "nonmem") {
    # create new model
    .mod <- copy_nonmem_model_from(
      .parent_mod = .parent_mod,
      .new_model = .new_model,
      .description = .description,
      .based_on_additional = .based_on_additional,
      .add_tags = .add_tags,
      .inherit_tags = .inherit_tags,
      .update_mod_file = .update_mod_file
      )
  } else if (.model_type == "stan") {
    stop(NO_STAN_ERR_MSG)
  } else {
    stop(glue("Passed `{.model_type}`. Valid options: `{SUPPORTED_MOD_TYPES}`"))
  }
  return(.mod)
}

#' S3 dispatch for passing an integer model identifier to copy_model_from()
#' This will only work if you are calling from the same directory as the models, or if you have set the model directory with `set_model_directory()`
#' @param .parent_mod Integer that corresponds to parent model to use as a basis for the copy.
#' @param .new_model Integer that corresponds to the new model name to create. Function will create both `{.new_model}.yaml` and a new model file based on this path.
#' @param .directory Model directory which BOTH `.parent_model` and `.new_model` are relative to. Defaults to `options('rbabylon.model_directory')`, which can be set globally with `set_model_directory()`.
#' @export
#' @rdname copy_model_from
copy_model_from.numeric <- function(
  .parent_mod,
  .new_model,
  .description,
  .based_on_additional = NULL,
  .add_tags = NULL,
  .inherit_tags = FALSE,
  .update_mod_file = TRUE,
  .directory = getOption("rbabylon.model_directory")
) {

  # convert to character
  .parent_mod <- as.character(.parent_mod)
  .new_model <- as.character(.new_model)

  # call the character dispatch
  .mod <- copy_model_from(
    .parent_mod = .parent_mod,
    .new_model = .new_model,
    .description = .description,
    .based_on_additional = .based_on_additional,
    .add_tags = .add_tags,
    .inherit_tags = .inherit_tags,
    .update_mod_file = .update_mod_file,
    .directory = .directory
  )
  return(.mod)
}

#' Copy model from an existing model
#'
#' Create new .mod/ctl and new .yaml files based on a previous model. Used for iterating on model development.
#' Also fills in necessary YAML fields for using `create_run_log()` later.
#' @param .parent_mod S3 object of class `bbi_nonmem_model` to be used as the basis for copy.
#' @param .new_model Path to write new model files to WITHOUT FILE EXTENSION. Function will create both `{.new_model}.yaml` and `{.new_model}.[mod|ctl]` based on this path.
#' @param .description Description of new model run. This will be stored in the yaml (to be used later in `create_run_log()`) and optionally passed into the `$PROBLEM` of the new control stream.
#' @param .update_mod_file Boolean for whether to update the `$PROBLEM` line in the new control stream. By default it is TRUE, but if FALSE is passed `{.new_model}.[mod|ctl]` will be an exact copy of its parent control stream.
#' @importFrom fs file_copy
#' @importFrom readr read_file write_file
#' @importFrom stringr str_replace
#' @importFrom yaml write_yaml
#' @importFrom purrr list_modify
#' @return S3 object of class `bbi_nonmem_model` that can be passed to `submit_nonmem_model()`
#' @rdname copy_model_from
copy_nonmem_model_from <- function(
  .parent_mod,
  .new_model,
  .description,
  .based_on_additional = NULL,
  .add_tags = NULL,
  .inherit_tags = FALSE,
  .update_mod_file = TRUE
) {
  # Check model for correct class and then copy it
  if (!("bbi_nonmem_model" %in% class(.parent_mod))) {
    stop(paste(
      "copy_nonmem_model_from() requires a model object of class `bbi_nonmem_model`. Passed object has the following classes:",
      paste(class(.parent_mod), collapse = ", "),
      "Consider creating a model with `new_model()` or `read_model()`",
      sep = "\n"))
  }

  # build new model object
  .new_mod <- list()
  .new_mod[[YAML_DESCRIPTION]] <- .description

  # reset model working directory and yaml path
  .new_mod[[WORKING_DIR]] <- normalizePath(dirname(.new_model))
  .new_mod[[YAML_YAML_NAME]] <- basename(yaml_ext(.new_model))

  # fill output directory
  .new_mod[[YAML_OUT_DIR]] <- basename(tools::file_path_sans_ext(.new_model))

  # fill based_on
  .new_mod[[YAML_BASED_ON]] <- get_model_id(.parent_mod[[YAML_MOD_PATH]]) %>% c(.based_on_additional)

  # pass through model type and bbi_args
  .new_mod[[YAML_MOD_TYPE]] <- .parent_mod[[YAML_MOD_TYPE]]
  .new_mod[[YAML_BBI_ARGS]] <- .parent_mod[[YAML_BBI_ARGS]]

  # fill tags
  if (.inherit_tags && !is.null(.parent_mod[[YAML_TAGS]])) {
    .new_mod[[YAML_TAGS]] <- .parent_mod[[YAML_TAGS]] %>% c(.add_tags)
  } else {
    .new_mod[[YAML_TAGS]] <- .add_tags
  }

  # build new model path
  .file_ext <- tools::file_ext(.parent_mod[[YAML_MOD_PATH]])
  if (.file_ext == "mod") {
    new_mod_path <- mod_ext(.new_model)
  } else if (.file_ext == "ctl") {
    new_mod_path <- ctl_ext(.new_model)
  } else {
    stop(glue("copy_nonmem_model_from() requires a model object with a `{YAML_MOD_PATH}` pointing to either a .ctl or .mod file. Got `{YAML_MOD_PATH} = {.parent_mod[[YAML_MOD_PATH]]}`"))
  }
  .new_mod[[YAML_MOD_PATH]] <- basename(new_mod_path) # path should be relative to YAML location

  # copy control steam to new path
  if (.update_mod_file) {
    parent_mod_id <- .parent_mod %>% get_model_path() %>% get_model_id()
    new_mod_id <- .new_model %>% get_model_id()

    # read parent control stream
    mod_str <- .parent_mod %>% get_model_path() %>% read_file()

    # replace the $PROBLEM line(s)
    mod_str <- str_replace(mod_str,
                "\\$PROB(.|\n)*?\\$",
                as.character(glue("$PROBLEM {new_mod_id} {.description}\n\n$")))

    # read parent control stream
    write_file(mod_str, get_model_path(.new_mod))
  } else {
    fs::file_copy(get_model_path(.parent_mod), get_model_path(.new_mod))
  }

  # assign model class
  .new_mod <- create_model_object(.new_mod)

  # write .new_mod out
  new_yaml_path <- yaml_ext(.new_model)
  save_model_yaml(.new_mod, .out_path = new_yaml_path)

  return(.new_mod)
}


#####################
# Modifying models
#####################

#' Reconcile model object with YAML file
#'
#' Extracts YAML path from model object and pulls in YAML file.
#' Any shared keys are overwritten with the values from the YAML and new keys in YAML are added to the model object.
#' @param .mod `bbi_{.model_type}_model` object
#' @rdname reconcile_yaml
#' @export
reconcile_yaml <- function(.mod) {

  # extract path to yaml
  .yaml_path <- get_yaml_path(.mod)

  # load model from yaml on disk
  .loaded_mod <- read_model(.yaml_path)

  # overwrite values in memory from the ones on disk
  .new_mod <- combine_list_objects(.loaded_mod, .mod)

  return(.new_mod)
}


#' Checks that model YAML file is the same as when it was last read into the model object


#' Modify field in model object
#'
#' Implementation function for updating fields in a `bbi_{.model_type}_model` object
#' Also reconciles the object with the corresponding YAML before modifying and then writes the modified object back to the YAML
#' @param .mod The `bbi_{.model_type}_model` object to modify
#' @param .field Character scaler of the name of the component to modify
#' @param .value Whatever is to be added to `.mod[[.field]]`, typically a character scaler or vector
#' @param .append Boolean for whether to concatenate new values with currently present values. TRUE by default. If FALSE, new values will overwrite old values.
#' @param .unique Boolean for whether to de-duplicate `.mod[[.field]]` after adding new values. TRUE by default.
#' @rdname modify_model_field
modify_model_field <- function(.mod, .field, .value, .append = TRUE, .unique = TRUE) {

  # update .mod with any changes from yaml on disk
  .mod <- reconcile_yaml(.mod)

  # Either append new value or overwrite with new value
  if (isTRUE(.append)) {
    .mod[[.field]] <- c(.mod[[.field]], .value)
  } else {
    .mod[[.field]] <- .value
  }

  # de-duplicate values
  if (isTRUE(.unique)) {
    .mod[[.field]] <- .mod[[.field]] %>% unique()
  }

  # overwrite the yaml on disk with modified model
  save_model_yaml(.mod)

  return(.mod)
}

#' Add tags to a model object and corresponding YAML
#' @param .mod The `bbi_{.model_type}_model` object to modify
#' @param .tags Character scaler or vector of tags to add
#' @export
#' @rdname modify_model_field
add_tags <- function(.mod, .tags) {
  .mod <- modify_model_field(.mod = .mod,
                             .field = YAML_TAGS,
                             .value = .tags,
                             .append = TRUE)
  return(.mod)
}

#' Replaces tags on a model object and corresponding YAML with new tags
#' @param .mod The `bbi_{.model_type}_model` object to modify
#' @param .tags Character scaler or vector of tags use as replacement
#' @export
#' @rdname modify_model_field
replace_tags <- function(.mod, .tags) {
  .mod <- modify_model_field(.mod = .mod,
                             .field = YAML_TAGS,
                             .value = .tags,
                             .append = FALSE)
  return(.mod)
}

#' Append new decisions to the one(s) in a model object and corresponding YAML
#' @param .mod The `bbi_{.model_type}_model` object to modify
#' @param .decisions Character scaler or vector of text to add to `decisions` field
#' @export
#' @rdname modify_model_field
add_decisions <- function(.mod, .decisions) {
  .mod <- modify_model_field(.mod = .mod,
                             .field = YAML_DECISIONS,
                             .value = .decisions,
                             .append = TRUE)
  return(.mod)
}

#' Replaces `decisions` field in a model object and corresponding YAML with new values
#' @param .mod The `bbi_{.model_type}_model` object to modify
#' @param .decisions Character scaler or vector to use as replacement
#' @export
#' @rdname modify_model_field
replace_decisions <- function(.mod, .decisions) {
  .mod <- modify_model_field(.mod = .mod,
                             .field = YAML_DECISIONS,
                             .value = .decisions,
                             .append = FALSE)
  return(.mod)
}


#' Append new `based_on` tag to the one in a model object and corresponding YAML
#' @param .mod The `bbi_{.model_type}_model` object to modify
#' @param .based_on Character scaler or vector of model id's to add to `based_on` field
#' @export
#' @rdname modify_model_field
add_based_on <- function(.mod, .based_on) {
  .mod <- modify_model_field(.mod = .mod,
                             .field = YAML_BASED_ON,
                             .value = .based_on,
                             .append = TRUE)
  return(.mod)
}

#' Replaces `based_on` field in a model object and corresponding YAML with new values
#' @param .mod The `bbi_{.model_type}_model` object to modify
#' @param .based_on Character scaler or vector to use as replacement
#' @export
#' @rdname modify_model_field
replace_based_on <- function(.mod, .based_on) {
  .mod <- modify_model_field(.mod = .mod,
                             .field = YAML_BASED_ON,
                             .value = .based_on,
                             .append = FALSE)
  return(.mod)
}


#' Replaces description field in a model object and corresponding YAML with new description
#' @param .mod The `bbi_{.model_type}_model` object to modify
#' @param .description Character scaler to use as replacement
#' @export
#' @rdname modify_model_field
replace_description <- function(.mod, .description) {
  .mod <- modify_model_field(.mod = .mod,
                             .field = YAML_DESCRIPTION,
                             .value = .description,
                             .append = FALSE)

  return(.mod)
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
  .base_dir = getOption("rbabylon.model_directory"),
  .recurse = TRUE
) {

  # if no directory defined, set to working directory
  if (is.null(.base_dir)) {
    .base_dir <- getwd()
  }

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
    mutate(run_id = get_model_id(.data[[YAML_MOD_PATH]])) %>%
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
  .base_dir = getOption("rbabylon.model_directory"),
  .recurse = TRUE
) {

  # if no directory defined, set to working directory
  if (is.null(.base_dir)) {
    .base_dir <- getwd()
  }

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
      mutate(run_id = get_model_id(.data$model_name)) %>%
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

