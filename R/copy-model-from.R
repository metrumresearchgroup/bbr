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
#' @param .update_model_file Boolean for whether to update the newly created model file. By default it is TRUE, but if FALSE is passed new model file will be an exact copy of its parent.
#' @param .overwrite Boolean for whether to overwrite model file if one already exists specified `.new_model` path
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
  .update_model_file = TRUE,
  .overwrite = FALSE,
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
  .update_model_file = TRUE,
  .overwrite = FALSE,
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
    .update_model_file = .update_model_file,
    .overwrite = .overwrite
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
  .update_model_file = TRUE,
  .overwrite = FALSE,
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
      .update_model_file = .update_model_file,
      .overwrite = .overwrite
    )
  } else if (.model_type == "stan") {
    stop(NO_STAN_ERR_MSG)
  } else {
    stop(glue("Passed `{.model_type}`. Valid options: `{paste(SUPPORTED_MOD_TYPES, collapse = ', ')}`"))
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
  .update_model_file = TRUE,
  .overwrite = FALSE,
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
    .update_model_file = .update_model_file,
    .overwrite = .overwrite,
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
#' @param .update_model_file Boolean for whether to update the `$PROBLEM` line in the new control stream. By default it is TRUE, but if FALSE is passed `{.new_model}.[mod|ctl]` will be an exact copy of its parent control stream.
#' @param .overwrite Boolean for whether to overwrite .ctl or .mod file if one already exists at `{.new_model}.[mod|ctl]`
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
  .update_model_file = TRUE,
  .overwrite = FALSE
) {
  # Check model for correct class
  if (!("bbi_nonmem_model" %in% class(.parent_mod))) {
    stop(paste(
      "copy_nonmem_model_from() requires a model object of class `bbi_nonmem_model`. Passed object has the following classes:",
      paste(class(.parent_mod), collapse = ", "),
      "Consider creating a model with `new_model()` or `read_model()`",
      sep = "\n"))
  }

  # check parent against YAML
  check_yaml_in_sync(.parent_mod)

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
  .parent_model_path <- get_model_path(.parent_mod)
  .new_model_path <-  get_model_path(.new_mod, .check_exists = FALSE)


  if (fs::file_exists(.new_model_path) && !isTRUE(.overwrite)) {
    # if .overwrite != TRUE, warn that file already exists
    stop(glue("File already exists at {.new_model_path} -- cannot copy new control stream. Either delete old file or use `new_model({yaml_ext(.new_model_path)})`"))
  }

  # copy control stream file to new location, optionally updating it
  copy_control_stream(.parent_model_path, .new_model_path, .update_model_file, .description)

  # write .new_mod out to yaml
  new_yaml_path <- yaml_ext(.new_model)
  save_model_yaml(.new_mod, .out_path = new_yaml_path)

  # make list into S3 object
  .new_mod[[YAML_YAML_MD5]] <- digest(file = new_yaml_path, algo = "md5")
  .new_mod <- create_model_object(.new_mod)

  return(.new_mod)
}


#' Copy a NONMEM control stream file
#'
#' Helper function to copy a NONMEM control stream file and optionally update the description, etc. in the new file.
#' Note that any file existing at `.new_model_path` will be overwritten.
#' @param .parent_model_path Path to the control stream to copy
#' @param .new_model_path Path to copy the new control stream to
#' @param .update_model_file Boolean for whether to update the `$PROBLEM` line in the new control stream. By default it is TRUE, but if FALSE is passed `{.new_model}.[mod|ctl]` will be an exact copy of its parent control stream.
#' @param .description Description of new model run. This will be passed into the `$PROBLEM` of the new control stream (if `.update_model_file=TRUE`).
copy_control_stream <- function(.parent_model_path, .new_model_path, .update_model_file = FALSE, .description = NULL) {
  if (.update_model_file) {
    if (is.null(.description)) {
      stop("If `.update_model_file` is TRUE, user must specify a `.description` for the new model.")
    }

    # get model id's
    parent_mod_id <- .parent_model_path %>% get_model_id()
    new_mod_id <- .new_model_path %>% get_model_id()

    # read parent control stream
    mod_str <- .parent_model_path %>% read_file()

    # replace the $PROBLEM line(s)
    mod_str <- str_replace(mod_str,
                           "\\$PROB(.|\n)*?\\$",
                           as.character(glue("$PROBLEM {new_mod_id} {.description}\n\n$")))

    # read parent control stream
    write_file(mod_str, .new_model_path)
  } else {
    fs::file_copy(.parent_model_path, .new_model_path)
  }
}


#' Checks for yaml files relative to a starting location
#' @importFrom fs path_rel file_exists
#' @importFrom purrr map_lgl
#' @param .start The directory of the model (i.e. the YAML file) that the `based_on` will be added to.
#' @param .based_on Character vector or scaler of paths (with or without extension) to the models that will be added to `based_on`. Paths should be relative to `.start` argument.
check_based_on <- function(.start, .based_on) {
  # make all input paths relative to model and then absolute
  .paths <- fs::path_rel(.based_on, .start)
  .paths <- file.path(.start, .paths)

  # check for either a .yaml or .yml file at each location
  .paths_bool <- map_lgl(.paths, function(.p) {
    .y1 <- sprintf("%s.yaml", tools::file_path_sans_ext(.p))
    .y2 <- sprintf("%s.yml", tools::file_path_sans_ext(.p))
    .bool_test <- fs::file_exists(c(.y1, .y2))
    return(any(.bool_test))
  })
  names(.paths_bool) <- .paths

  if (!all(.paths_bool)) {
    strict_mode_error(paste(
      glue("Attempted to add {length(.paths_bool)} models as `based_on` but cannot find .yaml or .yml files for {length(.paths_bool) - sum(.paths_bool)} of them: "),
      paste(names(which(!.paths_bool)), collapse = ', ')
    ))
  }
}
