#######################
# Iterating on models
#######################

#' Create new model by copying existing model
#'
#' Create new model by copying existing model. Useful for iterating during model development.
#' Also fills `based_on` field by default, for constructing model ancestry. See ["Using based_on field" vignette](../articles/using-based-on.html) for details.
#' @param .parent_mod Model to copy from
#' @param .new_model Path to write new model files to WITHOUT FILE EXTENSION. Function will create both `{.new_model}.yaml` and a new model file based on this path.
#' @param .description Description of new model run. This will be stored in the yaml (to be used later in `run_log()`).
#' @param .based_on_additional Character vector of path(s) to other models that this model was "based on." These are used to reconstuct model developement and ancestry. **Paths must be relative to `.new_model` path.** Note that the `.parent_model` will automatically be added to the `based_on` field, so no need to include that here.
#' @param .add_tags Character vector with any new tags(s) to be added to `{.new_model}.yaml`
#' @param .inherit_tags If `FALSE`, the default, new model will only have any tags passed to `.add_tags` argument. If `TRUE` inherit any tags from `.parent_mod`, with any tags passed to `.add_tags` appended.
#' @param .update_model_file If `TRUE`, the default, update the newly created model file with new description and name.
#' For a NONMEM model, this currently means only the `$PROBLEM` line in the new control stream will be updated.
#' If `FALSE`, new model file will be an exact copy of its parent.
#' @param .overwrite If `FALSE`, the default,  function will error if a model file already exists at specified `.new_model` path. If `TRUE` any existing file at `.new_model` will be overwritten silently.
#' @param .directory Model directory which `.new_model` is relative to. Defaults to `options('rbabylon.model_directory')`, which can be set globally with `set_model_directory()`.
#' @export
copy_model_from <- function(
  .parent_mod,
  .new_model,
  .description,
  .based_on_additional = NULL,
  .add_tags = NULL,
  .inherit_tags = FALSE,
  .update_model_file = TRUE,
  .overwrite = FALSE,
  .directory = get_model_directory()
) {
  UseMethod("copy_model_from")
}

#' @describeIn copy_model_from `.parent_mod` takes a `bbi_nonmem_model` object to use as a basis for the copy.
#' @export
copy_model_from.bbi_nonmem_model <- function(
  .parent_mod,
  .new_model,
  .description,
  .based_on_additional = NULL,
  .add_tags = NULL,
  .inherit_tags = FALSE,
  .update_model_file = TRUE,
  .overwrite = FALSE,
  .directory = get_model_directory()
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

#' @describeIn copy_model_from Takes a file path to parent model to use as a basis for the copy. Ideally a YAML path, but can also pass control stream or output directory.
#' Can be absolute or relative to `.directory`, which defaults to `options('rbabylon.model_directory')`.
#' @export
copy_model_from.character <- function(
  .parent_mod,
  .new_model,
  .description,
  .based_on_additional = NULL,
  .add_tags = NULL,
  .inherit_tags = FALSE,
  .update_model_file = TRUE,
  .overwrite = FALSE,
  .directory = get_model_directory()
) {

  # check for .directory and combine with both .parent_mod and .new_model
  .parent_mod <- combine_directory_path(.directory, .parent_mod)
  .new_model <- combine_directory_path(.directory, .new_model)

  # attempt to read in parent model
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

#' @describeIn copy_model_from Both `.parent_mod` and `.new_model` take integers which must correspond to a model file name (without extension obviously).
#' This will only work if you are calling from the same directory as the models, or if you have set `options('rbabylon.model_directory')` to the directory constaining the relevant models.
#' @export
copy_model_from.numeric <- function(
  .parent_mod,
  .new_model,
  .description,
  .based_on_additional = NULL,
  .add_tags = NULL,
  .inherit_tags = FALSE,
  .update_model_file = TRUE,
  .overwrite = FALSE,
  .directory = get_model_directory()
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

#' Copy model from an existing NONMEM model
#'
#' Private implementation function called by `copy_model_from()` dispatches.
#' Create new .mod/ctl and new .yaml files based on a previous model. Used for iterating on model development.
#' Also fills in necessary YAML fields for using `run_log()` later.
#' @param .parent_mod S3 object of class `bbi_nonmem_model` to be used as the basis for copy.
#' @param .description Description of new model run. This will be stored in the yaml (to be used later in `run_log()`) and optionally passed into the `$PROBLEM` of the new control stream.
#' @param .update_model_file If `TRUE`, the default, update the `$PROBLEM` line in the new control stream. If `FALSE`, `{.new_model}.[mod|ctl]` will be an exact copy of its parent control stream.
#' @importFrom fs file_copy path_rel
#' @importFrom readr read_file write_file
#' @importFrom stringr str_replace
#' @importFrom yaml write_yaml
#' @importFrom purrr list_modify
#' @importFrom digest digest
#' @return S3 object of class `bbi_nonmem_model` that can be passed to `submit_nonmem_model()`
#' @keywords internal
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
  if (!inherits(.parent_mod, NM_MOD_CLASS)) {
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
  .parent_based_on <- fs::path_rel(get_model_path(.parent_mod), start = .new_mod[[WORKING_DIR]])
  .new_mod[[YAML_BASED_ON]] <- safe_based_on(.new_mod[[WORKING_DIR]], c(.parent_based_on, .based_on_additional))

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
#' @param .update_model_file If `TRUE`, the default, update the `$PROBLEM` line in the new control stream. If `FALSE`, `{.new_model}.[mod|ctl]` will be an exact copy of its parent control stream.
#' @param .description Description of new model run. This will be passed into the `$PROBLEM` of the new control stream (if `.update_model_file=TRUE`).
#' @keywords internal
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
