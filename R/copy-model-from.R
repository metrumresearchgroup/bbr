#######################
# Iterating on models
#######################

#' Create new model by copying existing model
#'
#' Create new model by copying existing model. Useful for iterating during model
#' development. Also fills `based_on` field by default, for constructing model
#' ancestry. See ["Using based_on field"
#' vignette](../articles/using-based-on.html) for details.
#' @param .parent_mod Model to copy from
#' @param .new_model Path to the new model, either absolute or relative to the
#'   path to `.parent_mod`. Represents an absolute model path, which is the path
#'   to the YAML file and model file, both without extension, and the output
#'   directory (once the model is run). Numeric values will be coerced to
#'   character. See examples for usage.
#' @param .description Character scalar description of new model run. This will
#'   be stored in the yaml (and can be viewed later in `run_log()`).
#' @param .based_on_additional Character vector of path(s) to other models that
#'   this model was "based on." These are used to reconstuct model developement
#'   and ancestry. **Paths must be relative to `.new_model` path.** Note that
#'   the `.parent_model` will automatically be added to the `based_on` field, so
#'   no need to include that here.
#' @param .add_tags Character vector with any new tags(s) to be added to
#'   `{.new_model}.yaml`
#' @param .inherit_tags If `TRUE`, the default, inherit any tags from
#'   `.parent_mod`, with any tags passed to `.add_tags` appended. If `FALSE` new
#'   model will only have any tags passed to `.add_tags` argument.
#' @param .update_model_file **Only relevant to NONMEM models.** If `TRUE`, the
#'   default, update the newly created model file. If `FALSE`, new model file
#'   will be an exact copy of its parent. For a NONMEM model, this currently
#'   means only the `$PROBLEM` line in the new control stream will be updated to
#'   read `See {.new_model}.yaml. Created by bbr.`.
#' @param .overwrite If `FALSE`, the default,  function will error if a model
#'   file already exists at specified `.new_model` path. If `TRUE` any existing
#'   file at `.new_model` will be overwritten silently.
#' @examples
#' \dontrun{
#' parent <- read_model("/foo/parent")
#'
#' # create model file at /bar/child.ctl and YAML at /bar/child.yaml
#' copy_model_from(parent, "/bar/child", "child model with absolute path")
#'
#' # create model file at /foo/child.ctl and YAML at /foo/child.yaml
#' copy_model_from(parent, "child", "relative to parent model path")
#'
#' mod1 <- read_model("/path/to/1")
#'
#' # create model file at /path/to/2.ctl and YAML at /path/to/2.yaml
#' copy_model_from(mod1, 2, "numeric input works")
#'
#' # create model file at /path/to/100.1.ctl and YAML at /path/to/100.1.yaml
#' copy_model_from(mod1, 100.1, "a period is okay")
#' }
#' @export
copy_model_from <- function(
  .parent_mod,
  .new_model,
  .description = NULL,
  .based_on_additional = NULL,
  .add_tags = NULL,
  .inherit_tags = TRUE,
  .update_model_file = TRUE,
  .overwrite = FALSE
) {
  UseMethod("copy_model_from")
}

#' @describeIn copy_model_from `.parent_mod` takes a `bbi_nonmem_model` object to use as a basis for the copy.
#' @export
copy_model_from.bbi_nonmem_model <- function(
  .parent_mod,
  .new_model,
  .description = NULL,
  .based_on_additional = NULL,
  .add_tags = NULL,
  .inherit_tags = TRUE,
  .update_model_file = TRUE,
  .overwrite = FALSE
) {

  .new_model <- build_new_model_path(.parent_mod, .new_model)

  .mod <- copy_model_from_impl(
    .parent_mod = .parent_mod,
    .new_model = .new_model,
    .description = .description,
    .based_on_additional = .based_on_additional,
    .add_tags = .add_tags,
    .inherit_tags = .inherit_tags,
    .update_model_file = .update_model_file,
    .overwrite = .overwrite,
    .model_type = "nonmem"
  )

  return(.mod)
}

#' @describeIn copy_model_from `.parent_mod` takes a `bbi_stan_model` object to use as a basis for the copy.
#' @export
copy_model_from.bbi_stan_model <- function(
  .parent_mod,
  .new_model,
  .description = NULL,
  .based_on_additional = NULL,
  .add_tags = NULL,
  .inherit_tags = TRUE,
  .update_model_file = TRUE,
  .overwrite = FALSE
) {

  check_stan_model(.parent_mod, .error = TRUE)

  .new_model <- build_new_model_path(.parent_mod, .new_model)

  .mod <- copy_model_from_impl(
    .parent_mod = .parent_mod,
    .new_model = .new_model,
    .description = .description,
    .based_on_additional = .based_on_additional,
    .add_tags = .add_tags,
    .inherit_tags = .inherit_tags,
    .update_model_file = .update_model_file,
    .overwrite = .overwrite,
    .model_type = "stan"
  )

  return(.mod)
}

#####################################
# Private implementation function(s)
#####################################

#' Copy model from an existing model
#'
#' Private implementation function called by `copy_model_from()` dispatches.
#' Create new new .yaml and other necessary files based on a previous model.
#' Used for iterating on model development. Also fills in necessary YAML fields
#' for using `run_log()` later.
#' @param .parent_mod S3 object of class `bbi_{.model_type}_model` to be used as the basis for copy.
#' @param .update_model_file **Only relevant for NONMEM models.** If `TRUE`, the
#'   default, update the `$PROBLEM` line in the new control stream. If `FALSE`,
#'   `{.new_model}.[mod|ctl]` will be an exact copy of its parent control
#'   stream.
#' @inheritParams copy_model_from
#' @importFrom fs file_copy path_rel is_absolute_path
#' @importFrom readr read_file write_file
#' @importFrom stringr str_replace
#' @importFrom yaml write_yaml
#' @importFrom purrr list_modify
#' @importFrom digest digest
#' @return S3 object with the same class as `.parent_mod`
#' @keywords internal
copy_model_from_impl <- function(
  .parent_mod,
  .new_model,
  .description = NULL,
  .based_on_additional = NULL,
  .add_tags = NULL,
  .inherit_tags = TRUE,
  .update_model_file = TRUE,
  .overwrite = FALSE,
  .model_type = c("nonmem", "stan")
) {

  .model_type <- match.arg(.model_type)

  check_for_existing_model(.new_model, .overwrite)

  # check parent
  check_yaml_in_sync(.parent_mod)

  # build based_on
  if(!fs::is_absolute_path(.new_model)) {
    dev_error(".new_model argument to copy_model_from_impl() must be absolute.")
  }
  .parent_based_on <- fs::path_rel(.parent_mod[[ABS_MOD_PATH]], start = dirname(.new_model))

  # build tags
  if (.inherit_tags && !is.null(.parent_mod[[YAML_TAGS]])) {
    new_tags <- c(.parent_mod[[YAML_TAGS]], .add_tags)
  } else {
    new_tags <- .add_tags
  }

  if (inherits(.parent_mod, NM_MOD_CLASS)) {
    # copy control steam to new path
    .parent_model_path <- get_model_path(.parent_mod)
    parent_ext <- fs::path_ext(.parent_model_path)
    .new_model_path <- paste(.new_model, parent_ext, sep = ".")
    copy_control_stream(.parent_model_path, .new_model_path, .overwrite, .update_model_file)
  } else if (inherits(.parent_mod, STAN_MOD_CLASS)) {
    copy_stan_files(.parent_mod, .new_model, .overwrite)
  } else {
    dev_error(paste(
      "Unsupported object passed to copy_model_from_impl(). Classes: ",
      paste(class(.parent_mod), collapse = ", ")
    ))
  }

  # create new model
  .new_mod <- new_model(
    .new_model,
    .description = .description,
    .based_on = c(.parent_based_on, .based_on_additional),
    .tags = new_tags,
    .bbi_args = .parent_mod[[YAML_BBI_ARGS]],
    .overwrite = FALSE, # will have already overwritten if necessary
    .model_type = .model_type
  )

  return(.new_mod)
}


#' Copy a NONMEM control stream file
#'
#' Helper function to copy a NONMEM control stream file and optionally update the new file.
#' Note that any file existing at `.new_model_path` will be overwritten.
#' @param .parent_model_path Path to the control stream to copy
#' @param .new_model_path Path to copy the new control stream to
#' @param .overwrite If `TRUE`, overwrite existing file at `.new_model_path`. If `FALSE` and file exists at `.new_model_path` error.
#' @param .update_model_file If `TRUE`, the default, update the `$PROBLEM` line in the new control stream. If `FALSE`, `{.new_model}.[mod|ctl]` will be an exact copy of its parent control stream.
#' @keywords internal
copy_control_stream <- function(.parent_model_path, .new_model_path, .overwrite, .update_model_file = FALSE) {

  if (fs::file_exists(.new_model_path)) {
    if (isTRUE(.overwrite)) {
      fs::file_delete(.new_model_path)
    } else {
      stop(glue("File already exists at {.new_model_path} -- cannot copy new control stream. Either pass `.overwrite = TRUE` or use `new_model({tools::file_path_sans_ext(.new_model_path)})`"))
    }

  }

  if (.update_model_file) {

    # read parent control stream
    mod_str <- .parent_model_path %>% read_file()

    # replace the $PROBLEM line(s)
    new_mod_id <- get_model_id(.new_model_path)
    mod_str <- str_replace(mod_str,
                           "\\$PROB(.|\n)*?\\$",
                           as.character(glue("$PROBLEM From bbr: see {new_mod_id}.yaml for details\n\n$")))
    glue("")

    # read parent control stream
    write_file(mod_str, .new_model_path)
  } else {
    fs::file_copy(.parent_model_path, .new_model_path)
  }
}


#' Copy necessary Stan files
#'
#' Helper function to copy a necessary Stan files from parent model to new model
#' and rename them appropriately. Note that any directory existing at
#' `.new_model` will be overwritten by default.
#' @param .parent_mod a `bbi_stan_model` object to copy from
#' @param .new_model Path to new model directory
#' @param .overwrite If `TRUE`, overwrite existing directory at `.new_model`. If `FALSE` and directory exists at `.new_model` error.
#' @keywords internal
copy_stan_files <- function(.parent_mod, .new_model, .overwrite) {

  if (fs::dir_exists(.new_model)) {
    if (isTRUE(.overwrite)) {
      fs::dir_delete(.new_model)
    } else {
      stop(glue("Directory already exists at {.new_model} -- cannot copy new model files. Either pass `.overwrite = TRUE` or use `new_model({.new_model})`"))
    }
  }
  fs::dir_create(.new_model)

  # can't use real build_path_from_model() because the new model doesn't exist yet
  build_path_from_new_model_path <- function(.new_model, .suffix) {
    file.path(
      .new_model,
      paste0(basename(.new_model), .suffix)
    )
  }

  purrr::walk(STAN_MODEL_FILES_TO_CHECK, function(.s) {
    parent_file <- build_path_from_model(.parent_mod, .s)
    if (fs::file_exists(parent_file)) {
      fs::file_copy(
        parent_file,
        build_path_from_new_model_path(.new_model, .s)
      )
    }
  })

  # if -standata.R is a brms stub, copy through -standata.json
  parent_standata_r_path <- build_path_from_model(.parent_mod, STANDATA_R_SUFFIX)
  parent_standata_json_path <- build_path_from_model(.parent_mod, STANDATA_JSON_SUFFIX)
  if (file_matches_string(parent_standata_r_path, STANDATA_BRMS_COMMENT)) {
    fs::file_copy(
      parent_standata_json_path,
      build_path_from_new_model_path(.new_model, STANDATA_JSON_SUFFIX)
    )
  }
}

#' Private helper to build absolute path for [copy_model_from()].
#' Importantly, if the input `.new_model` is _not_ absolute, it will
#' be treated as relative to the working directory of `.parent_mod`.
#' @inheritParams copy_model_from
#' @return absolute file path to save new model to (without file extension)
#' @keywords internal
build_new_model_path <- function(.parent_mod, .new_model) {
  if (!fs::is_absolute_path(.new_model)) {
    .new_model <- as.character(.new_model)
    .new_model <- file.path(get_model_working_directory(.parent_mod), .new_model)
    .new_model <- fs::path_norm(.new_model)
  }
  return(.new_model)
}
