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
#'   path to `.parent_model`. Represents an absolute model path, which is the
#'   path to the YAML file and model file, both without extension, and the
#'   output directory (once the model is run). Numeric values will be coerced to
#'   character. See examples for usage.
#' @param .description Description of new model run. This will be stored in the
#'   yaml (to be used later in `run_log()`).
#' @param .based_on_additional Character vector of path(s) to other models that
#'   this model was "based on." These are used to reconstuct model developement
#'   and ancestry. **Paths must be relative to `.new_model` path.** Note that
#'   the `.parent_model` will automatically be added to the `based_on` field, so
#'   no need to include that here.
#' @param .add_tags Character vector with any new tags(s) to be added to
#'   `{.new_model}.yaml`
#' @param .inherit_tags If `FALSE`, the default, new model will only have any
#'   tags passed to `.add_tags` argument. If `TRUE` inherit any tags from
#'   `.parent_mod`, with any tags passed to `.add_tags` appended.
#' @param .update_model_file If `TRUE`, the default, update the newly created
#'   model file with new description and name. For a NONMEM model, this
#'   currently means only the `$PROBLEM` line in the new control stream will be
#'   updated. If `FALSE`, new model file will be an exact copy of its parent.
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
  .description,
  .based_on_additional = NULL,
  .add_tags = NULL,
  .inherit_tags = FALSE,
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
  .description,
  .based_on_additional = NULL,
  .add_tags = NULL,
  .inherit_tags = FALSE,
  .update_model_file = TRUE,
  .overwrite = FALSE
) {

  .new_model <- build_new_model_path(.parent_mod, .new_model)

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

#####################################
# Private implementation function(s)
#####################################

#' Copy model from an existing NONMEM model
#'
#' Private implementation function called by `copy_model_from()` dispatches.
#' Create new .mod/ctl and new .yaml files based on a previous model. Used for iterating on model development.
#' Also fills in necessary YAML fields for using `run_log()` later.
#' @param .parent_mod S3 object of class `bbi_nonmem_model` to be used as the basis for copy.
#' @param .description Description of new model run. This will be stored in the yaml (to be used later in `run_log()`) and optionally passed into the `$PROBLEM` of the new control stream.
#' @param .update_model_file If `TRUE`, the default, update the `$PROBLEM` line in the new control stream. If `FALSE`, `{.new_model}.[mod|ctl]` will be an exact copy of its parent control stream.
#' @importFrom fs file_copy path_rel is_absolute_path
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

  # build based_on
  if(!fs::is_absolute_path(.new_model)) {
    stop(".new_model argument to copy_nonmem_model_from() must be absolute. USER SHOULD NOT SEE THIS ERROR.")
  }
  .parent_based_on <- fs::path_rel(get_model_path(.parent_mod), start = dirname(.new_model))

  # build tags
  if (.inherit_tags && !is.null(.parent_mod[[YAML_TAGS]])) {
    new_tags <- c(.parent_mod[[YAML_TAGS]], .add_tags)
  } else {
    new_tags <- .add_tags
  }

  # copy control steam to new path
  .parent_model_path <- get_model_path(.parent_mod)
  parent_ext <- fs::path_ext(.parent_model_path)
  .new_model_path <- paste(.new_model, parent_ext, sep = ".")
  copy_control_stream(.parent_model_path, .new_model_path, .overwrite, .update_model_file, .description)

  # create new model
  .new_mod <- new_model(
    .new_model,
    .description = .description,
    .based_on = c(.parent_based_on, .based_on_additional),
    .tags = new_tags,
    .bbi_args = .parent_mod[[YAML_BBI_ARGS]],
    .overwrite = .overwrite,
    .model_type = "nonmem"
  )

  return(.new_mod)
}


#' Copy a NONMEM control stream file
#'
#' Helper function to copy a NONMEM control stream file and optionally update the description, etc. in the new file.
#' Note that any file existing at `.new_model_path` will be overwritten.
#' @param .parent_model_path Path to the control stream to copy
#' @param .new_model_path Path to copy the new control stream to
#' @param .overwrite If `TRUE`, overwrite existing file at `.new_model_path`. If `FALSE` and file exists at `.new_model_path` error.
#' @param .update_model_file If `TRUE`, the default, update the `$PROBLEM` line in the new control stream. If `FALSE`, `{.new_model}.[mod|ctl]` will be an exact copy of its parent control stream.
#' @param .description Description of new model run. This will be passed into the `$PROBLEM` of the new control stream (if `.update_model_file=TRUE`).
#' @keywords internal
copy_control_stream <- function(.parent_model_path, .new_model_path, .overwrite, .update_model_file = FALSE, .description = NULL) {

  if (fs::file_exists(.new_model_path) && !isTRUE(.overwrite)) {
    stop(glue("File already exists at {.new_model_path} -- cannot copy new control stream. Either delete old file or use `new_model({yaml_ext(.new_model_path)})`"))
  }

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
