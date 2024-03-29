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
#'   character. If `NULL`, the default, will try to increment to the next integer
#'   in the destination directory. See examples for usage.
#' @param .description Character scalar description of new model run. This will
#'   be stored in the yaml (and can be viewed later in `run_log()`).
#' @param .based_on_additional Character vector of path(s) to other models that
#'   this model was "based on." These are used to reconstuct model developement
#'   and ancestry. **Paths must be relative to `.new_model` path.** Note that
#'   the `.parent_model` will automatically be added to the `based_on` field, so
#'   no need to include that here.
#' @param .star Boolean, marks model to indicate special interest level.
#' @param .add_tags Character vector with any new tags(s) to be added to
#'   `{.new_model}.yaml`
#' @param .inherit_tags If `FALSE`, the default, new model will only have any
#'   tags passed to `.add_tags` argument. If `TRUE` inherit any tags from
#'   `.parent_mod`, with any tags passed to `.add_tags` appended.
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
#' copy_model_from(mod1, "increments to next integer by default")
#'
#' # create model file at /path/to/3.ctl and YAML at /path/to/3.yaml
#' copy_model_from(mod1, 3, "numeric input works")
#'
#' # create model file at /path/to/100.1.ctl and YAML at /path/to/100.1.yaml
#' copy_model_from(mod1, 100.1, "a period is okay")
#' }
#' @seealso [new_model()], [update_model_id()]
#' @export
copy_model_from <- function(
    .parent_mod,
    .new_model = NULL,
    .description = NULL,
    .based_on_additional = NULL,
    .add_tags = NULL,
    .star = NULL,
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
    .new_model = NULL,
    .description = NULL,
    .based_on_additional = NULL,
    .add_tags = NULL,
    .star = NULL,
    .inherit_tags = FALSE,
    .update_model_file = TRUE,
    .overwrite = FALSE
) {

  .new_model <- build_new_model_path(.parent_mod, .new_model)

  copy_ctl <- function() {
    .parent_model_path <- get_model_path(.parent_mod)
    parent_ext <- fs::path_ext(.parent_model_path)
    .new_model_path <- paste(.new_model, parent_ext, sep = ".")
    copy_control_stream(.parent_model_path, .new_model_path, .overwrite,
                        .update_model_file)
  }

  .mod <- copy_model_from_impl(
    .parent_mod = .parent_mod,
    .new_model = .new_model,
    .description = .description,
    .based_on_additional = .based_on_additional,
    .add_tags = .add_tags,
    .star = .star,
    .inherit_tags = .inherit_tags,
    .update_model_file = .update_model_file,
    .overwrite = .overwrite,
    setup_fn = copy_ctl
  )

  return(.mod)
}

#####################################
# Private implementation function(s)
#####################################

# SHARED: copy_model_from_impl() is used by bbr.bayes, so any changes here
# should be compatible with its use there.

#' Copy model from an existing model
#'
#' Private implementation function called by `copy_model_from()` dispatches.
#' Create new .yaml and other necessary files based on a previous model.
#' Used for iterating on model development. Also fills in necessary YAML fields
#' for using `run_log()` later.
#' @param .parent_mod S3 object of class `bbi_{.model_type}_model` to be used as the basis for copy.
#' @param .update_model_file **Only relevant for NONMEM models.** If `TRUE`, the
#'   default, update the `$PROBLEM` line in the new control stream. If `FALSE`,
#'   `{.new_model}.[mod|ctl]` will be an exact copy of its parent control
#'   stream.
#' @param setup_fn A function to call (with no arguments) before creating the
#'   model with [new_model()].
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
    .star = NULL,
    .inherit_tags = FALSE,
    .update_model_file = TRUE,
    .overwrite = FALSE,
    setup_fn = NULL
) {
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

  if (!is.null(setup_fn)) {
    setup_fn()
  }

  mtype <- stringr::str_replace(
    class(.parent_mod)[1], "^bbi_(.*)_model$", "\\1")
  # create new model
  .new_mod <- new_model(
    .new_model,
    .description = .description,
    .based_on = c(.parent_based_on, .based_on_additional),
    .tags = new_tags,
    .star = .star,
    .bbi_args = .parent_mod[[YAML_BBI_ARGS]],
    .overwrite = FALSE, # will have already overwritten if necessary
    .model_type = mtype
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

  # if copying to new dir, create the dir first
  if(!fs::dir_exists(dirname(.new_model_path))) {
    fs::dir_create(dirname(.new_model_path))
  }

  if (.update_model_file) {

    # read parent control stream
    mod_str <- tryCatch(nmrec::read_ctl(.parent_model_path), error = identity)

    if(inherits(mod_str, "error")){
      warning(
        glue::glue(".update_model_file = TRUE was not performed. Reason: {mod_str$message}")
      )
      fs::file_copy(.parent_model_path, .new_model_path)
    }else{
      # replace the $PROBLEM line(s)
      new_mod_id <- get_model_id(.new_model_path)
      prob_recs <- nmrec::select_records(mod_str, "prob")
      if (length(prob_recs) != 1) {
        dev_error("select_records should always return a single $PROBLEM record")
      }
      prob_rec <- prob_recs[[1]]
      prob_str <- nmrec::get_record_option(prob_rec, "text")
      new_prob_text <- as.character(glue("From bbr: see {new_mod_id}.yaml for details"))
      if(is.null(prob_str)){
        # NONMEM permits a bare "$PROB{newline}", in which case there won't be a text option.
        prob_rec$parse()
        idx <- purrr::detect_index(
          prob_rec$values,
          function(x) inherits(x, "nmrec_option_record_name")
        )
        if(idx == 0){
          dev_error("Unsupported $PROBLEM record format")
        }
        prob_rec$values <- append(prob_rec$values, paste0(" ", new_prob_text), idx)
      }else{
        prob_str$value <- new_prob_text
      }

      # write parent control stream
      nmrec::write_ctl(mod_str, .new_model_path)
    }

  } else {
    fs::file_copy(.parent_model_path, .new_model_path)
  }
}

# SHARED: build_new_model_path() is used by bbr.bayes, so any changes here
# should be compatible with its use there.

#' Private helper to build absolute path for [copy_model_from()].
#' Importantly, if the input `.new_model` is _not_ absolute, it will
#' be treated as relative to the working directory of `.parent_mod`.
#' @inheritParams copy_model_from
#' @return absolute file path to save new model to (without file extension)
#' @keywords internal
build_new_model_path <- function(.parent_mod, .new_model) {
  if (is.null(.new_model)){
    .new_model <- get_next_integer(.parent_mod)
  }

  if (length(.new_model) != 1) {
    stop(paste(
      glue("copy_model_from(.new_model) must be either a string or numeric, but got length {length(.new_model)}:\n\n"),
      paste(.new_model, collapse = ", ")
    ))
  }

  if (!fs::is_absolute_path(.new_model)) {
    .new_model <- as.character(.new_model)
    .new_model <- file.path(get_model_working_directory(.parent_mod), .new_model)
    .new_model <- fs::path_norm(.new_model)
  }
  return(.new_model)
}

#' Check directory for model files with integer names
#' and return the next integer. Used by copy_model_from().
#' @importFrom stringr str_extract_all str_pad
#' @keywords internal
get_next_integer <- function(.parent_mod){
  .dir <- dirname(get_model_path(.parent_mod))
  .ext <- tools::file_ext(get_model_path(.parent_mod))

  # find model in same dir with largest integer name
  mods <- fs::dir_ls(.dir, regexp=glue("\\.{.ext}$")) %>%
    basename() %>%
    tools::file_path_sans_ext()

  mods_int <- suppressSpecificWarning(as.integer(mods), .regexpr = "NAs introduced by coercion")
  if (all(is.na(mods_int))) {
    stop(paste(
      glue("There are no models in {.dir} with integer names, so cannot increment `.new_model` to next integer."),
      "Please pass a valid name to `copy_model_from(.new_model)`."
    ))
  }
  largest_mod <- which(mods_int == max(mods_int, na.rm = TRUE))
  pick <- as.character(mods_int[largest_mod] + 1)

  # pad if necessary
  .nc <- nchar(mods[largest_mod])
  if(nchar(pick) < .nc) {
    pick <- str_pad(pick, .nc, side = "left", pad = "0")
  }

  return(pick)
}
