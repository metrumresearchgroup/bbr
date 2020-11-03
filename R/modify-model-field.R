#####################
# Modifying models
#####################

#' Modify field in model object
#'
#' Helper functions for updating fields in a `bbi_{.model_type}_model` object.
#' Note that calling `modify_model_field()` or `replace_model_field()` directly
#' is _not_ recommended for most users because it requires knowing about the
#' internal structure of the model object. Instead, **we recommend using the
#' friendlier helpers listed below** (`add_...` or `replace_...`) when possible.
#'
#' @details
#' All functions in this family also check the object against the corresponding YAML with `check_yaml_in_sync()` before modifying it,
#' and errors if they are out of sync.
#' After the object has been modified they will write the modified object back to the YAML and update the model object in
#' memory with an md5 digest of the newly written YAML.
#' @param .mod The `bbi_{.model_type}_model` object to modify
#' @param .field Character scalar of the name of the component to modify
#' @param .value Whatever is to be added to `.mod[[.field]]`, typically a character vector
#' @param .append If `TRUE`, the default, concatenate new values with currently present values. If `FALSE`, new values will overwrite old values.
#' @param .remove If `TRUE`, `.value` with be removed from the `.field` instead of added. `FALSE` by default. Cannot have both `.append` and `.remove` be true in the same call.
#' @param .unique If `TRUE`, the default, de-duplicate `.mod[[.field]]` after adding new values. If `FALSE` duplicate values will be kept.
#' @export
modify_model_field <- function(.mod, .field, .value, .append = TRUE, .remove = FALSE, .unique = TRUE) {

  # update .mod with any changes from yaml on disk
  check_yaml_in_sync(.mod)

  if (isTRUE(.append) & isTRUE(.remove)) {
    stop("modify_model_field() cannot have both `.append` and `.remove` be true in the same call.")
  }

  if (isTRUE(.append)) {
    .mod[[.field]] <- c(.mod[[.field]], .value)
  } else if (isTRUE(.remove)) {
    # first warn if trying to remove values that aren't present, then remove any that are
    missing <- !(.value %in% .mod[[.field]])
    if (any(missing)) {
      missing_vals <- paste(.value[which(missing)], collapse = ", ")
      warning(glue("`{.field}` does not contain any of the following, so they cannot be removed: {missing_vals}"))
    }
    .mod[[.field]] <- setdiff(.mod[[.field]], .value)
  } else {
    .mod[[.field]] <- .value
  }

  # de-duplicate values
  if (isTRUE(.unique)) {
    .mod[[.field]] <- .mod[[.field]] %>% unique()
  }

  # overwrite the yaml on disk with modified model
  .mod <- save_model_yaml(.mod)

  return(.mod)
}

#' @describeIn modify_model_field Replace a single item in a model field
#' @param .old_val The value to be replaced. If `.old_val` is not present in
#'   `.mod[[.field]]`, function will warn user and return `.mod` unchanged.
#' @param .new_val The value to insert in place of `.old_val` in
#'   `.mod[[.field]]`.
#' @export
replace_model_field <- function(.mod, .field, .old_val, .new_val) {

  # update .mod with any changes from yaml on disk
  check_yaml_in_sync(.mod)

  idx <- which(.mod[[.field]] == .old_val)
  if (length(idx) == 0) {
    warning(glue("`{.field}` does not contain {.old_val}, so it cannot be replaced."))
    return(.mod)
  }

  .mod[[.field]][idx] <- .new_val

  return(.mod)
}


#####################
# FRIENDLIER HELPERS
#####################

#' @describeIn modify_model_field Add tags to a model object and corresponding YAML
#' @param .tags Character vector to add to `tags` field
#' @export
add_tags <- function(.mod, .tags) {
  modify_model_field(
    .mod = .mod,
    .field = YAML_TAGS,
    .value = .tags,
    .append = TRUE
  )
}

#' @describeIn modify_model_field Replaces a specific `.old_tag` with `.new_tag` on a model object and corresponding YAML.
#' Warns and does nothing if `.old_tag` is not present.
#' @param .old_tag Character scalar of tag to be replaced
#' @param .new_tag Character scalar of tag that will be added
#' @export
replace_tag <- function(.mod, .old_tag, .new_tag) {
  replace_model_field(.mod, YAML_TAGS, .old_tag, .new_tag)
}

#' @describeIn modify_model_field Replaces all tags on a model object and corresponding YAML with new tags
#' @export
replace_all_tags <- function(.mod, .tags) {
  modify_model_field(
    .mod = .mod,
    .field = YAML_TAGS,
    .value = .tags,
    .append = FALSE
  )
}

#' @describeIn modify_model_field _Deprecated_ as of rbabylon 0.10.0, use `replace_all_tags()` instead.
#' @export
replace_tags <- function(.mod, .tags) {
  deprecate_warn("0.10.0", "rbabylon::replace_tags()", "replace_all_tags()")
  replace_all_tags(.mod, .tags)
}

#' @describeIn modify_model_field Removes tags from a model object and corresponding YAML
#' @export
remove_tags <- function(.mod, .tags) {
  modify_model_field(
    .mod = .mod,
    .field = YAML_TAGS,
    .value = .tags,
    .append = FALSE,
    .remove = TRUE
  )
}


#' @describeIn modify_model_field Add notes to a model object and corresponding YAML
#' @param .notes Character vector to add to `notes` field
#' @export
add_notes <- function(.mod, .notes) {
  modify_model_field(
    .mod = .mod,
    .field = YAML_NOTES,
    .value = .notes,
    .append = TRUE
  )
}

#' @describeIn modify_model_field Replaces a specific `.old_note` with `.new_note` on a model object and corresponding YAML.
#' Warns and does nothing if `.old_note` is not present.
#' @param .old_note Character scalar of note to be replaced
#' @param .new_note Character scalar of note that will be added
#' @export
replace_note <- function(.mod, .old_note, .new_note) {
  replace_model_field(.mod, YAML_NOTES, .old_note, .new_note)
}

#' @describeIn modify_model_field Replaces all notes on a model object and corresponding YAML with new notes
#' @export
replace_all_notes <- function(.mod, .notes) {
  modify_model_field(
    .mod = .mod,
    .field = YAML_NOTES,
    .value = .notes,
    .append = FALSE
  )
}

#' @describeIn modify_model_field Removes notes from a model object and corresponding YAML
#' @export
remove_notes <- function(.mod, .notes) {
  modify_model_field(
    .mod = .mod,
    .field = YAML_NOTES,
    .value = .notes,
    .append = FALSE,
    .remove = TRUE
  )
}

#' @describeIn modify_model_field Append new `based_on` tag(s) to a model object and corresponding YAML
#' @param .based_on Character vector of relative paths to add to `based_on` field
#' @export
add_based_on <- function(.mod, .based_on) {
  modify_model_field(
    .mod = .mod,
    .field = YAML_BASED_ON,
    .value = safe_based_on(get_model_working_directory(.mod), .based_on),
    .append = TRUE
  )
}

#' @describeIn modify_model_field Replaces entire `based_on` field in a model object and corresponding YAML with new values
#' @export
replace_all_based_on <- function(.mod, .based_on) {
  modify_model_field(
    .mod = .mod,
    .field = YAML_BASED_ON,
    .value = safe_based_on(get_model_working_directory(.mod), .based_on),
    .append = FALSE
  )
}

#' @describeIn modify_model_field _Deprecated_ as of rbabylon 0.10.0, use `replace_all_based_on()` instead.
#' @export
replace_based_on <- function(.mod, .based_on) {
  deprecate_warn("0.10.0", "rbabylon::replace_based_on()", "replace_all_based_on()")
  replace_all_based_on(.mod, .based_on)
}

#' @describeIn modify_model_field Remove specified `based_on` tag(s) from a model object and corresponding YAML
#' @param .based_on Character vector of relative paths to add to `based_on` field
#' @export
remove_based_on <- function(.mod, .based_on) {
  modify_model_field(
    .mod = .mod,
    .field = YAML_BASED_ON,
    .value = safe_based_on(get_model_working_directory(.mod), .based_on),
    .append = FALSE,
    .remove = TRUE
  )
}

#' @describeIn modify_model_field Replaces description field in a model object and corresponding YAML with new description
#' @importFrom checkmate assert_scalar
#' @param .description Character scalar to use as replacement for the `description` field
#' @export
replace_description <- function(.mod, .description) {
  checkmate::assert_scalar(.description, na.ok = TRUE, null.ok = TRUE)
  modify_model_field(
    .mod = .mod,
    .field = YAML_DESCRIPTION,
    .value = .description,
    .append = FALSE
  )
}

#' @describeIn modify_model_field Fills the description field in a model object and corresponding YAML, if it is currently empty.
#' @importFrom checkmate assert_scalar
#' @export
add_description <- function(.mod, .description) {
  if (!is.null(.mod[[YAML_DESCRIPTION]])) {
    stop(paste(
      "`description` field is not empty. Please use `replace_description()` to replace contents.",
      glue("Current description: {.mod[[YAML_DESCRIPTION]]}")
    ))
  }
  replace_description(.mod, .description)
}

#' @describeIn modify_model_field Modifies model object and corresponding YAML
#'   by adding named list passed to `.bbi_args`, overwriting any args that are
#'   already present with the new values. Use [print_bbi_args()] to see a list
#'   of valid babylon arguments.
#' @param .bbi_args named list of arguments to add to the model
#' @export
add_bbi_args <- function(.mod, .bbi_args) {

  # update .mod with any changes from yaml on disk
  check_yaml_in_sync(.mod)

  # combine the two lists, with .bbi_args overwriting any keys that are shared
  .mod[[YAML_BBI_ARGS]] <- parse_args_list(.bbi_args, .mod[[YAML_BBI_ARGS]])

  # overwrite the yaml on disk with modified model
  .mod <- save_model_yaml(.mod)

  return(.mod)
}

#' @describeIn modify_model_field Modifies model object and corresponding YAML
#'   by replacing all `bbi_args` with named list passed to `.bbi_args`. Use
#'   [print_bbi_args()] to see a list of valid babylon arguments.
#' @export
replace_all_bbi_args <- function(.mod, .bbi_args) {

  # update .mod with any changes from yaml on disk
  check_yaml_in_sync(.mod)

  # overwrite all previous
  .mod[[YAML_BBI_ARGS]] <- .bbi_args

  # overwrite the yaml on disk with modified model
  .mod <- save_model_yaml(.mod)

  return(.mod)
}

#' @describeIn modify_model_field _Deprecated_ as of rbabylon 0.10.0, use `replace_all_bbi_args()` instead.
#' @export
replace_bbi_args <- function(.mod, .bbi_args) {
  deprecate_warn("0.10.0", "rbabylon::replace_bbi_args()", "replace_all_bbi_args()")
  replace_all_bbi_args(.mod, .bbi_args)
}

#' @describeIn modify_model_field _Deprecated_  Append new decisions to the one(s) in a model object and corresponding YAML
#' @param .decisions Character vector to add to `decisions` field
#' @export
add_decisions <- function(.mod, .decisions) {
  warning("The `decisions` field has been replaced by `notes` as of rbabylon 0.10.0 and will be removed in a future release. Please use `add_notes()` going forward.")
  modify_model_field(
    .mod = .mod,
    .field = YAML_DECISIONS,
    .value = .decisions,
    .append = TRUE
  )
}

#' @describeIn modify_model_field _Deprecaed_ Replaces `decisions` field in a model object and corresponding YAML with new values
#' @param .decisions Character vector to use as replacement
#' @export
replace_decisions <- function(.mod, .decisions) {
  warning("The `decisions` field has been replaced by `notes` as of rbabylon 0.10.0 and will be removed in a future release. Please use `replace_all_notes()` going forward.")
  modify_model_field(
    .mod = .mod,
    .field = YAML_DECISIONS,
    .value = .decisions,
    .append = FALSE
  )
}

###########################
# private helper functions
###########################

#' Checks for yaml files relative to a starting location
#' @importFrom fs file_exists is_absolute_path path_rel
#' @importFrom purrr map_lgl
#' @param .start The directory of the model (i.e. the YAML file) that the `based_on` will be added to.
#' @param .based_on Character vector or scalar of paths (with or without extension) to the models that will be added to `based_on`. Paths should be relative to `.start` argument.
#' @keywords internal
safe_based_on <- function(.start, .based_on) {
  # make all input paths relative to .start
  .based_on <- map_chr(.based_on, function(.p) {
    if (fs::is_absolute_path(.p)) {
      .p <- fs::path_rel(.p, .start)
    }
    return(.p)
  })

  .paths <- file.path(.start, .based_on)

  # check for a .yaml file at each location
  .paths_bool <-
    .paths %>%
    purrr::map(yaml_ext) %>%
    purrr::map_lgl(fs::file_exists)

  names(.paths_bool) <- .paths

  if (!all(.paths_bool)) {
    strict_mode_error(paste(
      glue("Parsed {length(.paths_bool)} models as `based_on` but cannot find .yaml files for {length(.paths_bool) - sum(.paths_bool)} of them: "),
      paste(names(which(!.paths_bool)), collapse = ', ')
    ))
  }

  return(tools::file_path_sans_ext(.based_on))
}


################
# synching YAML
################

#' Verify YAML file integrity
#'
#' These functions use an md5 digest of the model YAML file, stored each time a model is loaded into memory,
#' to keep track of any changes made to the YAML that are _not_ reflected in the object held in memory.
#' @name verify_model_yaml_integrity
#' @param .mod `bbi_{.model_type}_model` object
NULL

#' @describeIn verify_model_yaml_integrity Use to manually reconcile model object in memory with its YAML file.
#' Extracts YAML path from model object and pulls in YAML file.
#' Any shared keys are overwritten with the values from the YAML and new keys in YAML are added to the model object.
#' The md5 digest is then updated to reflect the new state.
#' @export
reconcile_yaml <- function(.mod) {
  check_model_object(.mod)

  # extract path to yaml
  .yaml_path <- get_yaml_path(.mod)

  # load model from yaml on disk
  .loaded_mod <- read_model(fs::path_ext_remove(.yaml_path))

  # overwrite values in memory from the ones on disk
  .new_mod <- combine_list_objects(.loaded_mod, .mod)

  return(.new_mod)
}


#' @describeIn verify_model_yaml_integrity Checks that model YAML file is the same as when it was last read into the model object.
#' Errors if the md5 digests are not identical. This is called internally in most functions that interact with a model object.
#' @importFrom digest digest
#' @export
check_yaml_in_sync <- function(.mod) {
  check_model_object(.mod)

  # get md5 of current YAML on disk
  .yaml_file <- get_yaml_path(.mod)
  current_md5 <- digest(file = .yaml_file, algo = "md5")

  # check against md5 stored on load
  if (current_md5 != .mod[[YAML_YAML_MD5]]) {
    check_yaml_err_msg <- paste(glue("Model NOT in sync with corresponding YAML file {.yaml_file}"),
                                "User can call `mod <- reconcile_yaml(mod)` to pull in updates from the YAML on disk.",
                                "NOTE: This can be caused by modifying the object without reassigning for instance calling `mod %>% add_tags(...)` instead of `mod <- mod %>% add_tags(...)`.",
                                "Users should avoid this pattern or this error will be generated the next time this model object is passed to a function.",
                                sep = "\n")
    strict_mode_error(check_yaml_err_msg)
  }
}


