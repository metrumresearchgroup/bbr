#####################
# Modifying models
#####################

#' Modify field in model object
#'
#' Helper functions for updating fields in a `bbi_{.model_type}_model` object.
#' Note that calling `modify_model_field()` or `replace_model_field()` directly
#' is _not_ recommended for most users because it requires knowing about the
#' internal structure of the model object. Instead, **we recommend using the
#' friendlier helpers listed below in "See also"** whenever possible.
#'
#' @details
#' All functions in this family also check the object against the corresponding
#' YAML with `check_yaml_in_sync()` before modifying it, and errors if they are
#' out of sync. After the object has been modified they will write the modified
#' object back to the YAML and update the model object in memory with an md5
#' digest of the newly written YAML.
#'
#' @seealso [add_tags()] [replace_tag()] [replace_all_tags()] [remove_tags()]
#'   [add_notes()] [replace_note()] [replace_all_notes()] [remove_notes()]
#'   [add_based_on()] [replace_all_based_on()] [remove_based_on()]
#'   [add_description()] [replace_description()] [add_bbi_args()]
#'   [replace_all_bbi_args()]
#'
#' @param .mod The `bbi_{.model_type}_model` object to modify
#' @param .field Character scalar of the name of the component to modify
#' @param .value Whatever is to be added to `.mod[[.field]]`, typically a character vector (or a named list in the case of `*_bbi_args()`).
#'   If `NULL` or `NA` is passed and `.append = FALSE` then `.field` with be deleted from the object and corresponding YAML.
#' @param .append If `TRUE`, the default, concatenate new values with currently present values. If `FALSE`, new values will overwrite old values.
#' @param .remove If `TRUE`, `.value` with be removed from the `.field` instead of added. `FALSE` by default. Cannot have both `.append` and `.remove` be true in the same call.
#' @param .unique If `TRUE`, the default, de-duplicate `.mod[[.field]]` after adding new values. If `FALSE` duplicate values will be kept.
#' @param .char_value If `TRUE`, check that `.value` (after unlisting) is a character vector.
#' @export
modify_model_field <- function(.mod, .field, .value, .append = TRUE, .remove = FALSE, .unique = TRUE, .char_value = TRUE) {

  # update .mod with any changes from yaml on disk
  check_yaml_in_sync(.mod)

  if (isTRUE(.append) & isTRUE(.remove)) {
    stop("modify_model_field() cannot have both `.append` and `.remove` be true in the same call.")
  }

  if (isTRUE(.char_value)) {
    .value <- unlist(.value)
    checkmate::assert_character(.value, null.ok = TRUE)
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
    .value <- na_to_null(.value) # both NA and NULL will remove the field from the object
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

  .mod <- save_model_yaml(.mod)

  return(.mod)
}


#####################
# FRIENDLIER HELPERS
#####################

#' @name modify_tags
#' @title Modify tags on a model object
#'
#' @description Add, replace, or remove tags on a model object and corresponding YAML.
#' Tags can be modified at any time (i.e. before _or_ after a model is submitted).
#'
#' @details
#' The `tags` fields on a `bbi_{.model_type}_model` object contains a character
#' vector of brief descriptors about the model. Often this is used to keep track
#' of the model structure, such as the covariates or error stucture that was
#' used.
#'
#' One of the more useful things to do with tags is to use them for filtering a
#' `bbi_run_log_df` (the tibble output from [run_log()]); for example to look at
#' all models that used a particular random effect or covariate. To facilitate
#' this, it is recommended to keep individual tags consistent and short, and/or
#' to use a "glossary" of acceptable tags, stored elsewhere in your project
#' (i.e. in a `.yaml` or `.csv` or a named list that you can source). In other
#' words, if you tag one model with `"CLAGE"` and another with
#' `"ClearanceAgeCov"`, etc. then it will be very difficult to make any use of
#' those tags later, for instance when you want to filter to models with that
#' covariate.
#'
#' Tags can also be collapsed using [collapse_to_string()] to create a compact description of the
#' model stucture. See the ["Getting Started" vignette](https://metrumresearchgroup.github.io/bbr/articles/getting-started.html#viewing-tags-example)
#' for an example of this.
#'
#' @return The modified `bbi_{.model_type}_model` object
#'
#' @seealso [run_log()] [collapse_to_string()] [modify_notes()] [modify_based_on()] [modify_description()] [modify_bbi_args()]
NULL

#' @describeIn modify_tags Add tags to a model object and corresponding YAML.
#' @inheritParams modify_model_field
#' @param .tags Character vector of tags to add or remove
#' @export
add_tags <- function(.mod, .tags) {
  modify_model_field(
    .mod = .mod,
    .field = YAML_TAGS,
    .value = .tags,
    .append = TRUE,
    .char_value = TRUE
  )
}

#' @describeIn modify_tags Replaces a specific `.old_tag` with `.new_tag` on a model object and corresponding YAML.
#' Warns and does nothing if `.old_tag` is not present.
#' @param .old_tag Character scalar of tag to be replaced
#' @param .new_tag Character scalar of tag that will be added
#' @export
replace_tag <- function(.mod, .old_tag, .new_tag) {
  replace_model_field(.mod, YAML_TAGS, .old_tag, .new_tag)
}

#' @describeIn modify_tags Replaces all tags on a model object and corresponding YAML with new tags.
#' @export
replace_all_tags <- function(.mod, .tags) {
  modify_model_field(
    .mod = .mod,
    .field = YAML_TAGS,
    .value = .tags,
    .append = FALSE,
    .char_value = TRUE
  )
}

#' @describeIn modify_tags Removes tags from a model object and corresponding YAML.
#' @export
remove_tags <- function(.mod, .tags) {
  modify_model_field(
    .mod = .mod,
    .field = YAML_TAGS,
    .value = .tags,
    .append = FALSE,
    .remove = TRUE,
    .char_value = TRUE,
  )
}


#' @name modify_notes
#' @title Modify notes on a model object
#'
#' @description Add, replace, or remove notes on a model object and corresponding YAML.
#' Notes can be modified at any time (i.e. before _or_ after a model is submitted).
#'
#' @details
#' The `notes` field on a `bbi_{.model_type}_model` object contains a character
#' vector of brief notes about the model or modeling process.
#'
#' These are often useful when looking at a `bbi_run_log_df` (the tibble output
#' from [run_log()]); for example to remind yourself of decisions you made along
#' the way, or of a particular thing you noticed when looking at model
#' diagnostics.
#'
#' Like tags, notes can also be collapsed using [collapse_to_string()]
#' See the ["Getting Started" vignette](https://metrumresearchgroup.github.io/bbr/articles/getting-started.html#viewing-tags-example)
#' for an example of this.
#'
#' @return The modified `bbi_{.model_type}_model` object
#'
#' @seealso [run_log()] [collapse_to_string()] [modify_tags()] [modify_based_on()] [modify_description()] [modify_bbi_args()]
NULL

#' @describeIn modify_notes Add notes to a model object and corresponding YAML.
#' @inheritParams modify_model_field
#' @param .notes Character vector of notes to add or remove
#' @export
add_notes <- function(.mod, .notes) {
  modify_model_field(
    .mod = .mod,
    .field = YAML_NOTES,
    .value = .notes,
    .append = TRUE,
    .char_value = TRUE
  )
}

#' @describeIn modify_notes Replaces a specific `.old_note` with `.new_note` on a model object and corresponding YAML.
#' Warns and does nothing if `.old_note` is not present.
#' @param .old_note Character scalar of note to be replaced
#' @param .new_note Character scalar of note that will be added
#' @export
replace_note <- function(.mod, .old_note, .new_note) {
  replace_model_field(.mod, YAML_NOTES, .old_note, .new_note)
}

#' @describeIn modify_notes Replaces all notes on a model object and corresponding YAML with new notes.
#' @export
replace_all_notes <- function(.mod, .notes) {
  modify_model_field(
    .mod = .mod,
    .field = YAML_NOTES,
    .value = .notes,
    .append = FALSE,
    .char_value = TRUE
  )
}

#' @describeIn modify_notes Removes notes from a model object and corresponding YAML.
#' @export
remove_notes <- function(.mod, .notes) {
  modify_model_field(
    .mod = .mod,
    .field = YAML_NOTES,
    .value = .notes,
    .append = FALSE,
    .remove = TRUE,
    .char_value = TRUE
  )
}


#' @name modify_based_on
#' @title Modify based_on field in a model object
#'
#' @description Add, replace, or remove model identifiers from the `based_on`
#'   field of a model object and corresponding YAML. The `based_on` field can be
#'   modified at any time (i.e. before _or_ after a model is submitted).
#'
#' @details
#' The `based_on` field on a `bbi_{.model_type}_model` object contains a
#' character vector identifying models which preceded `.mod` in the model
#' development process. This field is automatically populated with `.parent_mod`
#' when a model is created with [copy_model_from()].
#'
#' The model identifiers in the `based_on` field are paths another model file
#' (without file extension) _relative_ to the location of `.mod`.
#'
#' There is a ["Using the based_on field" vignette](https://metrumresearchgroup.github.io/bbr/articles/using-based-on.html)
#' which demonstrates the motivation and usage of this field.
#'
#' @return The modified `bbi_{.model_type}_model` object
#'
#' @seealso [copy_model_from()] [modify_tags()] [modify_notes()] [modify_description()] [modify_bbi_args()]
NULL

#' @describeIn modify_based_on Append new `based_on` identifiers to a model object and corresponding YAML.
#' @inheritParams modify_model_field
#' @param .based_on Character vector of relative paths to add or remove
#' @export
add_based_on <- function(.mod, .based_on) {
  modify_model_field(
    .mod = .mod,
    .field = YAML_BASED_ON,
    .value = safe_based_on(get_model_working_directory(.mod), .based_on),
    .append = TRUE
  )
}

#' @describeIn modify_based_on Replaces entire `based_on` field in a model object and corresponding YAML with new values.
#' @export
replace_all_based_on <- function(.mod, .based_on) {
  modify_model_field(
    .mod = .mod,
    .field = YAML_BASED_ON,
    .value = safe_based_on(get_model_working_directory(.mod), .based_on),
    .append = FALSE
  )
}

#' @describeIn modify_based_on Remove specified `based_on` identifier(s) from a model object and corresponding YAML.
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


#' @name modify_description
#' @title Modify description on a model object
#'
#' @description Add or replace a description on a model object and corresponding YAML.
#' The description can be modified at any time (i.e. before _or_ after a model is submitted).
#'
#' @details
#' The `description` field on a `bbi_{.model_type}_model` object contains a character
#' scalar with a brief description of the model. Note that it is _not_ a required field
#' and some users prefer to leave it blank and instead use the `tags` and/or `notes` fields
#' to describe their models instead.
#'
#' Another pattern is to fill the `description` field _only_ on notable models,
#' for instance with `"Base Model"` or `"Final Model"`. This can be useful when
#' looking at a `bbi_run_log_df` (the tibble output from [run_log()]); for
#' example to look at only the most important models by calling
#' `run_log() %>% filter(!is.na(description))`.
#'
#' @return The modified `bbi_{.model_type}_model` object
#'
#' @seealso [run_log()] [modify_tags()] [modify_notes()] [modify_based_on()] [modify_bbi_args()]
NULL

#' @describeIn modify_description Fills the description field in a model object and corresponding YAML, if it is currently empty.
#' @importFrom checkmate assert_scalar
#' @inheritParams modify_model_field
#' @param .description Character scalar to put in `description` field
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

#' @describeIn modify_description Replaces description field in a model object and corresponding YAML with new description.
#' @importFrom checkmate assert_scalar
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


#' @name modify_bbi_args
#' @title Modify bbi_args on a model object
#'
#' @description Add or replace the `bbi_args` field on a model object and corresponding YAML.
#'
#' @details
#' The `bbi_args` field on a `bbi_{.model_type}_model` object contains a named
#' list of arguments that will be passed through to `bbi` to modify how a model
#' is processed (for instance, when it is submitted to be run with
#' [submit_model()]). In many cases, it is preferable to store these arguments
#' with the model object, as opposed to passing them to
#' `submit_model(.bbi_args)` because then they will be applied every time that
#' model is run, for example if you need to re-run the model later.
#'
#' The options for what can be included in this list (and a brief description of
#' what each does) can be seen by calling [print_bbi_args()].
#'
#' Note that there is no `replace_bbi_arg` (singular) to replace individual arguments.
#' This is because you can use `add_bbi_args()` and any arguments passed in will automatically
#' overwrite any previous value stored for that argument.
#'
#' @return The modified `bbi_{.model_type}_model` object
#'
#' @seealso [submit_model()] [model_summary()] [print_bbi_args()]
#'   [modify_tags()] [modify_notes()] [modify_based_on()] [modify_description()]
NULL

#' @describeIn modify_bbi_args Modifies model object and corresponding YAML
#'   by adding named list passed to `.bbi_args`, overwriting any args that are
#'   already present with the new values.
#' @inheritParams modify_model_field
#' @param .bbi_args named list of arguments to add to the model. See
#'   [print_bbi_args()] for valid options.
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

#' @describeIn modify_bbi_args Modifies model object and corresponding YAML
#'   by replacing all `bbi_args` with named list passed to `.bbi_args`.
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


#' @name modify_decisions
#' @title Deprecated: Modify decisions on a model object
#'
#' @description The `decisions` field has been deprecated as of `bbr 0.10.0` and
#' replaced by the `notes` field, to reflect the fact that users
#' will want to use this field throughout the modeling process, not only at the end
#' once some "decisions" have been reached. As of `bbr 1.0.0`, `add_decisions()` and
#' `replace_decisions()` now error telling the user that they will be
#' deprecated in the future and encouraging use of their `*_notes` counterparts.
#' The functions will be removed entirely two releases after that.
#'
#' @return The modified `bbi_{.model_type}_model` object
#'
#' @seealso [modify_notes()]
NULL

#' @describeIn modify_decisions **Deprecated**  Append new decisions to the one(s) in a model object and corresponding YAML.
#' @inheritParams modify_model_field
#' @param .decisions Character vector to add to `decisions` field
#' @export
add_decisions <- function(.mod, .decisions) {
  stop("The `decisions` field has been replaced by `notes` as of bbr 0.10.0 and will be removed in a future release. Please use `add_notes()` going forward.", call. = FALSE)
  modify_model_field(
    .mod = .mod,
    .field = YAML_DECISIONS,
    .value = .decisions,
    .append = TRUE
  )
}

#' @describeIn modify_decisions **Deprecated** Replaces `decisions` field in a model object and corresponding YAML with new values.
#' @export
replace_decisions <- function(.mod, .decisions) {
  stop("The `decisions` field has been replaced by `notes` as of bbr 0.10.0 and will be removed in a future release. Please use `replace_all_notes()` going forward.", call. = FALSE)
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


