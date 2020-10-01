#####################
# Modifying models
#####################

#' Modify field in model object
#'
#' Helper functions for updating fields in a `bbi_{.model_type}_model` object.
#' Note that calling `modify_model_field()` directly is not recommended for most users
#' because it requires knowing about the internal structure of the model object.
#' Instead we recommend using the friendlier helpers listed below (`add_...` or `replace_...`) when possible.
#'
#' @details
#' All functions in this family also check the object against the corresponding YAML with `check_yaml_in_sync()` before modifying it,
#' and errors if they are out of sync.
#' After the object has been modified they will write the modified object back to the YAML and update the model object in
#' memory with an md5 digest of the newly written YAML.
#' @importFrom digest digest
#' @param .mod The `bbi_{.model_type}_model` object to modify
#' @param .field Character scalar of the name of the component to modify
#' @param .value Whatever is to be added to `.mod[[.field]]`, typically a character vector
#' @param .append If `TRUE`, the default, concatenate new values with currently present values. If `FALSE`, new values will overwrite old values.
#' @param .unique If `TRUE`, the default, de-duplicate `.mod[[.field]]` after adding new values. If `FALSE` duplicate values will be kept.
#' @export
modify_model_field <- function(.mod, .field, .value, .append = TRUE, .unique = TRUE) {

  # update .mod with any changes from yaml on disk
  check_yaml_in_sync(.mod)

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
  .mod <- save_model_yaml(.mod)

  return(.mod)
}

#' @describeIn modify_model_field Add tags to a model object and corresponding YAML
#' @param .tags Character vector to add to `tags` field
#' @export
add_tags <- function(.mod, .tags) {
  .mod <- modify_model_field(.mod = .mod,
                             .field = YAML_TAGS,
                             .value = .tags,
                             .append = TRUE)
  return(.mod)
}

#' @describeIn modify_model_field Replaces tags on a model object and corresponding YAML with new tags
#' @export
replace_tags <- function(.mod, .tags) {
  .mod <- modify_model_field(.mod = .mod,
                             .field = YAML_TAGS,
                             .value = .tags,
                             .append = FALSE)
  return(.mod)
}

#' @describeIn modify_model_field Append new decisions to the one(s) in a model object and corresponding YAML
#' @param .decisions Character vector to add to `decisions` field
#' @export
add_decisions <- function(.mod, .decisions) {
  .mod <- modify_model_field(.mod = .mod,
                             .field = YAML_DECISIONS,
                             .value = .decisions,
                             .append = TRUE)
  return(.mod)
}

#' @describeIn modify_model_field Replaces `decisions` field in a model object and corresponding YAML with new values
#' @param .decisions Character vector to use as replacement
#' @export
replace_decisions <- function(.mod, .decisions) {
  .mod <- modify_model_field(.mod = .mod,
                             .field = YAML_DECISIONS,
                             .value = .decisions,
                             .append = FALSE)
  return(.mod)
}


#' @describeIn modify_model_field Append new `based_on` tag to the one in a model object and corresponding YAML
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

#' @describeIn modify_model_field Replaces `based_on` field in a model object and corresponding YAML with new values
#' @export
replace_based_on <- function(.mod, .based_on) {
  modify_model_field(
    .mod = .mod,
    .field = YAML_BASED_ON,
    .value = safe_based_on(get_model_working_directory(.mod), .based_on),
    .append = FALSE
  )
}

#' @describeIn modify_model_field Replaces description field in a model object and corresponding YAML with new description
#' @param .description Character scalar to use as replacement for the `description` field
#' @export
replace_description <- function(.mod, .description) {
  .mod <- modify_model_field(.mod = .mod,
                             .field = YAML_DESCRIPTION,
                             .value = .description,
                             .append = FALSE)

  return(.mod)
}


#' @describeIn modify_model_field Modifies model object and corresponding YAML by adding new `bbi_args`,
#' overwriting any args that are already present with the new values.
#' Use [print_bbi_args()] to see a list of valid babylon arguments.
#' @importFrom digest digest
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

#' @describeIn modify_model_field Modifies model object and corresponding YAML by replacing `bbi_args` with new list passed to `.bbi_args`.
#' Use [print_bbi_args()] to see a list of valid babylon arguments.
#' @importFrom digest digest
#' @export
replace_bbi_args <- function(.mod, .bbi_args) {

  # update .mod with any changes from yaml on disk
  check_yaml_in_sync(.mod)

  # combine the two lists, with .bbi_args overwriting any keys that are shared
  .mod[[YAML_BBI_ARGS]] <- .bbi_args

  # overwrite the yaml on disk with modified model
  .mod <- save_model_yaml(.mod)

  return(.mod)
}


###################
# helper functions
###################

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
  .loaded_mod <- read_model(.yaml_path)

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


