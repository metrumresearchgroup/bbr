#####################
# Modifying models
#####################

#' Modify field in model object
#'
#' Implementation function for updating fields in a `bbi_{.model_type}_model` object
#' Also checks the object against the corresponding YAML before modifying (and errors if they are out of sync) and then writes the modified object back to the YAML.
#' @param .mod The `bbi_{.model_type}_model` object to modify
#' @param .field Character scaler of the name of the component to modify
#' @param .value Whatever is to be added to `.mod[[.field]]`, typically a character scaler or vector
#' @param .append Boolean for whether to concatenate new values with currently present values. TRUE by default. If FALSE, new values will overwrite old values.
#' @param .unique Boolean for whether to de-duplicate `.mod[[.field]]` after adding new values. TRUE by default.
#' @rdname modify_model_field
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
  save_model_yaml(.mod)

  # refresh md5 hash in model object
  .mod[[YAML_YAML_MD5]] <- digest(file = get_yaml_path(.mod), algo = "md5")

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
#' @param .based_on Character scaler or vector of relative paths to add to `based_on` field
#' @export
#' @rdname modify_model_field
add_based_on <- function(.mod, .based_on) {
  .mod <- modify_model_field(.mod = .mod,
                             .field = YAML_BASED_ON,
                             .value = safe_based_on(.mod[[WORKING_DIR]], .based_on),
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
                             .value = safe_based_on(.mod[[WORKING_DIR]], .based_on),
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


#' Add new babylon args to model
#'
#' Modifies model object and corresponding YAML by adding new .bbi_args,
#' overwriting any args that are already present with the new values.
#' Use `print_nonmem_args()` to see a list list of valid babylon arguments.
#' @param .mod A `bbi_{.model_type}_model` object
#' @param .bbi_args named list of arguments to add to the model
#' @export
add_bbi_args <- function(.mod, .bbi_args) {

  # update .mod with any changes from yaml on disk
  check_yaml_in_sync(.mod)

  # combine the two lists, with .bbi_args overwriting any keys that are shared
  .mod[[YAML_BBI_ARGS]] <- parse_args_list(.bbi_args, .mod[[YAML_BBI_ARGS]])

  # overwrite the yaml on disk with modified model
  save_model_yaml(.mod)

  # refresh md5 hash in model object
  .mod[[YAML_YAML_MD5]] <- digest(file = get_yaml_path(.mod), algo = "md5")

  return(.mod)
}

#' Replaces babylon args to model
#'
#' Modifies model object and corresponding YAML by replacing .bbi_args with new .bbi_args
#' Use `print_nonmem_args()` to see a list list of valid babylon arguments.
#' @param .mod A `bbi_{.model_type}_model` object
#' @param .bbi_args named list of arguments to add to the model
#' @export
replace_bbi_args <- function(.mod, .bbi_args) {

  # update .mod with any changes from yaml on disk
  check_yaml_in_sync(.mod)

  # combine the two lists, with .bbi_args overwriting any keys that are shared
  .mod[[YAML_BBI_ARGS]] <- .bbi_args

  # overwrite the yaml on disk with modified model
  save_model_yaml(.mod)

  # refresh md5 hash in model object
  .mod[[YAML_YAML_MD5]] <- digest(file = get_yaml_path(.mod), algo = "md5")

  return(.mod)
}


###################
# helper functions
###################

#' Checks for yaml files relative to a starting location
#' @importFrom fs file_exists is_absolute_path path_rel
#' @importFrom purrr map_lgl
#' @param .start The directory of the model (i.e. the YAML file) that the `based_on` will be added to.
#' @param .based_on Character vector or scaler of paths (with or without extension) to the models that will be added to `based_on`. Paths should be relative to `.start` argument.
safe_based_on <- function(.start, .based_on) {
  # make all input paths relative to .start
  .based_on <- map_chr(.based_on, function(.p) {
    if (fs::is_absolute_path(.p)) {
      .p <- fs::path_rel(.p, .start)
    }
    return(.p)
  })

  .paths <- file.path(.start, .based_on)

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
      glue("Parsed {length(.paths_bool)} models as `based_on` but cannot find .yaml or .yml files for {length(.paths_bool) - sum(.paths_bool)} of them: "),
      paste(names(which(!.paths_bool)), collapse = ', ')
    ))
  }

  return(tools::file_path_sans_ext(.based_on))
}


################
# synching YAML
################

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


#' Check model against YAML
#'
#' Checks that model YAML file is the same as when it was last read into the model object.
#' Errors if the md5 hashes are not identical.
#' @importFrom digest digest
#' @param .mod `bbi_{.model_type}_model` object
#' @rdname reconcile_yaml
#' @export
check_yaml_in_sync <- function(.mod) {
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


