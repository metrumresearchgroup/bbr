
#' @title Get based_on from bbi object
#'
#' @description Returns character vector (or list of character vectors) of the absolute paths to all models stored in the `based_on` field of a `bbi_...` object.
#'
#' @details
#' `get_model_ancestry()` extracts paths to all models that this model is based on (iterating over `get_based_on()` function),
#' and all models that those models are based on, recursively.
#' It returns a sorted unique character vector for the `default` and `character` method, and a list of such vectors for the `bbi_run_log_df` method.
#' Some notes on `get_model_ancestry()`:
#' * `.check_exists = TRUE` is set for this iterative search because each model found will subsequently be loaded.
#' * `get_model_ancestry()` has all the same S3 methods as `get_based_on()`
#'
#' For both `get_based_on()` and `get_model_ancestry()`, all paths saved in the object or accompanying YAML will be relative **to the location of that YAML**
#' When the object is loaded into memory, the absolute path to the YAML is stored in the object.
#' These functions simply stitch together that path with the relative paths from the `based_on` field.
#' As long as the YAML has not moved since it was read into memory, these paths will be both absolute and correct.
#'
#' @param .bbi_object The model object to query. Could be
#' a `bbi_{.model_type}_model` object,
#' a  file path to a model,
#' a tibble of class `bbi_run_log_df`,
#' or some other custom object containing model data.
#' @param .check_exists If `FALSE`, the default, function will return all paths in `based_on` regardless of whether they point to an existing file.
#' If `TRUE`, function will check if a file exists at each path in `based_on` and error if one does not.
#' @export
get_based_on <- function(.bbi_object, .check_exists = FALSE) {
  UseMethod("get_based_on")
}


#' @describeIn get_based_on The default method attempts to extract the path from any object passed to it,
#' but is designed for a list of class `bbi_{.model_type}_model` or something similar.
#' @importFrom fs path_norm
#' @export
get_based_on.default <- function(.bbi_object, .check_exists = FALSE) {

  # do some QA on the required WORKING_DIR field
  if (is.null(.bbi_object[[WORKING_DIR]])) {
    stop_get_fail_msg(
      .bbi_object,
      YAML_BASED_ON,
      glue(".bbi_object must contain key for `{WORKING_DIR}` but has only the following keys: {paste(names(.bbi_object), collapse = ', ')}")
    )
  }

  # if no based_on field, return NULL
  if (is.null(.bbi_object[[YAML_BASED_ON]])) {
    return(NULL)
  }

  # optionally check if they exist
  if (isTRUE(.check_exists)) {
    tryCatch({
      invisible(safe_based_on(.bbi_object[[WORKING_DIR]], .bbi_object[[YAML_BASED_ON]]))
    }, error = function(e) {
      stop_get_fail_msg(.bbi_object, YAML_BASED_ON, e$message)
    })
  }

  # extract the requested paths
  return(as.character(fs::path_norm(file.path(.bbi_object[[WORKING_DIR]], .bbi_object[[YAML_BASED_ON]]))))
}

#' @describeIn get_based_on Takes a character scalar of a path to a model that can be loaded with `read_model(.bbi_object)`.
#' @export
get_based_on.character <- function(.bbi_object, .check_exists = FALSE) {

  if (length(.bbi_object) > 1) {
    stop_get_scalar_msg(length(.bbi_object))
  }

  .bbi_object <- tryCatch(
    {
      read_model(.bbi_object)
    },
    error = function(e) {
      stop(glue("Cannot load model object from path `{.bbi_object}` :\n{paste(e, collapse = '\n')}"))
    }
  )

  .out_paths <- get_based_on(.bbi_object, .check_exists = .check_exists)

  return(.out_paths)
}


#' @describeIn get_based_on Takes a tibble of class `bbi_run_log_df` and returns a list containing one character vector of paths for each row of the tibble.
#' @importFrom purrr map
#' @export
get_based_on.bbi_run_log_df <- function(.bbi_object, .check_exists = FALSE) {

  .out_paths <- map(.bbi_object[[ABS_MOD_PATH]], function(.path) {
    get_based_on(.path, .check_exists = .check_exists)
  })

  return(.out_paths)
}


#' @rdname get_based_on
#' @export
get_model_ancestry <- function(.bbi_object) {
  UseMethod("get_model_ancestry")
}

#' @rdname get_based_on
#' @export
get_model_ancestry.default <- function(.bbi_object) {
  .checked <- c()
  .to_check <- get_based_on(.bbi_object)
  .results <- .to_check

  while(length(.to_check) > 0) {
    # record this round of models as being checked
    .checked <- c(.checked, .to_check)

    # get based_on for this round of models
    .this_res <- map(.to_check, get_based_on)
    .this_res <- unique(unlist(.this_res))

    # add to results
    .results <- unique(c(.results, .this_res))

    # see if there are any we haven't checked yet
    .to_check <- .this_res[!(.this_res %in% .checked)]
  }

  return(sort(.results))
}

#' @rdname get_based_on
#' @export
get_model_ancestry.character <- function(.bbi_object) {

  if (length(.bbi_object) > 1) {
    stop_get_scalar_msg(length(.bbi_object))
  }

  .bbi_object <- tryCatch(
    {
      read_model(.bbi_object)
    },
    error = function(e) {
      stop(glue("Cannot load model object from path `{.bbi_object}` :\n{paste(e, collapse = '\n')}"))
    }
  )

  return(get_model_ancestry(.bbi_object))
}

#' @rdname get_based_on
#' @importFrom purrr map
#' @export
get_model_ancestry.bbi_run_log_df <- function(.bbi_object) {

  # create key-value for a get_based_on of all models in df
  .all_mods <- .bbi_object[[ABS_MOD_PATH]]
  .based_on_list <- map(.all_mods, function(.path) {
    get_based_on(.path, .check_exists = FALSE)
  })

  names(.based_on_list) <- .all_mods

  # iterate over models and recursively look up ancestors in .based_on_list
  .out_paths <- map(.all_mods, function(.m) {
    .checked <- c()
    .to_check <- .based_on_list[[.m]]
    .results <- .to_check

    while(length(.to_check) > 0) {
      # record this round of models as being checked
      .checked <- c(.checked, .to_check)

      # get based_on for this round of models
      .this_res <- map(.to_check, function(.p) {
        if(!(.p %in% .all_mods)) {
          stop(glue("Found {.p} in get_model_ancestry() tree, but could not find a YAML file for that model."), call. = FALSE)
        }
        return(.based_on_list[[.p]])
      })
      .this_res <- unique(unlist(.this_res))

      # add to results
      .results <- unique(c(.results, .this_res))

      # see if there are any we haven't checked yet
      .to_check <- .this_res[!(.this_res %in% .checked)]
    }

    return(sort(.results))
  })

  return(.out_paths)
}
