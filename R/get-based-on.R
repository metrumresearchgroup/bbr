
#' Get based_on from bbi object
#'
#' Returns character vector of the absolute paths all models stored in the `based_on` field of a `bbi_...` object
#' NOTE: All paths saved in the object or accompanying YAML will be relative **to the location of that YAML**
#' When the object is loaded into memory, the absolute path to the YAML is stored in the object.
#' These functions simply stitch together that path with the relative paths from the `based_on` field.
#' As long as the YAML has not moved since it was loaded, this will work.
#' @param .bbi_object The model object (or path, etc.) to query
#' @param .check_exists Logical scaler for whether it will check if the file exists and error if it does not. FALSE by default.
#' @export
#' @rdname get_based_on
get_based_on <- function(.bbi_object, .check_exists = FALSE) {
  UseMethod("get_based_on")
}


#' The default method attempts to extract the path from any object passed to it
#' @importFrom fs path_norm
#' @param .bbi_object The object to attempt to query. Could be a partially built bbi_{.model_type}_object or some other custom object containing model data.
#' @rdname get_based_on
#' @export
get_based_on.default <- function(.bbi_object, .check_exists = FALSE) {
  tryCatch(
    {
      # do some QA on the required WORKING_DIR field
      if (is.null(.bbi_object[[WORKING_DIR]])) {
        stop(glue(".bbi_object must contain key for `{WORKING_DIR}` but has only the following keys: {paste(names(.bbi_object), collapse = ', ')}"), call. = FALSE)
      }

      # if no based_on field, return NULL
      if (is.null(.bbi_object[[YAML_BASED_ON]])) {
        return(invisible())
      }

      # optionally check if they exist
      if (isTRUE(.check_exists)) {
        invisible(check_based_on(.bbi_object[[WORKING_DIR]], .bbi_object[[YAML_BASED_ON]]))
      }

      # extract the requested paths
      return(as.character(fs::path_norm(file.path(.bbi_object[[WORKING_DIR]], .bbi_object[[YAML_BASED_ON]]))))

    },
    error = function(e) {
      stop(glue("Cannot extract `{YAML_BASED_ON}` from object of class `{paste(class(.bbi_object), collapse = ', ')}` :\n{paste(e, collapse = '\n')}"), call. = FALSE)
    }
  )
}


#' @rdname get_based_on
#' @param .bbi_object Character scaler of a path to a model that can be loaded with `read_model(.bbi_object)`
#' @export
get_based_on.character <- function(.bbi_object, .check_exists = FALSE) {

  .bbi_object <- tryCatch(
    {
      read_model(.bbi_object)
    },
    error = function(e) {
      stop(glue("Cannot load model object from path `{.bbi_object}` :\n{paste(e, collapse = '\n')}"))
    }
  )

  return(get_based_on(.bbi_object, .check_exists = .check_exists))
}


#' @rdname get_based_on
#' @param .bbi_object Tibble of class `bbi_run_log_df`
#' @importFrom purrr map
#' @export
get_based_on.bbi_run_log_df <- function(.bbi_object, .check_exists = FALSE) {

  .out_paths <- map(.bbi_object[[ABS_MOD_PATH]], function(.path) {
    get_based_on(.path, .check_exists = .check_exists)
  })

  return(.out_paths)
}


#' Get model ancestry
#'
#' Extract paths to all models that this model is based on,
#' and all models that those models are based on, recursively.
#' Returns a sorted unique character vector.
#' @importFrom purrr map
#' @param .mod `bbi_{.model_type}_model` object
#' @rdname get_based_on
#' @export
get_model_ancestry <- function(.mod) {
  .checked <- c()
  .to_check <- get_based_on(.mod)
  .results <- .to_check
  while(length(.to_check) > 0) {
    # record this round of models as being checked
    .checked <- c(.checked, .to_check)

    # get based_on for this round of models
    .this_res <- map(.to_check, ~ get_based_on(.x))
    .this_res <- unique(unlist(.this_res))

    # add to results
    .results <- unique(c(.results, .this_res))

    # see if there are any we haven't checked yet
    .to_check <- .this_res[!(.this_res %in% .checked)]
  }

  return(sort(.results))
}
