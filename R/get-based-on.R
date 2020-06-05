
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


#' @rdname get_based_on
#' @param .bbi_object Character scaler of a path to a model that can be loaded with `read_model(.bbi_object)`
#' @export
get_based_on.character <- function(.bbi_object, .check_exists = FALSE) {

  if (length(.bbi_object) > 1) {
    stop_get_scaler_msg(length(.bbi_object))
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
#' Extract paths to all models that this model is based on (iterating over `get_based_on()` function),
#' and all models that those models are based on, recursively.
#' Returns a sorted unique character vector.
#' @param .bbi_object The model object (or path, etc.) to query
#' @rdname get_based_on
#' @export
get_model_ancestry <- function(.bbi_object) {
  UseMethod("get_model_ancestry")
}

#' The default method attempts to extract the path from any object passed to it
#' @importFrom purrr map
#' @importFrom stringr str_detect
#' @param .bbi_object The object to attempt to query. Could be a partially built bbi_{.model_type}_object or some other custom object containing model data.
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
    .this_res <- map(.to_check, function(.p) {
      .res <- tryCatch(
        {
          get_based_on(.p)
        },
        error = function(e) {
          if (str_detect(e$message, FIND_YAML_ERR_MSG)) {
            stop(glue("Found {.p} in get_model_ancestry() tree, but could not find a YAML file for that model."), call. = FALSE)
          }
          stop(e$message)
        }
      )

      return(.res)
    })
    .this_res <- unique(unlist(.this_res))

    # add to results
    .results <- unique(c(.results, .this_res))

    # see if there are any we haven't checked yet
    .to_check <- .this_res[!(.this_res %in% .checked)]
  }

  return(sort(.results))
}


#' @rdname get_based_on
#' @param .bbi_object Character scaler of a path to a model that can be loaded with `read_model(.bbi_object)`
#' @export
get_model_ancestry.character <- function(.bbi_object) {

  if (length(.bbi_object) > 1) {
    stop_get_scaler_msg(length(.bbi_object))
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
#' @param .bbi_object Tibble of class `bbi_run_log_df`
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
