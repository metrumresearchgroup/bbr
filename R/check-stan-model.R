#' Checks a `bbi_stan_model` for necessary files
#'
#' Checks a [bbi_stan_model] object to make sure it can find all of the files
#' necesssary to submit the model. (See `?`[bbi_stan_model] for details about
#' these files.)
#'
#' @details
#' Will look for the following:
#' * `build_path_from_model(.mod, "-standata.R")`
#' * `build_path_from_model(.mod, "-init.R")`
#' * `build_path_from_model(.mod, ".stan")`
#'
#' @param .mod A [bbi_stan_model] object
#' @param .error If `FALSE`, the default, will warn if any necessary files are
#'   missing. If `TRUE` will error instead.
#' @importFrom stringr str_replace
#' @importFrom purrr map_lgl
#' @export
check_stan_model <- function(.mod, .error = FALSE) {
  # check if output dir exists and if not create an empty one
  model_dir <- dirname(get_output_dir(.mod, .check_exists = FALSE))
  if (!fs::dir_exists(model_dir)) fs::dir_create(model_dir)

  # check for files in output dir
  files_to_check <- build_path_from_model(.mod, STAN_MODEL_REQ_FILES)
  files_present <- fs::file_exists(files_to_check)
  files_missing <- !files_present

  problems <- NULL

  if (any(files_missing)) {
    problems <- paste(
      problems,
      glue("{MISSING_STAN_FILES_ERR_MSG} from {get_model_id(.mod)}:"),
      paste(paste0(" * ", names(which(files_missing))), collapse = "\n"),
      paste("See `?add_file_to_model_dir` for helper functions to add them.\n"),
      sep = "\n"
    )
  }

  # checking if any of the files found are only scaffolds
  if (any(files_present)) {
    candidates <- names(which(files_present))

    scaffold_bool <- map_lgl(candidates, function(.f) {
      tools::md5sum(.f) %in% STAN_SCAFFOLD_MD5_VEC
    })

    if (any(scaffold_bool)) {
      problems <- paste(
        problems,
        STAN_SCAFFOLD_ERR_MSG,
        paste(paste0(" * ", candidates[scaffold_bool]), collapse = "\n"),
        "Please add necessary code to the scaffolded files.\n",
        sep = "\n"
      )
    }
  }

  if (!is.null(problems)) {
    if (isTRUE(.error)) {
      stop(problems, call. = FALSE)
    } else {
      message(problems)
    }
  }

  return(invisible(is.null(problems)))
}
