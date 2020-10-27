#######################
# Generating run logs
#######################

#' Create tibble summarizing all model runs
#'
#' Parses all model YAML files and outputs into a tibble that serves as a run log for the project.
#' Future releases will incorporate more diagnostics and parameter estimates, etc. from the runs into this log.
#' Users can also use [add_config()] or [add_summary()] to append additional output about the model run.
#' @seealso [config_log()], [summary_log()]
#'
#' @param .base_dir Base directory to look in for models.
#' @param .recurse If `TRUE`, the default, search recursively in all subdirectories. Passed through to `fs::dir_ls()` -- If a positive number, the number of levels to recurse.
#'
#' @importFrom purrr map_df
#' @importFrom tibble tibble
#' @return A tibble of class `bbi_run_log_df` with information on each model, or an empty tibble if no models are found.
#' @export
run_log <- function(.base_dir, .recurse = TRUE) {
  checkmate::assert_string(.base_dir)

  mod_list <- find_models(.base_dir, .recurse)
  if(length(mod_list) == 0) {
    return(tibble())
  }

  df <- mod_list %>% map_df(run_log_entry)

  df <- create_run_log_object(df)

  return(df)
}



#' Collapse list column to a character column
#'
#' Collapses a list column in a tibble into a column of character scalars.
#' @details
#' Any non-list columns passed to `...` will be ignored and will trigger a warning notifying the user that only list columns can be collapsed.
#'
#' Any list columns passed to `...` which do _not_ contain either character, numeric, or logical vectors (i.e. lists of lists) will be silently ignored.
#'
#' @return
#' Returns the same tibble as `.data`, but any list columns named in `...` will be collapsed to a character column, with one scalar value (or `NA`) for each row.
#' See "Details" section for caveats.
#'
#' @examples
#' df <- tibble::tibble(
#'   row_num   = c(1, 2, 3),
#'   char_list = list(c("foo", "bar"), "baz", c("naw", "dawg")),
#'   num_list  = list(c(23, 34, 45), c(-1, -2, -3), NULL)
#' )
#'
#' collapse_to_string(df, char_list, num_list)
#'
#' @param .data Input tibble to modify
#' @param ... <[`tidy-select`][dplyr::dplyr_tidy_select]> One or more unquoted
#'   expressions separated by commas. Variable names can be used as if they
#'   were positions in the data frame, so expressions like `x:y` can
#'   be used to select a range of variables.
#' @param .sep Character scalar to use a separator when collapsing vectors. Defaults to `", "`.
#' @importFrom dplyr mutate mutate_at group_by ungroup select row_number
#' @importFrom tidyselect eval_select
#' @importFrom rlang expr
#' @importFrom purrr map_lgl
#' @importFrom checkmate assert_scalar
#' @export
collapse_to_string <- function(.data, ..., .sep = ", ") {
  checkmate::assert_scalar(.sep)

  loc <- tidyselect::eval_select(rlang::expr(c(...)), .data)

  # do we need this? seems like a reasonable safety catch but it's internal... danger...
  loc <- dplyr:::ensure_group_vars(loc, .data, notify = TRUE)

  # warn if passed columns that are not lists
  valid_cols <- map_lgl(loc, ~ inherits(.data[[.x]], "list"))
  if (any(!valid_cols)) {
    bad_cols <- names(valid_cols)[!valid_cols]
    warning(glue("collapse_to_string() only works on list columns. The following columns are not lists and will be ignored: {paste(bad_cols, collapse = ', ')}"))
  }

  # collapse together any lists of vectors
  .data %>%
    mutate(.collapse_key = row_number()) %>%
    group_by(.collapse_key) %>%
    mutate_at(.vars = vars({{loc}}),
              function(.vec) {
                if (inherits(.vec, "list")) {
                  if (is.null(.vec[[1]])) {
                    .vec <- NA
                  } else if (inherits(.vec[[1]], c("character", "numeric", "logical"))) {
                    .vec <- paste(unlist(.vec), collapse = .sep)
                  }
                }
                .vec
              }) %>%
    ungroup(.collapse_key) %>%
    select(-.collapse_key)
}


##################
# PRIVATE HELPERS
##################

#' Search for model YAML files and read them
#'
#' Private helper function that searches from a base directory for any YAML files (excluding `babylon.yaml`)
#' and attempts to read them to a model object with [safe_read_model()].
#' @param .base_dir Directory to search for model YAML files.
#' @param .recurse If `TRUE` search recursively in subdirectories as well.
#' @importFrom stringr str_subset
#' @importFrom purrr map_lgl map compact
#' @importFrom fs dir_ls
#' @keywords internal
find_models <- function(.base_dir, .recurse) {

  # get yaml files
  yaml_files <- dir_ls(.base_dir, recurse = .recurse)
  yaml_files <- str_subset(yaml_files, "\\.ya?ml$")
  yaml_files <- str_subset(yaml_files, "babylon\\.ya?ml$", negate = TRUE)

  # read in all candidate yaml's
  all_yaml <-
    yaml_files %>%
    purrr::map(fs::path_ext_remove) %>%
    purrr::map(safe_read_model)

  # filter to only model yaml's
  mod_list <- purrr::compact(all_yaml)
  if (length(mod_list) != length(all_yaml)) {
    null_idx <- purrr::map_lgl(all_yaml, is.null)
    not_mod <- yaml_files[which(null_idx)]
    warning(glue("Found {length(not_mod)} YAML files that do not contain required keys for a model YAML. Ignoring the following files: `{paste(not_mod, collapse='`, `')}`"))
  }

  # warn if no valid model YAML found
  if (length(mod_list) == 0) {
    warning(glue("Found no valid model YAML files in {.base_dir}"))
  }

  return(mod_list)
}


#' Read in model YAML with error handling
#'
#' Private helper function that tries to call `read_model()` on a yaml path and returns NULL, with no error, if the YAML is not a valid model file.
#'
#' @inheritParams read_model
#'
#' @return A model object, if `.path` represents a valid model.
#' @keywords internal
safe_read_model <- function(.path) {
  tryCatch(
    read_model(.path),
    error = function(e) {
      if (stringr::str_detect(e$message, "Model list must have keys")) {
        return(NULL)
      } else {
        stop(
          glue("Unexpected error trying to read model `{.path}`: {e$message}")
        )
      }
    }
  )
}


#' Add columns to log df
#'
#' Private helper to extact columns from another log tibble and join them onto a `bbi_run_log_df`
#' @importFrom dplyr left_join
#' @importFrom purrr map
#' @param .log_df a `bbi_run_log_df` tibble (the output of [run_log()])
#' @param .impl_func Implementation function to extract the appropriate log tibble that will be joined against the input tibble.
#' @param ... Arguments passed through to `.impl_func`
#' @return The input `bbi_run_log_df` tibble, with any columns from the tibble output by `.impl_func` left joined onto it.
#' @keywords internal
add_log_impl <- function(.log_df, .impl_func, ...) {
  # check input df
  check_bbi_run_log_df_object(.log_df)

  # get config log
  mod_list <- map(.log_df[[ABS_MOD_PATH]], read_model)
  .new_df <- .impl_func(mod_list, ...) %>% select(-{{RUN_ID_COL}})

  # join to log df
  df <- left_join(
    .log_df,
    .new_df,
    by = ABS_MOD_PATH
  )

  # add new class
  .new_class <- setdiff(class(.new_df), class(.log_df))
  class(df) <- c(.new_class, class(.log_df))

  return(df)
}


#' Create a run log row from a `bbi_{.model_type}_model` object
#' @param .mod S3 object of class `bbi_{.model_type}_model`
#' @importFrom tibble tibble
#' @keywords internal
run_log_entry <- function(.mod) {
  checkmate::assert_scalar(.mod[[YAML_YAML_MD5]])
  checkmate::assert_scalar(.mod[[YAML_MOD_TYPE]])
  checkmate::assert_scalar(.mod[[YAML_DESCRIPTION]])

  # build row
  entry_df <- tibble::tibble(
    !!ABS_MOD_PATH      := .mod[[ABS_MOD_PATH]],
    !!YAML_YAML_MD5     := .mod[[YAML_YAML_MD5]],
    !!YAML_MOD_TYPE     := .mod[[YAML_MOD_TYPE]],
    !!YAML_DESCRIPTION  := .mod[[YAML_DESCRIPTION]],
    !!YAML_BBI_ARGS     := .mod[[YAML_BBI_ARGS]] %>% list(),
    !!YAML_BASED_ON     := .mod[[YAML_BASED_ON]] %>% list(),
    !!YAML_TAGS         := .mod[[YAML_TAGS]] %>% list(),
    !!YAML_NOTES        := .mod[[YAML_NOTES]] %>% list(),
    !!YAML_DECISIONS    := .mod[[YAML_DECISIONS]] %>% list()
  )

  entry_df <- add_run_id_col(entry_df)

  # check that it is only one row
  if (nrow(entry_df) != 1) {
    stop(glue("There is a problem with {get_yaml_path(.mod)} file. `run_log()` should be able to load it to 1 row but got {nrow(entry_df)} rows instead. User should not see this error; report to developers if encountered."))
  }

  return(entry_df)
}
