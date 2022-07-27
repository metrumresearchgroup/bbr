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
#' @param .include Provides filter for runs based on an input vector.
#' @importFrom purrr map_df
#' @importFrom tibble tibble
#' @return A tibble of class `bbi_run_log_df` with information on each model, or an empty tibble if no models are found.
#' @export
run_log <- function(.base_dir, .recurse = TRUE, .include = NULL) {
  checkmate::assert_string(.base_dir)

  mod_list <- find_models(.base_dir, .recurse, .include)
  if(length(mod_list) == 0) {
    return(tibble())
  }

  df <- mod_list %>% map_df(run_log_entry)

  df <- create_run_log_object(df)

  return(df)
}


##################
# PRIVATE HELPERS
##################

#' Search for model YAML files and read them
#'
#' Private helper function that searches from a base directory for any YAML files (excluding `bbi.yaml`)
#' and attempts to read them to a model object with [safe_read_model()].
#' @param .base_dir Directory to search for model YAML files.
#' @param .recurse If `TRUE` search recursively in subdirectories as well.
#' @inheritParams run_log
#' @importFrom dplyr filter
#' @importFrom stringr str_subset str_remove
#' @importFrom purrr map_lgl map compact
#' @importFrom fs dir_ls
#' @keywords internal
find_models <- function(.base_dir, .recurse , .include) {

  # get yaml files
  yaml_files <- dir_ls(.base_dir, recurse = .recurse)
  yaml_files <- str_subset(yaml_files, "\\.ya?ml$")
  yaml_files <- str_subset(yaml_files, "bbi\\.ya?ml$", negate = TRUE)


  # read in all candidate yaml's
  all_yaml <-
    yaml_files %>%
    map(fs::path_ext_remove) %>%
    map(safe_read_model)


  # filter to only model yaml's
  mod_list <- compact(all_yaml)
  if (length(mod_list) != length(all_yaml)) {
    null_idx <- map_lgl(all_yaml, is.null)
    not_mod <- yaml_files[which(null_idx)]
    warning(glue("Found {length(not_mod)} YAML files that do not contain required keys for a model YAML. Ignoring the following files: `{paste(not_mod, collapse='`, `')}`"))
  }
  # warn if no valid model YAML found
  if (length(mod_list) == 0) {
    warning(glue("Found no valid model YAML files in {.base_dir}"))
  }

  #If models are not specified to be kept, it keeps all models
  # Filters only to models specified
  if(!is.null(.include)){
    yaml_df <- map(mod_list, ~ {
      .tag <- if(is.null(.x$tags)) NA_character_ else .x$tags
      data.frame(yaml = .x$absolute_model_path, tags = .tag)
    }) %>% bind_rows()
    yaml_df <- yaml_df %>% filter(.data$yaml %>% basename() %>% stringr::str_remove(".yaml|.yml") %in% .include | .data$tags %in% .include)
    yaml_files <- unique(yaml_df$yaml)
    mod_list <- mod_list[map(mod_list, ~ .x$absolute_model_path %in% yaml_files) %>% unlist()]
  }

  if(length(mod_list) == 0){
    warning("All models excluded by filter.")
  }

  return(mod_list)
}


#' Read in model with error handling
#'
#' Private helper function that tries to call `read_model()` on a model path and returns NULL, with no error, if the YAML is not a valid model file.
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
  if (!is.null(.mod[[YAML_DESCRIPTION]])) checkmate::assert_scalar(.mod[[YAML_DESCRIPTION]])

  # build row
  entry_df <- tibble::tibble(
    !!ABS_MOD_PATH      := .mod[[ABS_MOD_PATH]],
    !!YAML_YAML_MD5     := .mod[[YAML_YAML_MD5]],
    !!YAML_MOD_TYPE     := .mod[[YAML_MOD_TYPE]],
    !!YAML_DESCRIPTION  := .mod[[YAML_DESCRIPTION]] %||% NA_character_,
    !!YAML_BBI_ARGS     := .mod[[YAML_BBI_ARGS]] %>% list(),
    !!YAML_BASED_ON     := .mod[[YAML_BASED_ON]] %>% list(),
    !!YAML_TAGS         := .mod[[YAML_TAGS]] %>% list(),
    !!YAML_NOTES        := .mod[[YAML_NOTES]] %>% list(),
    !!YAML_STAR         := .mod[[YAML_STAR]] %||% FALSE,
  )

  entry_df <- add_run_id_col(entry_df)


  # check that it is only one row
  if (nrow(entry_df) != 1) {
    stop(glue("There is a problem with {get_yaml_path(.mod)} file. `run_log()` should be able to load it to 1 row but got {nrow(entry_df)} rows instead. User should not see this error; report to developers if encountered."))
  }

  return(entry_df)
}
