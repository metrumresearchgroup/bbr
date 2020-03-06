# Parsing model summary outputs into R

################################################
# s3 dispatches to parse NONMEM output to list
################################################

#' S3 generic for getting model summary
#' @param .mod generic .mod
#' @param ... args passed through
#' @export
#' @rdname model_summary
model_summary <- function(.mod, ...) {
  UseMethod("model_summary")
}

#' Get model summary from `bbi_nonmem_model` object and optionally check if model is done before erroring
#' @param .mod `bbi_nonmem_model` object for summary
#' @param ... arguments passed through to `nonmem_summary()`
#' @rdname model_summary
#' @export
model_summary.bbi_nonmem_model <- function(.mod, ...) {
  res_list <- nonmem_summary(.mod, ...)
  return(res_list)
}

#' S3 dispatch for getting model summary from output directory path
#' @param .mod Full path to one of the following:
#'     1) path to a model file in the output directory
#'     2) path to a nonmem output directory
#' @param .model_type Character scaler specifying the type of model, either 'nonmem' or 'stan'
#' @param ... args passed through
#' @export
#' @rdname model_summary
model_summary.character <- function(.mod, .model_type = c("nonmem", "stan"), ...) {
  .model_type <- match.arg(.model_type)
  if (.model_type == "nonmem") {
    .mod <- read_model(.mod)
    res_list <- nonmem_summary(.mod, ...)
  } else if (.model_type == "stan") {
    stop(NO_STAN_ERR_MSG)
  } else {
    stop(glue("Passed `{.model_type}`. Valid options: `{SUPPORTED_MOD_TYPES}`"))
  }
  return(res_list)
}

#' Run `bbi nonmem summary` and parse the output to a list
#' @param .mod `bbi_nonmem_model` object for summary
#' @param .args A named list specifying arguments to pass to babylon formatted like `list("nm_version" = "nm74gf_nmfe", "json" = T, "threads" = 4)`. Run `print_nonmem_args()` to see valid arguments.
#' @param ... args passed through to `bbi_exec()`
#' @param .dry_run show what the command would be without actually running it
#' @return List of S3 Class "bbi_nonmem_summary" with all summary information
#' @rdname model_summary
#' @export
nonmem_summary <- function(.mod,
                           .args = NULL,
                           ...,
                           .dry_run = FALSE
) {

  # extract output path
  .path <- file.path(.mod[[WORKING_DIR]], .mod[[YAML_OUT_DIR]])

  # lst file can both be the check for whether the dir is a nonmem output dir
  # and also the output always should be runname.lst so we can determine the model name
  # this is definitely better checking for .mod as there are temporary files with that extension
  # as well
  lst_file_path <- check_lst_file(.path)

  # build command line args
  if (is.null(.args)) {
    .args <- list()
  }
  .args <- purrr::list_modify(.args, json = TRUE)

  args_vec <- check_nonmem_args(.args)
  cmd_args <- c("nonmem", "summary", get_mod_id(lst_file_path), args_vec)

  # if .dry_run return output call
  if (.dry_run) {
    res <- bbi_dry_run(cmd_args, .path)
    return(res)
  }

  # otherwise, execute
  res <- tryCatch(
    bbi_exec(cmd_args, .dir = .path, ..., .wait = TRUE),
    error = function(e) {
      err_msg <- glue("nonmem_summary('{.path}') failed with the following error. This may be because the modeling run has not finished successfully. ERROR: \n{e}")
      stop(err_msg)
    }
  )

  res_list <- res$stdout %>%
    paste(collapse="") %>%
    jsonlite::fromJSON(simplifyDataFrame = FALSE)

  class(res_list) <- c("bbi_nonmem_summary", class(res_list))

  return(res_list)
}

#' Helper function to look for .lst function in a directory
#' @param .x The directory path to look in for the lst file
check_lst_file <- function(.x) {
  lst_file <- fs::dir_ls(.x, type = "file", glob = "*.lst")
  if (!length(lst_file)) {
    stop(glue("Unable to locate `.lst` file in dir: {.x}. Check to be sure this is a NONMEM output folder, and that the run has finished successfully."))
  }
  lst_file
}


######################################
# format NONMEM output to tables
######################################

#' S3 generic for parsing parameter estimate table
#' @param .summary generic summary object
#' @export
#' @rdname param_estimates
param_estimates <- function(.summary) {
  UseMethod("param_estimates")
}

#' S3 dispatch for parsing `bbi_nonmem_summary` object into parameter estimate table
#' @param .summary `bbi_nonmem_summary` object
#' @importFrom tibble tibble
#' @export
#' @rdname param_estimates
param_estimates.bbi_nonmem_summary <- function(.summary) {
  num_methods <- length(.summary$parameters_data)
  param_names <- .summary$parameter_names
  param_estimates <- .summary$parameters_data[[num_methods]]$estimates
  tibble::tibble(
    names = unlist(param_names),
    estimate = unlist(param_estimates),
    stderr = unlist(.summary$parameters_data[[num_methods]]$std_err) %||% NA_real_,
    fixed = unlist(.summary$parameters_data[[num_methods]]$fixed),
  )
}



