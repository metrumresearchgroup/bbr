#' Run `bbi summary` and parse the output to a list
#' @param .path Full path to one of the following:
#'     1) path to a model file in the output directory
#'     2) path to a nonmem output directory
#' @param .args A named list specifying arguments to pass to babylon formatted like `list("nm_version" = "nm74gf_nmfe", "json" = T, "threads" = 4)`. Run `print_nonmem_args()` to see valid arguments.
#' @param ... args passed through to `bbi_exec()`
#' @param .dry_run show what the command would be without actually running it
#' @return List of S3 Class "bbi_nonmem_summary" with all summary information
#' @rdname model_summary
#' @export
nonmem_summary <- function(.path,
                                .args = NULL,
                                ...,
                                .dry_run = FALSE
                                ) {

  # if file was passed, reset to directory path
  if (!fs::is_dir(.path)) {
    .path <- dirname(.path)
  }
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
    return(list(
      cmd_args = cmd_args,
      wd = .path,
      call = paste(
        "cd", .path, ";",
        getOption("rbabylon.bbi_exe_path"),
        paste(cmd_args, collapse = " ")
      )
    ))
  }

  # otherwise, execute
  res <- bbi_exec(cmd_args, wd = .path, ..., .wait = TRUE)

  res_list <- get_stdout(res)$output %>%
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
    stop(glue("unable to locate lst file in dir: {.x}, cannot proceed..."))
  }
  lst_file
}


# s3 dispatches to parse NONMEM output to list

#' S3 generic for getting model summary
#' @param .res generic .res
#' @param ... args passed through
#' @export
#' @rdname model_summary
model_summary <- function(.res, ...) {
  UseMethod("model_summary", .res)
}

#' Get model summary from `bbi_nonmem_result` object and optionally check if model is done before erroring
#' @param .res `bbi_nonmem_result` object for summary
#' @param .wait Integer number of seconds to wait trying `check_nonmem_progress()` for the model to be done.
#' @param .ext_wait Integer number of seconds to wait for an .ext file (i.e. for the model to start). Passed through to `check_nonmem_progress()`
#' @param ... arguments passed through to `nonmem_summary()`
#' @rdname model_summary
#' @export
model_summary.bbi_nonmem_result <- function(.res, .wait = 30, .ext_wait = 30, ...) {
  # if .wait=NULL just run once and don't wait for ext file
  if (is.null(.wait)) {
    .done <- check_nonmem_progress(.res, .ext_wait = 0)
    if (!.done) {
      stop(glue("Summary failed because {.res$model_path} is not finished. Check back later or set `.wait` to a positive non-zero value."))
    }
  }

  start <- Sys.time()
  .chill <- NULL
  # check if model run is finished
  .done <- check_nonmem_progress(.res, .ext_wait = .ext_wait)
  while(!.done) {
    .chill <- 3
    if (as.numeric(Sys.time() - start) < .wait) {
      .done <- check_nonmem_progress(.res, .ext_wait = .ext_wait)
    } else {
      stop(glue("{.res$model_path} is not finished and {.wait} second wait time has elapsed. Check back later or increase `.wait`."))
    }
  }
  if (!is.null(.chill)) {
    cat("\n\n---\nModel run finished. Preparing summary...")
    Sys.sleep(.chill)
    cat(" done.", sep = "\n")
  }

  # get summary
  res_list <- nonmem_summary(.res$output_dir, ...)
  return(res_list)
}

#' S3 dispatch for getting model summary from output directory path
#' @param .res generic .res
#' @param ... args passed through
#' @export
#' @rdname model_summary
model_summary.character <- function(.res_dir, .model_type = c("nonmem"), ...) {
  .model_type <- match.arg(.model_type)
  if (.model_type == "nonmem") {
    res_list <- nonmem_summary(.res_dir, ...)
  } else {
    stop(glue("Passed `{.model_type}` to model_summary(.model_type). Valid options include: `'nonmem'`"))
  }
  return(res_list)
}


#### considering if I should do something like the following (unfinished) to create a `bbi_result_spec` from a path
#### not sure if this has any real point, but it feels more consistent with how the spec and result are done.
#### also, consider a way to get to model_summary from run_log. Is this related/useful towards that goal?

# #' Create a `bbi_nonmem_res` object from a file path
# #' @export
# create_res_from_path <- function(.path) {
#   files_exist_bool <- fs::dir_exists(tools::file_path_sans_ext(.path)) &&
#                       fs::file_exists(yaml_ext(.path)) &&
#                       (fs::file_exists(ctl_ext(.path)) || fs::file_exists(mod_ext(.path)))
#   if (!files_exist_bool) {
#
#   }
#
# }


######################################
# format NONMEM output to tables
######################################

#' S3 generic for parsing parameter estimate table
#' @param .x generic .x
#' @param ... args passed through
#' @export
#' @rdname param_estimates
param_estimates <- function(.x, ...) {
  UseMethod("param_estimates", .x)
}

#' S3 dispatch for parsing `bbi_nonmem_summary` object into parameter estimate table
#' @param .x `bbi_nonmem_summary` object
#' @param ... args passed through
#' @export
#' @rdname param_estimates
param_estimates.bbi_nonmem_summary <- function(.x) {
  num_methods <- length(.x$parameters_data)
  param_names <- .x$parameter_names
  param_estimates <- .x$parameters_data[[num_methods]]$estimates
  tibble::tibble(
    names = unlist(param_names),
    estimate = unlist(param_estimates),
    stderr = unlist(.x$parameters_data[[num_methods]]$std_err) %||% NA_real_,
    fixed = unlist(.x$parameters_data[[num_methods]]$fixed),
  )
}



