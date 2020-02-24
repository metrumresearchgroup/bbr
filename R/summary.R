#' Run `bbi summary` and parse the output to a list
#' @param .res `bbi_nonmem_result` object for summary
#' @param .wait Integer number of seconds to wait trying `check_nonmem_progress()` for the model to be done.
#' @param .ext_wait Integer number of seconds to wait for an .ext file (i.e. for the model to start). Passed through to `check_nonmem_progress()`
#' @param .args A named list specifying arguments to pass to babylon formatted like `list("nm_version" = "nm74gf_nmfe", "json" = T, "threads" = 4)`. Run `print_nonmem_args()` to see valid arguments.
#' @param ... args passed through to `bbi_exec()`
#' @param .dry_run show what the command would be without actually running it
#' @return List of S3 Class "bbi_nonmem_summary" with all summary information
#' @rdname model_summary
#' @export
nonmem_summary <- function(.res,
                           .wait = 30,
                           .ext_wait = 30,
                           .args = NULL,
                           ...,
                           .dry_run = FALSE
                           ) {

  # if .wait=NULL just look for output once and don't wait for ext file
  if (is.null(.wait)) {
    .done <- check_nonmem_progress(.res, .ext_wait = 0)
    if (!.done) {
      stop(glue("Summary failed because {.res[[YAML_MOD_PATH]]} is not finished. Check back later or set `.wait` to a positive non-zero value."))
    }
  }

  # if .wait not null, record time and check if model run is finished
  start <- Sys.time()
  .chill <- NULL
  .done <- check_nonmem_progress(.res, .ext_wait = .ext_wait)

  # if not, keep looking until wait time is exhausted
  while(!.done) {
    .chill <- 3
    if (as.numeric(Sys.time() - start) < .wait) {
      .done <- check_nonmem_progress(.res, .ext_wait = .ext_wait)
    } else {
      stop(glue("{.res[[YAML_MOD_PATH]]} is not finished and {.wait} second wait time has elapsed. Check back later or increase `.wait`."))
    }
  }
  if (!is.null(.chill)) {
    cat("\n\n---\nModel run finished. Preparing summary...")
    Sys.sleep(.chill)
    cat(" done.", sep = "\n")
  }

  # extract output path
  .path <- file.path(.res[[WORKING_DIR]], .res[[YAML_OUT_DIR]])

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
#' @param ... arguments passed through to `nonmem_summary()`
#' @rdname model_summary
#' @export
model_summary.bbi_nonmem_result <- function(.res, ...) {

  # get summary
  res_list <- nonmem_summary(.res, ...)
  return(res_list)
}

#' S3 dispatch for getting model summary from output directory path
#' @param .path Full path to one of the following:
#'     1) path to a model file in the output directory
#'     2) path to a nonmem output directory
#' @param .model_type Character scaler specifying the type of model, either 'nonmem' or 'stan'
#' @param ... args passed through
#' @export
#' @rdname model_summary
model_summary.character <- function(.path, .model_type = c("nonmem", "stan"), ...) {
  .model_type <- match.arg(.model_type)
  if (.model_type == "nonmem") {
    .res <- create_nonmem_res_from_path(.path)
    res_list <- nonmem_summary(.res, ...)
  } else if (.model_type == "stan") {
    stop(NO_STAN_ERR_MSG)
  } else {
    stop(glue("Passed `{.model_type}`. Valid options: `{SUPPORTED_MOD_TYPES}`"))
  }
  return(res_list)
}


#' Create a `bbi_nonmem_res` object from a file path (should work with either path to a model file, yaml file, and output folder)
#' @param .path Character scaler with the file path to use
#' @rdname model_summary
create_nonmem_res_from_path <- function(.path) {
  # check for the required files
  .output_dir <- tools::file_path_sans_ext(.path)
  .working_dir <- normalizePath(dirname(.output_dir))
  if (!fs::dir_exists(.output_dir)) {
    stop(glue("No directory exists at {.output_dir} -- Must pass path to a valid output directory."))
  }
  check_lst_file(.output_dir)

  .potential_yaml_file <- .output_dir %>% get_mod_id() %>% yaml_ext()
  .yaml_path <- file.path(.working_dir, .potential_yaml_file)
  if (fs::file_exists(.yaml_path)) {
    .spec <- parse_mod_yaml(.yaml_path)
  } else {
    .spec <- list()
    .spec[[WORKING_DIR]] <- .working_dir
    .spec[[YAML_MOD_TYPE]] <- "nonmem"
    .spec[[YAML_DESCRIPTION]] <- as.character(glue("Results object created from {.output_dir} with `create_nonmem_res_from_path()`"))
    .spec[[YAML_MOD_PATH]] <- get_model_file_path(.path)
    .spec[[YAML_BBI_ARGS]] <- list()

  }

  # fill with results info
  .spec[[YAML_OUT_DIR]] <- basename(.output_dir)
  .spec[["cmd_args"]] <- ""

  # assign class and return
  .res <- assign_result_class(.spec, .model_type = "nonmem")

  return(.res)
}


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



