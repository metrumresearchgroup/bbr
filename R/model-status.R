############################
# Model Status
############################
# - `bbi_nonmem_model_status` is the source of truth
#     - These will return "Not Run", "Finished Running", or "Incomplete Run"
#     - Is used in the print method of `bbi_model` objects
# - `model_is_finished` is a generic function that return logical values
#     - These will return `TRUE` if a bbi_config.json file exists, and `FALSE`
#       otherwise
# - `check_nonmem_finished` is an exported wrapper of `model_is_finished`, that
#   supports a list of models, and has a method implemented in bbr.bayes.
#     - The list method is used in `get_model_status` and `wait_for_nonmem`


#' Return status of a model
#'
#' Checks for the presence of `bbi_config.json` file. If one exists, the run
#' has `"Finished Running"`. If a directory exists, but a config file _has not_
#' been generated, the model is `"Incomplete"`. If no directory exists, the
#' model is `"Not Run"`. This is 'source of truth' that all other model status
#' helpers rely on.
#'
#' @param .mod a `bbi_model` object
#' @returns A character string. Either `"Not Run"`, `"Finished Running"`, or
#'  `"Incomplete Run"`.
#' @keywords internal
bbi_nonmem_model_status <- function(.mod) {
  UseMethod("bbi_nonmem_model_status")
}

#' @rdname bbi_nonmem_model_status
#' @keywords internal
bbi_nonmem_model_status.bbi_model <- function(.mod) {
  status <- "Not Run"
  output_dir <- get_output_dir(.mod, .check_exists = FALSE)
  if (dir.exists(output_dir)) {
    json_file <- get_config_path(.mod, .check_exists = FALSE)
    if (fs::file_exists(json_file)) {
      status <- "Finished Running"
    } else {
      status <- "Incomplete Run"
    }
  }
  return(status)
}

#' @rdname bbi_nonmem_model_status
#' @keywords internal
bbi_nonmem_model_status.bbi_nmboot_model <- function(.mod) {
  status <- "Not Run"
  output_dir <- get_output_dir(.mod, .check_exists = FALSE)
  if (dir.exists(output_dir)) {
    cleaned_up <- bootstrap_is_cleaned_up(.mod)
    if (isTRUE(cleaned_up)) {
      status <- "Finished Running"
    } else {
      # If not cleaned up, check each model individually:
      #  - Iterates through all models and sets the status based on whether all
      #    models have finished. This may increase the time required to print an
      #    nmboot model object to the console until the run has been cleaned up.
      spec_path <- get_spec_path(.mod, .check_exists = FALSE)
      if (!fs::file_exists(spec_path)) {
        status <- "Not Run"
      }else{
        boot_spec <- get_boot_spec(.mod)
        for(output_dir.i in boot_spec$bootstrap_runs$mod_path_abs){
          if (dir.exists(output_dir.i)) {
            # Exit early as incomplete if any model cannot be read in for any reason
            boot_m <- tryCatch({read_model(output_dir.i)}, error = function(e) NULL)
            if(is.null(boot_m)) return("Incomplete Run")
            # Otherwise check for presence of config file
            json_file <- get_config_path(boot_m, .check_exists = FALSE)
            if(fs::file_exists(json_file)) {
              # Set to incomplete if one config file exists. Update at the end
              # if they all exist
              status <- "Incomplete Run"
            }else{
              # Return the current status as soon as a config file cant be found
              return(status)
            }
          }else{
            # Return the current status as soon as a directory doesnt exist
            return(status)
          }
        }
        status <- "Finished Running"
      }
    }
  }
  return(status)
}

# Register private S3 methods for development purposes
.S3method("bbi_nonmem_model_status", "bbi_model", bbi_nonmem_model_status.bbi_model)
.S3method("bbi_nonmem_model_status", "bbi_nmboot_model", bbi_nonmem_model_status.bbi_nmboot_model)


#' @describeIn bbi_nonmem_model_status Check if model run has finished (coerces
#' the one of the three statuses to a logical value)
#'
#' @returns Logical (`TRUE`/`FALSE`)
#' @keywords internal
model_is_finished <- function(.mod){
  status <- bbi_nonmem_model_status(.mod)
  if(status == "Finished Running"){
    return(TRUE)
  }else if(status %in% c("Not Run", "Incomplete Run")){
    return(FALSE)
  }else{
    dev_error(glue("Unknown status: `{status}`"))
  }
}


#' Check if bootstrap run has been cleaned up
#' @param .boot_run Either a `bbi_nmboot_model` or `bbi_nmboot_summary` object
#' @noRd
bootstrap_is_cleaned_up <- function(.boot_run){
  check_model_object(.boot_run, c(NMBOOT_MOD_CLASS, NMBOOT_SUM_CLASS))
  if(inherits(.boot_run, NMBOOT_SUM_CLASS)){
    .boot_run <- read_model(.boot_run[[ABS_MOD_PATH]])
  }

  output_dir <- get_output_dir(.boot_run, .check_exists = FALSE)
  if(!fs::file_exists(output_dir)) return(FALSE)
  spec_path <- get_spec_path(.boot_run, .check_exists = FALSE)
  if(!fs::file_exists(spec_path)) return(FALSE)

  boot_spec <- jsonlite::read_json(spec_path, simplifyVector = TRUE)
  cleaned_up <- boot_spec$bootstrap_spec$cleaned_up
  if(!is.null(cleaned_up) && isTRUE(cleaned_up)){
    return(TRUE)
  }else{
    return(FALSE)
  }
}


#' Check the status of `NONMEM` run(s)
#'
#' Checks the status of `bbi_base_model` object(s) by looking for a
#' `bbi_config.json` file. Other operations (such as returning messages or
#' freezing the `R` console) may be done as a result.
#'
#' @param .mod a `bbi_base_model` or list of `bbi_base_model` objects. Other
#'  packages (e.g., `bbr.bayes`) may add additional methods.
#' @param ... Arguments passed to methods.
#'
#' @importFrom readr read_lines
#' @importFrom stringr str_detect
#'
#' @details
#'
#' If the result of `get_model_status()` assigned to a variable, users can
#' inspect the full list of runs to determine exactly which models are still
#' running vs. have finished executing:
#'
#' ```
#' res <- get_model_status(mod_list)
#' res
#' ```
#'
#' ### Bootstrap Runs
#' When providing a `bbi_nmboot_model` object to `get_model_status()` or
#' `wait_for_nonmem()`, the individual model runs (`get_boot_models(.boot_run)`)
#' will be checked instead, returning more informative messages about the status
#' of the bootstrap run.
#'
#' @examples
#' \dontrun{
#'
#' # Check if model(s) or bootstrap run has finished:
#'
#' check_nonmem_finished(.mod)
#' #> [1] TRUE
#'
#' check_nonmem_finished(.boot_run)
#' #> [1] FALSE
#'
#'
#' # Inspect progress of model(s) or bootstrap run:
#'
#' get_model_status(.mod)
#' #> The following model(s) have finished: `1`
#' #> 0 model(s) are incomplete
#'
#' get_model_status(.boot_run)
#' #> 25 model(s) have finished
#' #> 75 model(s) are incomplete
#'
#' get_model_status(list(.mod, .boot_run))
#' #> The following model(s) have finished: `1`
#' #> The following model(s) are incomplete: `1-boot`
#'
#'
#' # Freeze the `R` console until model(s) or bootstrap run has finished:
#'
#' wait_for_nonmem(.mod)
#' #> Waiting for 1 model(s) to finish...
#' #> 1 model(s) have finished
#'
#' # Batch submissions take longer to start
#' wait_for_nonmem(.boot_run, .delay = 6)
#' #> Waiting for 100 model(s) to finish...
#' #> Waiting for 50 model(s) to finish...
#' #> 100 model(s) have finished
#'
#' }
#'
#' @name model_status
NULL


#' @describeIn model_status Returns `TRUE` if the model appears to be finished
#'  running and `FALSE` otherwise.
#' @export
check_nonmem_finished <- function(.mod, ...) {
  UseMethod("check_nonmem_finished")
}


#' @export
check_nonmem_finished.bbi_base_model <- function(.mod, ...) {
  model_finished <- model_is_finished(.mod)
  return(model_finished)
}


#' @export
check_nonmem_finished.list <- function(.mod, ...) {
  check_model_object_list(.mod, .mod_types = c(NM_MOD_CLASS, NMBOOT_MOD_CLASS, NMSIM_MOD_CLASS))
  # Return logical values as vector (unique handling)
  #  - used in `wait_for_nonmem`
  models_finished <- map_lgl(.mod, ~model_is_finished(.x))
  return(models_finished)
}



#' @describeIn model_status Returns messages indicating which model(s) have
#'  finished executing and which are incomplete. Also invisibly returns a
#'  `data.frame` with a row for each model and a logical `finished` column.
#' @param max_print max number of models to explicitly print to the console. If
#' the number of finished or incomplete models are greater than this number,
#' just print the _number_ of models.
#'
#' @export
get_model_status <- function(.mod, max_print = 10, ...){
  UseMethod("get_model_status")
}


#' @export
get_model_status.default <- function(.mod, max_print = 10, ...){
  checkmate::assert_number(max_print, lower = 1)

  # Coerce to list of models regardless of input
  mod_list <- mod_list_setup(.mod)
  # Exit for bootstrap runs that haven't been set up or were already cleaned up
  if(is.null(mod_list)) return(invisible(NULL))

  mod_ids <- purrr::map_chr(mod_list, get_model_id)
  res <- check_nonmem_finished(mod_list) %>% tibble::as_tibble() %>%
    dplyr::transmute(model_id = mod_ids, finished = .data$value)

  res_fin <- res$model_id[res$finished]
  if(length(res_fin) > 0 && length(res_fin) <= max_print){
    mods_fin <- paste(res_fin, collapse = ", ")
    message(glue("The following model(s) have finished: `{mods_fin}`"))
  }else{
    message(glue("{length(res_fin)} model(s) have finished"))
  }

  res_inc <- res$model_id[!res$finished]
  if(length(res_inc) > 0 && length(res_inc) <= max_print){
    mods_inc <- paste(res_inc, collapse = ", ")
    message(
      paste(
        "The following model(s) are incomplete or have not been run:",
        glue("`{mods_inc}`")
      )
    )
  }else{
    message(glue("{length(res_inc)} model(s) are incomplete or have not been run"))
  }

  return(invisible(res))
}



#' @describeIn model_status Wait for `NONMEM` models to finish. i.e. freeze the
#'  user's console until the model(s) have finished running.
#' @param .time_limit integer for maximum number of seconds in total to wait
#'  before continuing (will exit after this time even if the run does not appear
#'  to have finished).
#' @param .interval integer for number of seconds to wait between each check.
#' @param .delay integer for number of seconds to wait before scanning the output
#'  directories. This function will exit early if no output directory exists, so
#'  this argument serves to delay this evaluation (e.g. if calling right after
#'  [submit_model()] in an `Rmarkdown` file).
#' @export
wait_for_nonmem <- function(.mod, .time_limit = 300, .interval = 5, .delay = 1.5) {
  UseMethod("wait_for_nonmem")
}


#' @export
wait_for_nonmem.default <- function(.mod, .time_limit = 300, .interval = 5, .delay = 1.5) {

  # Coerce to list of models regardless of input
  mod_list <- mod_list_setup(.mod)
  # Exit for bootstrap runs that haven't been set up or were already cleaned up
  if(is.null(mod_list)) return(invisible(NULL))

  Sys.sleep(.delay) # wait for lst file to be created

  output_dirs <- purrr::map_chr(mod_list, get_output_dir, .check_exists = FALSE)
  if(!any(fs::dir_exists(output_dirs))){
    # Exit if output directory(ies) don't exist
    # Either the model(s) haven't been submitted, or some bbi error occurred
    verbose_msg("No output directory(ies) found...exiting early")
    return(invisible(TRUE))
  }else{
    verbose_msg(glue("Waiting for {length(mod_list)} model(s) to finish..."))
    expiration <- Sys.time() + .time_limit
    n_interval <- 0

    while ((expiration - Sys.time()) > 0) {
      res <- check_nonmem_finished(mod_list)
      if (all(res)) {
        break
      }else{
        n_interval = n_interval + 1
        # print message every 10 intervals
        if(n_interval %% 10 == 0){
          verbose_msg(glue("Waiting for {length(res[!res])} model(s) to finish..."))
        }
      }
      Sys.sleep(.interval)
    }

    # Check again after expiration is reached or models are finished
    res <- check_nonmem_finished(mod_list)
    if(expiration < Sys.time() && !all(res)){
      warning(
        glue("Expiration was reached, but {length(res[!res])} model(s) haven't finished"),
        call. = FALSE, immediate. = TRUE)
      return(invisible(FALSE))
    }else{
      verbose_msg(glue("\n{length(mod_list)} model(s) have finished"))
      return(invisible(TRUE))
    }
  }
}

#' Coerce model object to a list of models for use in `get_model_status` and
#' `wait_for_nonmem`
#'
#' Coerce model object to a list of models (if not one already) for use in
#' `get_model_status` and `wait_for_nonmem`. For `bbi_nmboot_model` objects, the
#' list of individual models corresponding to that run will be returned.
#' @inheritParams check_nonmem_finished
#' @details
#' `.mod` is a list of models regardless of input after this section
#'  - Support a single bbi_nonmem_model, a single bbi_nmboot_model, or a list
#'    of bbi_base_models (bbi_nonmem_model or bbi_nmboot_model)
#' @keywords internal
mod_list_setup <- function(.mod){
  if(inherits(.mod, "list") && !inherits(.mod, "bbi_base_model")){
    check_model_object_list(.mod, .mod_types = c(NM_MOD_CLASS, NMBOOT_MOD_CLASS, NMSIM_MOD_CLASS))
  }else{
    check_model_object(.mod, .mod_types = c(NM_MOD_CLASS, NMBOOT_MOD_CLASS, NMSIM_MOD_CLASS))
    if(inherits(.mod, NMBOOT_MOD_CLASS)){
      # Coerce to list of models for bootstrap model runs to check individually
      #  - This only happens if checking a single bootstrap run model object
      .mod <- get_boot_models(.mod)
    }else{
      .mod <- list(.mod)
    }
  }
  return(.mod)
}
