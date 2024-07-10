#' Interface for running `NM-TRAN` on model objects
#'
#' Provides functions to run `NM-TRAN` on model objects for validation and to
#' generate `NM-TRAN` datasets (`FDATA`).
#'
#' @details
#' `NM-TRAN` is a preprocessor for `NONMEM` that translates user-specified
#' control stream data and instructions into a form executable by `NONMEM`.
#'
#' `run_nmtran()` allows users to test their models ahead of submission to ensure
#' correct coding, whereas `nm_fdata()` generates and returns the `NM-TRAN`
#' dataset (`FDATA`) for further analysis and verification.
#'
#' @param .mod A `bbr` model object.
#' @param .config_path Path to a bbi configuration file. If `NULL`, the default,
#'   will attempt to use a `bbi.yaml` in the same directory as the model.
#' @param nmtran_exe Path to an `NM-TRAN` executable. If `NULL`, will look for a
#'   `bbi.yaml` file in the same directory as the model.
#' @param delete_on_exit Logical. If `FALSE`, don't delete the temporary folder
#'   containing the `NM-TRAN` run, which will be stored in the current working
#'   directory.
#'
#' @examples
#' \dontrun{
#' mod <- read_model(file.path(MODEL_DIR, 1))
#' run_nmtran(mod)
#'
#' # Set the path to an NM-TRAN executable
#' run_nmtran(mod, nmtran_exe = "/opt/NONMEM/nm75/tr/NMTRAN.exe")
#'
#' # Generate and return `FDATA`
#' fdata <- nm_fdata(mod)
#'
#' }
#' @name nmtran
NULL


#' @describeIn nmtran Run `NM-TRAN` on a model object to validate its control
#' stream for correct coding before submission.
#' @export
run_nmtran <- function(
    .mod,
    .config_path = NULL,
    nmtran_exe = NULL,
    delete_on_exit = TRUE
){
  check_model_object(.mod, "bbi_nonmem_model")

  # Capture NONMEM and NMTRAN options
  nmtran_exe <- locate_nmtran(.mod, .config_path, nmtran_exe)
  nm_ver <- attr(nmtran_exe, "nonmem_version")

  mod_path <- get_model_path(.mod)
  data_path <- get_data_path_from_ctl(.mod)

  # make temporary directory in current directory
  mod_name <- fs::path_ext_remove(basename(mod_path))
  temp_folder <- paste0("nmtran_", mod_name, "_", basename(tempdir()))
  if(fs::dir_exists(temp_folder)) fs::dir_delete(temp_folder)
  fs::dir_create(temp_folder)
  if(isTRUE(delete_on_exit)){
    on.exit(unlink(temp_folder, recursive = TRUE, force = TRUE))
  }

  # Copy model
  file.copy(mod_path, temp_folder, overwrite = TRUE)
  nmtran_mod <- new_model(file.path(temp_folder, basename(mod_path)), .overwrite = TRUE)

  # Copy dataset & overwrite $DATA record of new model
  # NMTRAN will error if data cannot be found
  if(fs::file_exists(data_path)){
    file.copy(data_path, temp_folder, overwrite = TRUE)
    # overwrite $DATA record of new model
    modify_data_path_ctl(nmtran_mod, data_path = basename(data_path))
  }

  # Run NMTRAN
  nmtran_results <- c(
    list(
      nmtran_exe = as.character(nmtran_exe),
      nonmem_version = nm_ver,
      absolute_model_path = mod_path
    ),
    execute_nmtran(nmtran_exe, mod_path = basename(mod_path), dir = temp_folder)
  )

  # assign class and return
  class(nmtran_results) <- c(NMTRAN_PROCESS_CLASS, class(nmtran_results))
  return(nmtran_results)
}


#' @describeIn nmtran Executes `run_nmtran` on a `bbi_nonmem_model` and
#'  returns the `NM-TRAN` dataset (`FDATA`)
#' @export
nm_fdata <- function(
    .mod,
    .config_path = NULL,
    nmtran_exe = NULL
){
  nmtran_p <- run_nmtran(.mod, .config_path, nmtran_exe, delete_on_exit = FALSE)
  on.exit(fs::dir_delete(nmtran_p$run_dir))

  if(nmtran_p$status_val != 0){
    # trim output
    output_lines <- nmtran_p$output_lines[!grepl("^\\s+$", nmtran_p$output_lines)]
    rlang::warn(
      c(
        "NM-TRAN was unsuccessful and returned the following messages:",
        paste(output_lines, collapse = "\n")
      )
    )
  }

  # Attempt to read in FDATA (even if status_val is not 0)
  #  - FDATA can still be read in in _some scenarios_ where NM-TRAN fails
  fdata_path <- file.path(nmtran_p$run_dir, "FDATA")
  if(fs::file_exists(fdata_path)){
    input_data <- nm_data(.mod) %>% suppressMessages()
    fdata <- tryCatch({
      nm_file_impl(fdata_path, skip = 0) %>%
        stats::setNames(names(input_data))
    }, error = function(cond){
      rlang::inform(
        c("FDATA could not be read in:", cond$parent$message)
      )
      return(NULL)
    })

    if(!is.null(fdata)){
      attr(fdata, "n_records_dropped") <- nrow(input_data) - nrow(fdata)
    }

    # assign class and return
    class(fdata) <- c(NMTRAN_FDATA_CLASS, class(fdata))
    return(fdata)
  }else{
    return(invisible(NULL))
  }
}


#' Search for and validate existence of an `NM-TRAN` executable
#'
#' If `nmtran_exe = NULL`, this will look for a `bbi.yaml` file in the same
#' directory as the model.
#'
#' @inheritParams run_nmtran
#'
#' @keywords internal
locate_nmtran <- function(.mod = NULL, .config_path = NULL, nmtran_exe = NULL){

  if(is.null(nmtran_exe)){
    if(!is.null(.mod)){
      check_model_object(.mod, "bbi_nonmem_model")
      model_dir <- get_model_working_directory(.mod)
    }
    config_path <- .config_path %||% file.path(model_dir, "bbi.yaml")

    if(!file_exists(config_path)){
      rlang::abort(
        c(
          "x" = "No bbi configuration was found in the execution directory.",
          "i" = "Please run `bbi_init()` with the appropriate directory to continue."
        )
      )
    }

    if(!is.null(.config_path)){
      config_path <- normalizePath(.config_path)
    }

    bbi_config <- yaml::read_yaml(config_path)
    nm_config <- bbi_config$nonmem

    # look for default nonmem installation
    default_nm <- purrr::keep(nm_config, function(nm_ver){
      !is.null(nm_ver$default)
    })

    # Set nonmem path
    if(length(default_nm) > 0){
      default_nm <- default_nm[[1]]
    }else{
      # If no default, use the last one (likely higher version)
      default_nm <- nm_config[[length(nm_config)]]
    }

    # Set NMTRAN executable path
    nm_path <- default_nm$home
    # TODO: should we recursively look for this executable, or assume Metworx?
    # i.e. can we assume NMTRAN is in a `tr` folder, and is called `NMTRAN.exe`?
    nmtran_exe <- file.path(nm_path, "tr", "NMTRAN.exe")

    # If executable found via bbi.yaml, append NONMEM version as attribute
    attr(nmtran_exe, "nonmem_version") <- basename(default_nm$home)
  }

  if(!file_exists(nmtran_exe)){
    stop(glue("Could not find an NMTRAN executable at `{nmtran_exe}`"))
  }

  return(nmtran_exe)
}


#' Execute `NM-TRAN` in a given directory
#'
#' @param nmtran_exe Path to `NM-TRAN` executable.
#' @param mod_path Path of a model to evaluate. Should be relative to `dir`.
#' @param dir Directory in which to execute the command.
#'
#' @keywords internal
execute_nmtran <- function(nmtran_exe, mod_path, dir = NULL) {
  if(is.null(dir)) dir <- "."
  checkmate::assert_directory_exists(dir)

  nmtran.p <- processx::process$new(
    command = nmtran_exe, args = mod_path, wd = dir,
    stdout = "|", stderr="|", stdin = file.path(dir, mod_path)
  )

  # Wait till finished for status to be reflective of result
  nmtran.p$wait()

  # Assign status
  status <- "Not Run"
  status_val <- nmtran.p$get_exit_status()
  if(status_val == 0){
    status <- "NMTRAN successful"
  }else{
    status <- "NMTRAN failed. See errors."
  }

  # Tabulate NMTRAN results
  nmtran_results <- list(
    nmtran_model = nmtran.p$get_input_file(),
    run_dir = as.character(fs::path_real(dir)),
    status = status, status_val = status_val,
    output_lines = nmtran.p$read_all_output_lines(),
    error_lines = nmtran.p$read_all_error_lines()
  )

  return(nmtran_results)
}
