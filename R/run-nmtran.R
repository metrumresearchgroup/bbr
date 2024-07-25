#' Interface for running `NM-TRAN` on model objects
#'
#' Function to run `NM-TRAN` on a model object to validate its control stream
#'  for correct coding before submission. The `NM-TRAN` dataset (`FDATA`) and
#'  other `NONMEM` artifacts can be further inspected by keeping the run directory
#'  around.
#'
#' @details
#' `NM-TRAN` is a preprocessor for `NONMEM` that translates user-specified
#' control stream data and instructions into a form executable by `NONMEM`.
#'
#' Note that `nmtran_presort` is run ahead of `NM-TRAN` for `NONMEM` versions
#' `'nm74gf'`, `'nm74gf_nmfe'`, and `'nm75'`.
#'  - `nmtran_presort` is an supplementary utility that preprocesses the control
#'     stream to ensure it is in the correct format for `NM-TRAN`. It is particularly
#'     relevant for handling specific data manipulations and ensuring compatibility
#'     with the `NM-TRAN` executable.
#'
#' @param .mod A `bbi_nonmem_model` object.
#' @param .bbi_args A named list specifying arguments to pass to `NM-TRAN`.
#'   Similar to the `.bbi_args` argument defined in [submit_model()], though here
#'   only `prdefault`, `tprdefault`, and `maxlim` flags are passed to `NM-TRAN`.
#'   See [print_bbi_args()] for more details.
#' @inheritParams submit_model
#' @param .nonmem_version Character scalar for default version of NONMEM to use.
#'   If left `NULL`, will look for the default version specified in the provided
#'   `bbi` configuration file.
#' @param clean Logical (`T`/`F`). If `FALSE`, don't delete the temporary folder
#'   containing the `NM-TRAN` run, which will be stored in the current working
#'   directory.
#' @param run_dir Directory to run `NM-TRAN` in. Only relevant if `clean = FALSE`.
#'
#' @examples
#' \dontrun{
#'
#' mod <- read_model(file.path(MODEL_DIR, 1))
#' run_nmtran(mod, .nonmem_version = "nm75")
#'
#' # Save the run directory for manual inspection
#' run_nmtran(mod, clean = FALSE, run_dir = getwd())
#'
#' }
#'
#' @export
run_nmtran <- function(
    .mod,
    .bbi_args = list(prdefault = FALSE, tprdefault = FALSE, maxlim = 2),
    .nonmem_version = NULL,
    .config_path = NULL,
    run_dir = tempdir(),
    clean = TRUE
){
  check_model_object(.mod, "bbi_nonmem_model")

  # Capture NONMEM/NM-TRAN options and format `NM-TRAN` args
  nmtran_specs <- nmtran_setup(.mod, .nonmem_version, .bbi_args, .config_path)

  mod_path <- get_model_path(.mod)
  data_path <- get_data_path_from_ctl(.mod)

  # Make temporary directory in current directory
  mod_name <- fs::path_ext_remove(basename(mod_path))
  temp_folder <- withr::local_tempdir(
    pattern = paste0("nmtran-mod_", mod_name, "-"),
    tmpdir = run_dir, clean = clean
  )

  # Copy model
  file.copy(mod_path, temp_folder)
  nmtran_mod <- new_model(file.path(temp_folder, basename(mod_path)))

  # Copy dataset & overwrite $DATA record of new model
  # NM-TRAN will error if data cannot be found
  if(fs::file_exists(data_path)){
    file.copy(data_path, temp_folder)
    # overwrite $DATA record of new model
    modify_data_path_ctl(nmtran_mod, data_path = basename(data_path))
  }

  # Run NM-TRAN
  nmtran_results <- c(
    list(
      nmtran_exe = nmtran_specs$nmtran_exe,
      nmtran_presort_exe = nmtran_specs$nmtran_presort_exe,
      nonmem_version = nmtran_specs$nonmem_version,
      absolute_model_path = mod_path
    ),
    execute_nmtran(
      nmtran_specs$nmtran_exe,
      mod_path = basename(mod_path),
      cmd_args = nmtran_specs$cmd_args,
      nmtran_presort_exe = nmtran_specs$nmtran_presort_exe,
      dir = temp_folder
    )
  )

  # assign class and return
  class(nmtran_results) <- c(NMTRAN_PROCESS_CLASS, class(nmtran_results))
  return(nmtran_results)
}


#' Sets up `NM-TRAN` to run by identifying an `NM-TRAN` executable for a
#'  provided `NONMEM` version and formatting arguments based on `.bbi_args`,
#'  the corresponding `.mod` yaml file, and any `nmfe_options` defined in a
#'  `bbi.yaml` file
#'
#' @inheritParams run_nmtran
#' @keywords internal
nmtran_setup <- function(
    .mod = NULL,
    .nonmem_version = NULL,
    .bbi_args = NULL,
    .config_path = NULL
){
  bbi_yaml_path <- get_bbi_yaml_path(.mod, .config_path = .config_path)
  bbi_yaml <- yaml::read_yaml(bbi_yaml_path)

  # Combine NONMEM submission args
  #  - The main ones of interest are prdefault, tprdefault, and maxlim, which
  #    impact the evaluation of `NM-TRAN`
  #  - Priority: .bbi_args > model yaml > bbi.yaml
  cmd_args <- parse_nmtran_args(
    .mod, .bbi_args = .bbi_args, nmfe_options = bbi_yaml$nmfe_options
  )

  # Check nonmem version
  nm_config <- bbi_yaml$nonmem
  if (!is.null(.nonmem_version)) {
    # check for valid version
    if (!(.nonmem_version %in% names(nm_config))) {
      rlang::abort(
        c(
          "Must specify a valid `.nonmem_version` for run_nmtran.",
          "i" = glue("{bbi_yaml_path} contains the following options:"),
          glue("`{paste(names(nm_config), collapse='`, `')}`")
        )
      )
    }
    default_nm <- nm_config[[.nonmem_version]]
  }else{
    # Look for default nonmem installation
    default_nm <- purrr::keep(nm_config, function(nm_ver){
      !is.null(nm_ver$default) & isTRUE(nm_ver$default)
    })

    if(length(default_nm) > 1){
      nm_vers <- paste(names(default_nm), collapse = ", ")
      rlang::abort(
        c(
          glue("Found multiple default NONMEM versions ({nm_vers}) at `{config_path}`"),
          "i" = "Please ensure only one version is set to the default"
        )
      )
    }else if(length(default_nm) == 1){
      default_nm <- default_nm[[1]]
    }else{
      # If no default, use the last one (likely higher version)
      default_nm <- nm_config[[length(nm_config)]]
    }
  }

  # Set NM-TRAN executable path
  nm_path <- default_nm$home
  nmtran_exe <- file.path(nm_path, "tr", "NMTRAN.exe")

  # Check if nmtran_presort exists
  presort_dir <- file.path(dirname(dirname(nmtran_exe)), "util")
  nmtran_presort_exe <- file.path(presort_dir, "nmtran_presort")
  run_presort <- unname(fs::file_exists(nmtran_presort_exe))

  return(
    list(
      nmtran_exe = nmtran_exe,
      nonmem_version = basename(default_nm$home),
      cmd_args = cmd_args,
      nmtran_presort_exe = if(run_presort) nmtran_presort_exe else NULL
    )
  )
}

#' Execute `NM-TRAN` in a given directory
#'
#' Execute `NM-TRAN` in a given directory. Also runs `nmtran_presort` if an
#' executable is found.
#'
#' @param nmtran_exe Path to `NM-TRAN` executable.
#' @param mod_path Path of a model to evaluate. Should be relative to `dir`.
#' @param cmd_args A character vector of command line arguments for the `NM-TRAN`
#'  execution call
#' @param nmtran_presort_exe Path to `nmtran_presort` executable. Only available
#' for `NONMEM` versions `nm74gf`, `nm74gf_nmfe`, and `nm75`. If provided, will run
#' `nmtran_presort` before running `NM-TRAN`. Set to `NULL` to skip this step.
#' @param dir Directory in which to execute the command.
#'
#' @keywords internal
execute_nmtran <- function(
    nmtran_exe,
    mod_path,
    cmd_args = c("0", "0", "2"),
    nmtran_presort_exe = NULL,
    dir = "."
){
  checkmate::assert_directory_exists(dir)
  checkmate::assert_character(cmd_args, len = 3)

  run_dir <- as.character(fs::path_real(dir))

  # Check if nmtran_presort exists
  run_presort <- !is.null(nmtran_presort_exe)

  # Preprocess with nmtran_presort
  if(run_presort){
    presort.p <- processx::run(
      command = nmtran_presort_exe, wd = run_dir, args = character(),
      stdout = "|", stdin = file.path(run_dir, mod_path),
      stderr_to_stdout = TRUE, error_on_status = FALSE
    )

    presort_status_val <- presort.p$status
    if(is.na(presort_status_val)){
      rlang::abort("nmtran_presort terminated unexpectedly")
    }else if(presort_status_val != 0){
      return(
        list(
          nmtran_model = file.path(run_dir, mod_path),
          run_dir = run_dir,
          status = "nmtran_presort failed. See errors.",
          status_val = presort_status_val,
          output_lines = presort.p$stdout
        )
      )
    }

    # Write the output to tempzzzz1 control stream
    writeLines(presort.p$stdout, con = file.path(run_dir, "tempzzzz1.ctl"))
  }

  # Run NM-TRAN
  stdin_file <- ifelse(run_presort, "tempzzzz1.ctl", mod_path)
  nmtran.p <- processx::run(
    command = nmtran_exe, wd = run_dir, args = cmd_args,
    stdout = "|", stdin = file.path(run_dir, stdin_file),
    stderr_to_stdout = TRUE, error_on_status = FALSE
  )

  # Assign status
  status_val <- nmtran.p$status
  if(is.na(status_val)){
    rlang::abort("NM-TRAN terminated unexpectedly")
  }else if(status_val == 0){
    status <- "NM-TRAN successful"
  }else{
    status <- "NM-TRAN failed. See errors."
  }

  # Tabulate NM-TRAN results
  nmtran_results <- list(
    nmtran_model = stdin_file,
    run_dir = run_dir,
    status = status,
    status_val = status_val,
    output_lines = nmtran.p$stdout
  )

  return(nmtran_results)
}


#' Helper for finding and reading in a `bbi.yaml` file given a `bbi_nonmem_model`
#' object or explicit `.config_path`.
#' @inheritParams run_nmtran
#' @noRd
get_bbi_yaml_path <- function(.mod = NULL, .config_path = NULL){
  if(!is.null(.mod)){
    check_model_object(.mod, "bbi_nonmem_model")
    model_dir <- get_model_working_directory(.mod)
  }
  bbi_yaml_path <- .config_path %||% file.path(model_dir, "bbi.yaml")

  if(!file_exists(bbi_yaml_path)){
    if(is.null(.config_path)){
      msg <- c(
        "No bbi configuration was found in the execution directory.",
        "i" = "Please run `bbi_init()` with the appropriate directory to continue."
      )
    }else{
      msg <- glue("No bbi configuration was found at {.config_path}")
    }
    rlang::abort(msg)
  }

  return(bbi_yaml_path)
}


#' Parse `.bbi_args` and return the three expected `nmfe_options` for `NM-TRAN`
#'  in the correct format
#' @inheritParams run_nmtran
#' @param nmfe_options named list of nmfe options defined in a `bbi.yaml` file.
#' @noRd
parse_nmtran_args <- function(
    .mod,
    .bbi_args = NULL,
    nmfe_options = NULL
){

  nmfe_args_def <- list(prdefault = FALSE, tprdefault = FALSE, maxlim = 2)

  # Combine with any options stored in model yaml, preferring .bbi_args
  .nmfe_args <- parse_args_list(.bbi_args, .mod[[YAML_BBI_ARGS]])

  # Combine with nmfe options stored in bbi.yaml, preferring .nmfe_args
  .nmfe_args <- parse_args_list(.nmfe_args, nmfe_options)

  # Check provided args
  check_bbi_args(.nmfe_args)

  # Combine with and filter to default nmfe_options
  .nmfe_args <- parse_args_list(.nmfe_args, nmfe_args_def)
  .nmfe_args <- .nmfe_args[names(nmfe_args_def)]

  .nmtran_args <- c(
    ifelse(isTRUE(.nmfe_args$prdefault), 1, 0),
    ifelse(isTRUE(.nmfe_args$tprdefault), 1, 0),
    .nmfe_args$maxlim
  )

  return(as.character(.nmtran_args))
}
