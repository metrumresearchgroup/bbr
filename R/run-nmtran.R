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
#' Note that `nmtran_presort` is run ahead of `NM-TRAN` for `NONMEM 7.4` and later
#'  - `nmtran_presort` is a supplementary utility that preprocesses the control
#'     stream to ensure it is in the correct format for `NM-TRAN`.
#'
#' @param .mod A `bbi_nonmem_model` object.
#' @param .bbi_args A named list specifying arguments to pass to `NM-TRAN`.
#'   Similar to the `.bbi_args` argument defined in [submit_model()], though here
#'   only `prdefault`, `tprdefault`, and `maxlim` arguments are passed to `NM-TRAN`.
#'   `nm_version` is also supported and specifies which `NM-TRAN` executable to use.
#'   See [print_bbi_args()] for more details.
#' @inheritParams submit_model
#' @param clean Logical (`T`/`F`). If `FALSE`, don't delete the temporary directory
#'   containing the `NM-TRAN` run.
#' @param run_dir Directory to run `NM-TRAN` in. Only relevant if `clean = FALSE`.
#'
#' @examples
#' \dontrun{
#'
#' mod <- read_model(file.path(MODEL_DIR, 1))
#' run_nmtran(mod, .bbi_args = list(nm_version = "nm74gf"))
#'
#' # Save the run directory for manual inspection
#' run_nmtran(mod, clean = FALSE, run_dir = getwd())
#'
#' }
#'
#' @return An S3 object of class `nmtran_process`
#' @export
run_nmtran <- function(
    .mod,
    .bbi_args = NULL,
    .config_path = NULL,
    run_dir = tempdir(),
    clean = TRUE
){
  check_model_object(.mod, "bbi_nonmem_model")

  # Capture NONMEM/NM-TRAN options and format `NM-TRAN` args
  nmtran_specs <- nmtran_setup(
    .mod, .bbi_args = .bbi_args, .config_path = .config_path
  )

  mod_path <- get_model_path(.mod)
  data_path <- get_data_path_from_ctl(.mod)

  # NM-TRAN run directory
  temp_folder <- withr::local_tempdir(
    pattern = paste0("nmtran-mod_", get_model_id(.mod), "-"),
    tmpdir = run_dir, clean = clean
  )

  # Copy model
  fs::file_copy(mod_path, temp_folder)
  nmtran_mod <- new_model(file.path(temp_folder, basename(mod_path)))

  # Copy dataset & overwrite $DATA record of new model
  #  - New data path is used to avoid any special characters that require quoting
  if(fs::file_exists(data_path)){
    new_data_path <- file.path(temp_folder, "data.csv")
    fs::file_copy(data_path, new_data_path)
    # overwrite $DATA record of new model
    modify_data_path_ctl(nmtran_mod, data_path = basename(new_data_path))
  }else{
    cli::cli_abort("Could not find data at `{.file {data_path}}`")
  }

  # Run NM-TRAN
  nmtran_results <- c(
    list(
      nonmem_version = nmtran_specs$nonmem_version,
      nmtran_exe = nmtran_specs$nmtran_exe,
      nmtran_presort_exe = nmtran_specs$nmtran_presort_exe,
      absolute_model_path = mod_path,
      args = nmtran_specs$cmd_args
    ),
    execute_nmtran(
      nmtran_specs$nmtran_exe,
      mod_path = mod_path,
      cmd_args = unname(nmtran_specs$cmd_args),
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
#' @param .nonmem_version Character scalar for default version of NONMEM to use.
#'   If left `NULL`, will look for the default version specified in the provided
#'   `bbi` configuration file.
#'
#' @keywords internal
nmtran_setup <- function(
    .mod,
    .bbi_args = NULL,
    .nonmem_version = .bbi_args[["nm_version"]],
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
  if(is.null(nm_config)){
    cli::cli_abort(
      c(
        "Did not find required NONMEM information in configuration file: {.path {bbi_yaml_path}}",
        "i" = "Please see {.fun bbr::bbi_init} for additional details"
      )
    )
  }
  if (!is.null(.nonmem_version)) {
    # check for valid version
    if (!(.nonmem_version %in% names(nm_config))) {
      cli::cli_abort(
        c(
          "Must specify a valid {.var nm_version} for {.fun bbr::run_nmtran}.",
          "i" = "{.path {bbi_yaml_path}} contains the following options:",
          "{names(nm_config)}",
          "*" = "e.g., {.code run_nmtran(mod, .bbi_args = list(nm_version = '{names(nm_config)[1]}'))}"
        )
      )
    }
    default_nm <- nm_config[[.nonmem_version]]
  }else{
    # Look for default nonmem installation
    default_nm <- purrr::keep(nm_config, function(nm_ver){
      !is.null(nm_ver$default) && isTRUE(nm_ver$default)
    })

    if(length(default_nm) > 1){
      nm_vers <- paste(names(default_nm), collapse = ", ")
      cli::cli_abort(
        c(
          "Found multiple default NONMEM versions ({nm_vers}) at {.path {bbi_yaml_path}}",
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
  nmtran_presort_exe <- file.path(nm_path, "util", "nmtran_presort")
  if(ON_WINDOWS) nmtran_presort_exe <- paste0(nmtran_presort_exe, ".exe")
  run_presort <- unname(fs::file_exists(nmtran_presort_exe))

  return(
    list(
      nmtran_exe = nmtran_exe,
      nonmem_version = basename(nm_path),
      cmd_args = cmd_args,
      nmtran_presort_exe = if(run_presort) nmtran_presort_exe else NULL
    )
  )
}

#' Execute `NM-TRAN` in a given directory
#'
#' Execute `NM-TRAN` in a given directory. Also runs `nmtran_presort` if an
#' executable is found. This assumes the model at `mod_path` has already been
#' copied to the top-level of `dir`.
#'
#' @param nmtran_exe Path to `NM-TRAN` executable.
#' @param mod_path Path of a model to evaluate. Should be relative to `dir`.
#' @param cmd_args A character vector of command line arguments for the
#'   `NM-TRAN` execution call
#' @param nmtran_presort_exe Path to `nmtran_presort` executable. If provided,
#'   will run `nmtran_presort` ahead of `NM-TRAN` for `NONMEM 7.4` and later.
#'   Set to `NULL` to skip this step.
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
  nmtran_input <- mod_path_new <- file.path(run_dir, basename(mod_path))

  # This should only be possible via manual intervention
  if(!fs::file_exists(mod_path_new)){
    cli::cli_abort("Could not find model at {.path {mod_path_new}}")
  }

  # Preprocess with nmtran_presort
  if(!is.null(nmtran_presort_exe)){
    # NM-TRAN input is now output from nmtran_presort
    nmtran_input <- file.path(
      paste0(fs::path_ext_remove(mod_path_new), "_presort.", fs::path_ext(mod_path_new))
    )

    processx::run(
      command = nmtran_presort_exe,
      stdin = mod_path_new,
      stdout = nmtran_input,
      wd = run_dir,
      error_on_status = TRUE
    )

    # In our Window testing, nmtran_presort and NM-TRAN output write stdout to
    # fort.6 instead of the locations specified in the processx::run() calls.
    # See https://github.com/metrumresearchgroup/bbr/pull/705 for more discussion.
    # - Overwrite stdout with contents of fort.6 if on windows
    if(ON_WINDOWS){
      fort6_path <- file.path(run_dir, "fort.6")
      if(file.exists(fort6_path)){
        fs::file_copy(fort6_path, nmtran_input, overwrite = TRUE)
      }
    }
  }

  # Run NM-TRAN
  nmtran.p <- processx::run(
    command = nmtran_exe,
    args = cmd_args,
    stdin = nmtran_input,
    stdout = "|",
    wd = run_dir,
    stderr_to_stdout = TRUE,
    error_on_status = FALSE
  )

  # Assign status
  status_val <- nmtran.p$status
  if(is.na(status_val)){
    cli::cli_abort("NM-TRAN terminated unexpectedly")
  }else if(status_val == 0){
    status <- "Finished Running"
  }else{
    status <- "Failed. See errors."
  }

  # Overwrite stdout with contents of fort.6 if on windows
  if(ON_WINDOWS && file.exists(fort6_path)){
    output_lines <- readLines(fort6_path)
  }else{
    output_lines <- nmtran.p$stdout
  }

  # Tabulate NM-TRAN results
  nmtran_results <- list(
    nmtran_model = basename(nmtran_input),
    run_dir = run_dir,
    status = status,
    status_val = status_val,
    output = output_lines
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
      cli::cli_abort(
        c(
          "No bbi configuration was found in the execution directory.",
          "i" = "Please run {.fun bbr::bbi_init} with the appropriate directory to continue."
        )
      )
    }else{
      cli::cli_abort("No bbi configuration was found at {.path {(.config_path)}}")
    }
  }

  return(bbi_yaml_path)
}


#' Parse `.bbi_args` and return the three expected `nmfe_options` for `NM-TRAN`
#'  in the correct format
#' @inheritParams run_nmtran
#' @param nmfe_options named list of nmfe options defined in a `bbi.yaml` file.
#'
#' @details
#'
#' Combines NONMEM submission args and consolidates to `NMFE` arguments only
#'  - The arguments of interest are `prdefault`, `tprdefault`, and `maxlim`,
#' which impact the evaluation of `NM-TRAN`. Priority is `.bbi_args` > model
#' yaml > `bbi.yaml`
#'  - Note: `run_nmtran()` considers only the first three `NM-TRAN` arguments
#'  (`prdefault`, `tprdefault`, and `maxlim`), but, starting with `NONMEM 7.5`,
#'  nmfe passes a fourth argument (`do2test`) to NM-TRAN.
#'      - `bbi` _doesn't_ expose nmfe's `-do2test` argument, so in the `bbi`/`bbr`
#'      context this call always passes `"0"` to `NM-TRAN` for the `do2test`
#'      value. If that first call exits with a status of `9`, nmfe does a follow-up
#'      call that drops the fourth argument (`do2test`).
#'      - Given the above, `parse_nmtran_args()` does not support the additional
#'      `do2test` argument for `NONMEM 7.5`. This should be revisited if
#'      `run_nmtran()` is called within `submit_model()` as a method of validating
#'      the control stream ahead of model execution. See additional discussion at
#'      https://github.com/metrumresearchgroup/bbr/pull/705.
#'
#' @keywords internal
#' @return a named character vector
parse_nmtran_args <- function(
    .mod,
    .bbi_args = NULL,
    nmfe_options = NULL
){
  check_bbi_args(.bbi_args)

  # These are the default NMFE options that are passed to NM-TRAN
  #  - They should always be passed (in the correct order), so set the defaults
  #    here and merge with other .bbi_args to ensure these are passed to NM-TRAN
  nmfe_args_def <- list(prdefault = FALSE, tprdefault = FALSE, maxlim = 2)

  # Combine with any options stored in model yaml, preferring .bbi_args
  .nmfe_args <- parse_args_list(.bbi_args, .mod[[YAML_BBI_ARGS]])

  # Mimic submit_model behavior: a FALSE value for .bbi_arg has no effect
  # - Filter these out from .nmfe_args
  if(!is.null(.nmfe_args)){
    .nmfe_args <- purrr::imap(.nmfe_args, function(val, nmfe_arg){
      if(isFALSE(val)) return(NULL) else return(val)
    }) %>% purrr::compact()
  }

  # Combine with nmfe options stored in bbi.yaml, preferring .bbi_args
  .nmfe_args <- parse_args_list(.nmfe_args, nmfe_options)

  # Check all provided args
  check_bbi_args(.nmfe_args)

  # Combine with and filter to default nmfe_options
  .nmfe_args <- parse_args_list(.nmfe_args, nmfe_args_def)
  .nmfe_args <- .nmfe_args[names(nmfe_args_def)]

  .nmtran_args <- c(
    if(isTRUE(.nmfe_args$prdefault)) 1 else 0,
    if(isTRUE(.nmfe_args$tprdefault)) 1 else 0,
    .nmfe_args$maxlim
  ) %>% as.character() %>%
    stats::setNames(names(nmfe_args_def))

  return(.nmtran_args)
}
