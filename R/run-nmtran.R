
#' Run NMTRAN on a model object
#'
#' @param .mod a `bbr` model object
#' @param .config_path Path to a bbi configuration file. If `NULL`, the
#'   default, will attempt to use a `bbi.yaml` in the same directory as the
#'   model.
#' @param nmtran_exe Path to an `NMTRAN` executable. If `NULL`, will look for a
#'   `bbi.yaml` file in the same directory as the model.
#' @param delete_on_exit Logical. If `FALSE`, don't delete the temporary folder
#'   containing the `NMTRAN` run.
#' @param ... additional arguments passed to `system()`
#'
#' @export
run_nmtran <- function(
    .mod,
    .config_path = NULL,
    nmtran_exe = NULL,
    delete_on_exit = TRUE,
    ...
){
  nmtran_exe <- locate_nmtran(.mod, .config_path, nmtran_exe)
  nm_ver <- attr(nmtran_exe, "nonmem_version")

  mod_path <- get_model_path(.mod)
  data_path <- get_data_path_from_ctl(.mod)

  # make temporary directory in current directory
  mod_name <- fs::path_ext_remove(basename(mod_path))
  tempdir0 <- paste0("nmtran_", mod_name, "_", basename(tempdir()))
  dir.create(tempdir0)
  if(isTRUE(delete_on_exit)){
    on.exit(unlink(tempdir0, recursive = TRUE, force = TRUE))
  }

  # copy model and dataset
  file.copy(mod_path, tempdir0)
  file.copy(data_path, tempdir0)

  # overwrite $DATA of new model
  modify_data_path_ctl(
    mod_path = file.path(tempdir0, basename(mod_path)),
    data_path = basename(data_path)
  )

  # Get command & append the control file name
  cmd <- stringr::str_glue(nmtran_exe, .envir = list(ctl_name = basename(mod_path)), .na = NULL)
  cmd <- paste(cmd, "<", basename(mod_path))

  # Run NMTRAN
  if(!is.null(nm_ver)){
    message(glue("Running NMTRAN with NONMEM version `{nm_ver}`"))
  }
  system_nm(cmd, dir = tempdir0, wait = TRUE, ...)

  if(isFALSE(delete_on_exit)){
    return(tempdir0)
  }
}


#' Search for and validate existence of an `NMTRAN` executable
#'
#' If `nmtran_exe = NULL`, this will look for a `bbi.yaml` file in the same
#' directory as the model.
#'
#' @inheritParams run_nmtran
#'
#' @noRd
locate_nmtran <- function(.mod, .config_path = NULL, nmtran_exe = NULL){

  if(is.null(nmtran_exe)){
    model_dir <- get_model_working_directory(.mod)
    config_path <- .config_path %||% file.path(model_dir, "bbi.yaml")

    if(!file_exists(config_path)){
      stop(paste("No bbi configuration was found in the execution directory.",
                 "Please run `bbi_init()` with the appropriate directory to continue."))
    }

    if (!is.null(.config_path)) {
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
    nmtran_exe <- file.path(nm_path, "tr", "NMTRAN.exe")

    # If executable found via bbi.yaml, append NONMEM version as attribute
    attr(nmtran_exe, "nonmem_version") <- basename(default_nm$home)
  }

  if(!file_exists(nmtran_exe)){
    stop(glue("Could not find an NMTRAN executable at `{nmtran_exe}`"))
  }

  return(nmtran_exe)
}


#' Wrapper for `system()`, meant to allow windows operating systems
#'
#' @param cmd System command
#' @param ... additional arguments passed to `system()` or `shell()`
#'
#' @details
#' Taken from `NMproject`
#'
#' @noRd
system_nm_default <- function(cmd, ...) {
  if (.Platform$OS.type == "windows") {
    local_env_vars <- Sys.getenv()
    stdout_unit_vars <- local_env_vars[grepl("STDOUT_UNIT|STDERR_UNIT", names(local_env_vars))]
    for (i in seq_along(stdout_unit_vars)) {
      Sys.unsetenv(names(stdout_unit_vars)[i])
    }
    on.exit({
      if (length(stdout_unit_vars) > 0) {
        do.call(Sys.setenv, as.list(stdout_unit_vars))
      }
    })
    args <- list(...)
    if (!"wait" %in% names(args)) wait <- FALSE else wait <- args$wait
    if (wait == FALSE) {
      shell(paste("START CMD /C", cmd), ...)
    } else {
      shell(cmd, ...)
    }
  } else {
    system(cmd, ...)
  }
}

#' Run a system command in a given directory
#'
#' @details
#' Taken from `NMproject`
#'
#' @inheritParams system_nm_default
#' @param dir Directory in which to execute the command
#'
#' @noRd
system_nm <- function(cmd, dir = NULL, ...) {
  if (is.null(dir) || !file.exists(dir)) dir <- "."
  if (file.exists(dir)) {
    currentwd <- getwd()
    setwd(dir)
    on.exit(setwd(currentwd))
  } else {
    stop(paste0("Directory \"", dir, "\" doesn't exist."))
  }

  system_nm_default(cmd, ...)
}

#' Get the specified data path from a control stream file
#'
#' @param .mod a `bbr` model object
#'
#' @noRd
get_data_path_from_ctl <- function(.mod){
  mod_path <- get_model_path(.mod)
  ctl <- nmrec::read_ctl(mod_path)
  data_rec <- nmrec::select_records(ctl, "data")[[1]]
  data_path <- nmrec::get_record_option(data_rec, "filename")$value

  data_path_norm <- fs::path_norm(file.path(mod_path, data_path))

  if(!fs::file_exists(data_path_norm)){
    stop(glue("Could not find data at {data_path_norm}"))
  }

  return(data_path_norm)
}

#' Modify the specified data path in a control stream file
#' @param mod_path Path to a control stream file
#' @param data_path Data path to set in a `$DATA` record.
#'
#' @noRd
modify_data_path_ctl <- function(mod_path, data_path){
  # Get data record
  ctl <- nmrec::read_ctl(mod_path)
  data_rec <- nmrec::select_records(ctl, "data")[[1]]
  data_rec$parse()

  # Overwrite 'filename' option
  # TODO: confirm this works with .mod extensions
  data_rec$values <- purrr::map(data_rec$values, function(data_opt){
    if(inherits(data_opt, "nmrec_option_pos") && data_opt$name == "filename"){
      data_opt$value <- data_path
    }
    data_opt
  })

  # Write out modified ctl
  nmrec::write_ctl(ctl, mod_path)
}



#' Runs `run_nmtran` on two models, and compares the output
#'
#' Developer tool for comparing different NONMEM control stream configurations.
#'
#' @details
#' Say you wanted to test whether diagonal matrices could specify standard
#' deviation for one value, and variance for another
#'
#' The **reference model** would have this block:
#' ```r
#' $OMEGA
#' 0.05 STANDARD   ; iiv CL
#' 0.2     ; iiv V2
#' ```
#'
#' The **new model** would have this block:
#' ```r
#' $OMEGA
#' 0.05 STANDARD   ; iiv CL
#' 0.2 VAR    ; iiv V2
#' ```
#'
#' Comparing the two (see below), we find no differences. This means that adding
#' `VAR` to the second ETA value had no impact, and the two models would evaluate
#' the same.
#' ```r
#' > compare_nmtran(MOD1, MOD_COMPARE)
#' Running NMTRAN with NONMEM version `nm75`
#'
#' No differences found
#' character(0)
#' ```
#'
#' @keywords internal
compare_nmtran <- function(
    .mod,
    .mod_compare,
    .config_path = NULL,
    nmtran_exe = NULL
){
  nmtran_exe <- locate_nmtran(.mod, .config_path, nmtran_exe)
  nmtran_exe2 <- locate_nmtran(.mod_compare, .config_path, nmtran_exe)
  if(nmtran_exe != nmtran_exe2){
    rlang::warn(
      c(
        "!" = "Found two separate NMTRAN executables:",
        " " = paste("-", nmtran_exe),
        " " = paste("-", nmtran_exe2),
        "i" = "Defaulting to the first one"
      )
    )
  }

  # This function is used to remove problem statement differences introduced
  # via `copy_model_from()`
  empty_prob_statement <- function(.mod){
    mod_new <- copy_model_from(.mod, paste0(get_model_id(.mod), "_no_prob"))
    mod_path <- get_model_path(mod_new)
    ctl <- nmrec::read_ctl(mod_path)
    prob_rec <- nmrec::select_records(ctl, "prob")[[1]]
    prob_rec$parse()

    # Overwrite 'text' option
    prob_rec$values <- purrr::map(prob_rec$values, function(prob_rec){
      if(inherits(prob_rec, "nmrec_option_pos") && prob_rec$name == "text"){
        prob_rec$value <- ""
      }
      prob_rec
    })

    # Write out modified ctl
    nmrec::write_ctl(ctl, mod_path)
    return(mod_new)
  }

  # Run NMTRAN on each model (only message once)
  mod_no_prob <- empty_prob_statement(.mod)
  compare_no_prob <- empty_prob_statement(.mod_compare)
  on.exit(
    delete_models(
      list(mod_no_prob, compare_no_prob), .tags = NULL, .force = TRUE
    ) %>% suppressMessages(),
    add = TRUE
  )

  # Run NMTRAN on each model
  nmtran_mod <- run_nmtran(
    mod_no_prob,
    delete_on_exit = FALSE, intern = TRUE
  )
  nmtran_compare <- run_nmtran(
    compare_no_prob,
    delete_on_exit = FALSE, intern = TRUE
  ) %>% suppressMessages()

  # Force delete folders at the end
  on.exit(unlink(nmtran_mod, recursive = TRUE, force = TRUE), add = TRUE)
  on.exit(unlink(nmtran_compare, recursive = TRUE, force = TRUE), add = TRUE)

  # Compare FCON files
  nmtran_mod_fcon <- file.path(nmtran_mod, "FCON")
  nmtran_compare_fcon <- file.path(nmtran_compare, "FCON")
  cmd <- paste("cmp", nmtran_mod_fcon, nmtran_compare_fcon)

  # Warnings occur when files are different
  output <- system_nm(cmd, intern = TRUE, input = tempdir()) %>%
    suppressWarnings()

  if(length(output) == 0){
    message("\nNo differences found")
  }else{
    message("\nModels are not equivalent")
  }

  return(output)
}

