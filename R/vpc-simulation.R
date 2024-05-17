
#' Create a simulation model object from an existing model
#'
#' @param .mod a `bbi_nonmem_model` object.
#' @param n number of replicates/subproblems. Adds `SUBPROBLEMS=n` to a
#'  `$SIMULATION` record.
#' @param seed a seed for simulation. Appended to `$SIMULATION` record.
#' @param replicate_var a variable name for storing each iteration. Cannot match
#'  a variable previously defined in `$PRED` or `$ERROR`.
#' @param .suffix a suffix for the simulation directory. Will be prefixed by
#'  the model id of `.mod`.
#' @inheritParams copy_model_from
#'
#' @return S3 object of class `bbi_nmsim_model`.
#' @export
new_sim_model <- function(
    .mod,
    n = 100,
    seed = 1234,
    replicate_var = "REPI",
    .suffix = "sim",
    .inherit_tags = TRUE,
    .overwrite = FALSE
){
  check_model_object(.mod, NM_MOD_CLASS)
  check_model_for_sim(.mod, replicate_var = replicate_var)

  # Create new simulation model object
  model_dir <- get_model_working_directory(.mod)
  sim_dir <- glue("{get_model_id(.mod)}-{.suffix}")

  .sim_mod <- copy_model_from(
    .parent_mod = .mod,
    .new_model = sim_dir,
    .add_tags = "SIMULATION",
    .inherit_tags = .inherit_tags,
    .update_model_file = TRUE,
    .overwrite = .overwrite
  )

  .sim_mod[[YAML_MOD_TYPE]] <- "nmsim"
  .sim_mod <- save_model_yaml(.sim_mod)

  # Change problem statement
  prob <- glue("Simulation of model {get_model_id(.mod)}")
  modify_prob_statement(.sim_mod, prob)

  # Set up simulation
  setup_sim_run(.sim_mod, n = n, seed = seed, replicate_var = replicate_var)

  # Return read-in model to get the updated class as part of the model object
  return(read_model(file.path(model_dir, sim_dir)))
}


#' Set up `NONMEM` simulation
#'
#' Creates new `$SIMULATION` and `$MSFI` records, and modifies the `$PRED` or
#'  `$ERROR` record (whichever exists) to have the additional line: `REPI=IREP`.
#' @param .mod a `bbi_nonmem_model` or `bbi_nmsim_model` object.
#' @inheritParams new_sim_model
#' @keywords internal
setup_sim_run <- function(.mod, n = 100, seed = 1234, replicate_var = "REPI"){
  check_model_object(.mod, c(NM_MOD_CLASS, NMSIM_MOD_CLASS))
  checkmate::assert_numeric(n, lower = 1)
  checkmate::assert_numeric(seed)

  # Remove the following record types for simulation:
  # $EST, $COV,  $TABLE, $SIMULATION
  records_rm <- c("estimation", "covariance", "table", "simulation")
  purrr::walk(records_rm, function(rec){
    if(mod_has_record(.mod, type = rec)) remove_records(.mod, type = rec)
  })

  # Remove PK and prior records:
  # $PRIOR, $THETA/$THETAP/$THETAPV, $OMEGA/$OMEGAP/$OMEGAPD, $SIGMA/$SIGMAP/$SIGMAPD
  records_rm <- c("prior", "theta", "thetapv", "omega", "omegapd", "sigma", "sigmapd")
  purrr::walk(records_rm, function(rec){
    if(mod_has_record(.mod, type = rec)) remove_records(.mod, type = rec)
  })


  ## Add new variable assignment to $ERROR _or_ $PRED: `REPI=IREP` ##
  # Store current $error or $pred record
  rep_name <- get_sim_replicate_record(.mod)
  rep_lines <- get_records(.mod, rep_name, get_lines = TRUE)[[1]]
  rep_lines <- rep_lines[rep_lines != ""]
  rep_lines <- c(rep_lines, glue("{replicate_var}=IREP"), "")

  # Remove old record, and add a new one with the modification
  #  - Use existing record name, dont overwrite
  #  - easiest way to amend an existing record
  remove_records(.mod, rep_name)
  add_new_record(.mod, rep_name, rec_name = NULL, lines = rep_lines, after = NULL)


  ## Add new $SIMULATION record (must be _after_ $ERROR or $PRED, but before $TABLE) ##
  sim_lines <- glue("({seed}) SUBPROBLEMS={n} TRUE=FINAL")
  add_new_record(.mod, "simulation", lines = sim_lines, after = rep_name)


  ## Add new $TABLE record with predefined format and columns (including REPI) ##
  table_name <- glue("{get_model_id(get_based_on(.mod)[1])}-sim.tab")
  # TODO: DV automatically gets added in testing, is this always the case?
  # - i.e. adding DV causes duplicate DV columns
  # TODO: We likely want handling for `NUM` column, as this assumes `NUM` is part
  # of your input data (and we cant rely on that assumption)
  table_lines <- glue("ONEHEADER NOPRINT NOAPPEND {replicate_var} NUM DV PRED FILE={table_name}")
  add_new_record(.mod, "table", lines = table_lines, after = "simulation")


  ## Add new $MSFI record (end of file) ##
  msf_path <- get_msf_path(read_model(get_based_on(.mod)[1]))
  msf_path_rel <- fs::path_rel(msf_path, .mod[[ABS_MOD_PATH]])
  msfi_lines <- glue("{msf_path_rel} NOMSFTEST")
  add_new_record(.mod, "msfi", lines = msfi_lines, after = NULL)

  return(invisible(.mod))
}



#' Check that a `bbr` model can be used for simulation
#'
#' Checks that a `bbr` model object is in the correct format to generate a
#'  `bbi_nmsim_model` object.
#' @inheritParams new_sim_model
#' @returns `TRUE` invisibly
#' @keywords internal
check_model_for_sim <- function(.mod, replicate_var = "REPI"){
  # Check submission status
  if(!model_is_finished(.mod)){
    rlang::abort(
      glue("`new_sim_model` requires a previously submitted `{NM_MOD_CLASS}` object")
    )
  }

  # Check the .MSF file in $EST
  msf_path <- get_msf_path(.mod)
  if(is.null(msf_path)){
    # These symbols are not typical in NONMEM control streams
    mod_id_sanit <- gsub("(-|_)", "", get_model_id(.mod))
    rlang::abort(
      c(
        glue("`new_sim_model` requires an MSF estimation record option."),
        glue("e.g., `$EST MSFO={mod_id_sanit}.MSF`")
      )
    )
  }

  # Check .MSF file exists
  # Note: file name must be all caps for now; otherwise it will be git ignored
  msf_path_name <- basename(msf_path)
  checkmate::assert_true(msf_path_name == toupper(msf_path_name)) # TODO: remove this eventually
  if(!fs::file_exists(msf_path)){
    rlang::abort(glue("Could not find referenced MSF path ({msf_path_name}) at `{msf_path}`"))
  }

  # Check that `REPI=IREP` isnt already defined in $ERROR or $PRED
  rep_name <- get_sim_replicate_record(.mod) # ensures only one record exists
  rep_lines <- get_records(.mod, rep_name, get_lines = TRUE)[[1]]

  replicate_pattern <- glue("(^|\\s){replicate_var}\\s*=") # e.g.,`REPI=[x]` (w/wo spaces)
  if(any(grepl(replicate_pattern, rep_lines))){
    issue_lines <- rep_lines[grepl(replicate_pattern, rep_lines)]
    issue_lines_txt <- paste(issue_lines, collapse = "\n")

    rlang::abort(
      c(
        glue("Issue with {rep_name} record: `{replicate_var}=[x]` already defined."),
        paste0("Problematic line(s):\n", issue_lines_txt, "\n"),
        paste0("Problematic record:\n", get_records(.mod, rep_name)[[1]]$format())
      )
    )
  }

  # TODO: other checks:
  # - check that MSFI and SIM weren't already records?
  # - anything else?

  return(invisible(TRUE))
}


#' Determine whether `NONMEM` control stream is using a `$PRED` or `$ERROR` record
#' @inheritParams setup_sim_run
#' @returns an `nmrec` record type name
#' @keywords internal
get_sim_replicate_record <- function(.mod){
  replicate_opts <- c(
    "pred" = mod_has_record(.mod, "pred"),
    "error" = mod_has_record(.mod, "error")
  )

  if(!any(replicate_opts)){
    rlang::abort("This function requires either a $PRED or $ERROR record")
  }

  if(all(replicate_opts)){
    dev_error("This function only supports $PRED or $ERROR records. Not both.")
  }

  rep_name <- names(replicate_opts[replicate_opts])

  # Confirm only one occurrence of $PRED or ERROR
  if(length(get_records(.mod, rep_name)) > 1){
    rlang::abort("Found multiple {rep_name} records. Can only have one.")
  }

  return(rep_name)
}


