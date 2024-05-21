
#' Create a simulation model object from an existing model
#'
#' @param .mod a `bbi_nonmem_model` object.
#' @param n number of replicates/subproblems. Adds `SUBPROBLEMS=n` to a
#'  `$SIMULATION` record.
#' @param seed a seed for simulation. Appended to `$SIMULATION` record.
#' @param .join_col Character column name(s) used to join table files post
#'  execution. Gets appended to the generated `$TABLE` record. See
#'  [nm_join_sim()] documentation for details. Defaults to `'NUM'`.
#' @param .suffix a suffix for the simulation directory. Will be prefixed by
#'  the model id of `.mod`.
#' @param .inherit_tags If `TRUE`, the default, inherit any tags from `.mod`.
#' @inheritParams copy_model_from
#'
#' @details
#' `new_sim_model` does the following things:
#'  - Checks that `.mod` was previously executed and saved out an `.MSF` file was
#'  generated (i.e. `$EST MSFO=1.MSF`).
#'  - Performs various checks to confirm the status of `.mod` and contents of its
#'  control stream.
#'  - Creates a new `bbi_nmsim_model` object with the following differences from
#'  the original control stream:
#'    - Removes the following record types for simulation: `$EST`, `$COV`,
#'    `$TABLE`, `$SIMULATION`
#'    - Removes PK and prior records: `$PRIOR`, `$THETA/$THETAP/$THETAPV`,
#'    `$OMEGA/$OMEGAP/$OMEGAPD`, `$SIGMA/$SIGMAP/$SIGMAPD`
#'    - Adds a new custom `$SIMULATION` record using user specified values
#'    - Adds a new `$TABLE` record tabling out simulated values and `.join_col`
#'    column(s)
#'    - Adds a new `$MSFI` record (run with `NOMSFTEST`) pointing to the `.MSF`
#'    file of `.mod`
#'
#' @seealso nm_join_sim
#' @examples
#' \dontrun{
#'
#' # Create new `nmsim` model object
#' .sim_mod <- new_sim_model(.mod, n = 500, .join_col = c("NUM", "ID"))
#'
#' # Visualize changes
#' model_diff(.sim_mod)
#'
#' # Submit
#' submit_model(.sim_mod)
#' }
#'
#' @return S3 object of class `bbi_nmsim_model`.
#' @export
new_sim_model <- function(
    .mod,
    n = 200,
    seed = 1234,
    .join_col = "NUM",
    .suffix = "sim",
    .inherit_tags = TRUE,
    .overwrite = FALSE
){
  check_model_object(.mod, NM_MOD_CLASS)
  check_model_for_sim(.mod, .join_col)

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
  setup_sim_run(.sim_mod, n = n, seed = seed, .join_col = .join_col)

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
setup_sim_run <- function(.mod, n = 100, seed = 1234, .join_col = c("NUM")){
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


  ## Add new $SIMULATION record (must be _after_ $ERROR or $PRED, but before $TABLE) ##
  rep_name <- get_sim_replicate_record(.mod)
  sim_lines <- glue("({seed}) SUBPROBLEMS={n} TRUE=FINAL")
  add_new_record(.mod, "simulation", lines = sim_lines, after = rep_name)


  ## Add new $TABLE record with predefined format and columns (including REPI) ##
  table_name <- glue("{get_model_id(get_based_on(.mod)[1])}-sim.tab")
  # TODO: pick either DV or PRED for sim variable (KyleB to look into it)
  join_col_txt <- paste(.join_col, collapse = " ")
  fmt_expr <- "FORMAT=s1PE12.5"
  table_lines <- glue("ONEHEADER NOPRINT NOAPPEND {join_col_txt} DV PRED {fmt_expr} FILE={table_name}")
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
check_model_for_sim <- function(.mod, .join_col){
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

  # ensures only one record ($ERROR or $PRED) exists
  rep_name <- get_sim_replicate_record(.mod)

  # Make sure specified join columns are present in input data
  input_data <- nm_data(.mod) %>% suppressMessages()
  if(!all(.join_col %in% names(input_data))){
    missing_cols <- .join_col[!(.join_col %in% names(input_data))]
    missing_txt <- paste(missing_cols, collapse = ", ")
    rlang::abort(
      c(
        glue("The following .join_col columns were not found in the input data: {missing_txt}"),
        "Check `nm_data(.mod)` to see available columns."
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


