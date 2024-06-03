
#' Simulate a `bbi_nonmem_model` object
#'
#' @param .mod A `bbi_nonmem_model` or `bbi_nonmem_summary` object
#' @inheritParams new_sim_model
#' @inheritParams submit_model
#'
#' @details
#' `add_simulation` does the following things:
#'  - Checks that `.mod` was previously executed and tabled out an `.MSF` file
#'  (i.e. `$EST MSFO=1.MSF`).
#'     - **Note:** The `.MSF` file must have an upper case extension, otherwise
#'     it will be cleaned up after submission
#'  - Performs various checks to confirm the status of `.mod` and contents of its
#'  control stream.
#'  - Creates a new `bbi_nmsim_model` object with the following differences from
#'  the original control stream:
#'    - Removes the following record types for simulation: `$EST`, `$COV`,
#'    `$TABLE`, `$SIMULATION`
#'    - Removes PK and prior records: `$PRIOR`, `$THETA/$THETAP/$THETAPV`,
#'    `$OMEGA/$OMEGAP/$OMEGAPD`, `$SIGMA/$SIGMAP/$SIGMAPD`
#'    - Adds a new custom `$SIMULATION` record using user specified values (e.g.
#'     `seed` and `n`). `TRUE=FINAL` is appended to ensure the final values are
#'     used rather than the initial estimates.
#'    - Adds a new `$TABLE` record tabling out simulated values and `.join_col`
#'    column(s)
#'    - Adds a new `$MSFI` record (run with `NOMSFTEST`) pointing to the `.MSF`
#'    file of `.mod`
#'  - Creates a specification file, storing `seeds`, `n`, and other various items
#'  helpful for traceability purposes.
#'  - Submits the model for execution.
#'
#' @seealso nm_join_sim
#'
#' @examples
#' \dontrun{
#'
#' # Add a new simulation and submit for execution
#' add_simulation(
#'   .mod,
#'   n = 500,
#'   .join_col = c("NUM", "ID"),
#'   .mode = "local"
#' )
#'
#' # Optionally read in simulation model object
#' if(has_simulation(.mod)){
#'  .sim <- get_simulation(.mod)
#' }
#' }
#' @name simulate
NULL

#' @describeIn simulate Add a simulation to a `bbi_nonmem_model` object
#' @export
add_simulation <- function(
    .mod,
    n = 200,
    seed = 1234,
    sim_cols = "DV",
    .join_col = "NUM",
    .inherit_tags = TRUE,
    .bbi_args = NULL,
    .mode = getOption("bbr.bbi_exe_mode"),
    ...,
    .overwrite = NULL,
    .config_path = NULL,
    .wait = TRUE
){
  check_model_object(.mod, c(NM_MOD_CLASS, NM_SUM_CLASS))
  if(inherits(.mod, NM_SUM_CLASS)){
    .mod <- read_model(.mod[[ABS_MOD_PATH]])
  }

  .sim_mod <- new_sim_model(
    .mod, n = n, seed = seed,
    .join_col = .join_col, sim_cols = sim_cols,
    .inherit_tags = .inherit_tags, .overwrite = .overwrite
  )

  sim_args <- list(n = n, seed = seed)
  make_sim_spec(.sim_mod, sim_args)

  # Cannot support dry runs since this this function handles model creation as well
  # (at least not without deleting the model)
  submit_model(
    .sim_mod,
    .bbi_args = .bbi_args,
    .mode = .mode,
    ...,
    .overwrite = .overwrite,
    .config_path = .config_path,
    .wait = .wait,
    .dry_run = FALSE
  )
}


#' @describeIn simulate Helper for determining if a `bbi_nonmem_model` has a
#'  simulation attached to it. Returns `TRUE` if a `bbi_nmsim_model`
#'  specification file is found within the output directory of `.mod`, and
#'  `FALSE` otherwise.
#' @export
has_simulation <- function(.mod){
  return(!is.null(get_sim_spec(.mod)))
}


#' @describeIn simulate Read in and return a `bbi_nmsim_model` object stored
#'  within the output directory of `.mod`.
#' @export
get_simulation <- function(.mod){
  check_model_object(.mod, c(NM_MOD_CLASS, NM_SUM_CLASS))
  if(inherits(.mod, NM_SUM_CLASS)){
    .mod <- read_model(.mod[[ABS_MOD_PATH]])
  }

  sim_spec_path <- get_spec_path(.mod)
  if(!fs::file_exists(sim_spec_path)){
    rlang::abort(
      c(
        glue("Model {get_model_id(.mod)} does not have a simulation"),
        glue("Expected to find specification file at {sim_spec_path}")
      )
    )
  }

  sim_spec <- get_sim_spec(.mod)
  sim_path <- fs::path_ext_remove(sim_spec$model_path)
  return(read_model(sim_path))
}

#' Create a simulation model object from an existing model
#'
#' @param .mod a `bbi_nonmem_model` object.
#' @param n Number of simulations/subproblems. Adds `SUBPROBLEMS=n` to a
#'  `$SIMULATION` record.
#' @param seed A seed for simulation. Appended to `$SIMULATION` record.
#' @param sim_cols Character column name(s) defining the simulated values to
#'  table out.
#' @param .join_col Character column name(s) used to join table files post
#'  execution. Gets appended to the generated `$TABLE` record. See
#'  [nm_join_sim()] documentation for details. Defaults to `'NUM'`.
#' @param .sim_dir A directory for holding the new simulation model. Defaults to
#'  the output directory of `.mod`.
#' @param .inherit_tags If `TRUE`, the default, inherit any tags from `.mod`.
#' @inheritParams copy_model_from
#'
#' @details
#' `new_sim_model` does the following things:
#'  - Checks that `.mod` was previously executed and tabled out an `.MSF` file
#'  (i.e. `$EST MSFO=1.MSF`).
#'     - **Note:** The `.MSF` file must have an upper case extension, otherwise
#'     it will be cleaned up after submission
#'  - Performs various checks to confirm the status of `.mod` and contents of its
#'  control stream.
#'  - Creates a new `bbi_nmsim_model` object with the following differences from
#'  the original control stream:
#'    - Removes the following record types for simulation: `$EST`, `$COV`,
#'    `$TABLE`, `$SIMULATION`
#'    - Removes PK and prior records: `$PRIOR`, `$THETA/$THETAP/$THETAPV`,
#'    `$OMEGA/$OMEGAP/$OMEGAPD`, `$SIGMA/$SIGMAP/$SIGMAPD`
#'    - Adds a new custom `$SIMULATION` record using user specified values (e.g.
#'     `seed` and `n`). `TRUE=FINAL` is appended to ensure the final values are
#'     used rather than the initial estimates.
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
#' .sim <- new_sim_model(
#'   .mod,
#'   n = 500, seed = 1234,
#'   .join_col = c("NUM", "ID")
#' )
#'
#' # Store run details in specification file
#' #  - this is why these functions aren't exported
#' make_sim_spec(
#'   .sim,
#'   sim_args = list(n = 500, seed = 1234),
#'  .join_col = c("NUM", "ID")
#' )
#'
#' # Visualize changes
#' model_diff(.sim)
#'
#' # Submit
#' submit_model(.sim)
#' }
#'
#' @return S3 object of class `bbi_nmsim_model`.
#' @keywords internal
new_sim_model <- function(
    .mod,
    n = 200,
    seed = 1234,
    sim_cols = c("DV", "PRED"),
    .join_col = "NUM",
    .sim_dir = get_output_dir(.mod),
    .inherit_tags = TRUE,
    .overwrite = FALSE
){
  check_model_object(.mod, NM_MOD_CLASS)
  check_model_for_sim(.mod, .join_col)

  # Create new simulation model object
  model_dir <- get_model_working_directory(.mod)
  sim_dir_rel <- fs::path_rel(.sim_dir, model_dir)
  output_dir <- file.path(sim_dir_rel, glue("{get_model_id(.mod)}-sim"))

  .sim_mod <- copy_model_from(
    .parent_mod = .mod,
    .new_model = output_dir,
    .add_tags = "SIMULATION",
    .inherit_tags = .inherit_tags,
    .update_model_file = TRUE,
    .overwrite = .overwrite
  )

  .sim_mod[[YAML_MOD_TYPE]] <- "nmsim"
  .sim_mod <- save_model_yaml(.sim_mod)

  # Overwrite problem statement
  prob <- glue("Simulation of model {get_model_id(.mod)}")
  modify_prob_statement(.sim_mod, prob)

  # Overwrite $DATA record (one level deeper)
  based_on_data_path <- get_data_path_from_ctl(.mod, normalize = FALSE)
  data_path_rel <- file.path("..", based_on_data_path)
  modify_data_path_ctl(.sim_mod, data_path_rel)

  # Set up simulation
  setup_sim_run(
    .sim_mod, n = n, seed = seed, sim_cols = sim_cols,
    .join_col = .join_col
  )

  # Return read-in model to get the updated class as part of the model object
  return(read_model(file.path(model_dir, output_dir)))
}


#' Set up `NONMEM` simulation
#'
#' Creates new `$SIMULATION` and `$MSFI` records, and modifies the `$PRED` or
#'  `$ERROR` record (whichever exists) to have the additional line: `REPI=IREP`.
#' @param .mod a `bbi_nonmem_model` or `bbi_nmsim_model` object.
#' @inheritParams new_sim_model
#' @keywords internal
setup_sim_run <- function(
    .mod,
    n = 100,
    seed = 1234,
    sim_cols = c("DV", "PRED"),
    .join_col = c("NUM")
){
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
  join_col_txt <- paste(.join_col, collapse = " ")
  sim_col_txt <- paste(sim_cols, collapse = " ")
  fmt_expr <- "FORMAT=s1PE12.5"
  table_lines <- glue("ONEHEADER NOPRINT NOAPPEND {join_col_txt} {sim_col_txt} {fmt_expr} FILE={table_name}")
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
  msf_path_name <- basename(msf_path)
  if(!fs::file_exists(msf_path)){
    rlang::abort(glue("Could not find referenced MSF path ({msf_path_name}) at `{msf_path}`"))
  }
  # Note: file name must be all caps for now; otherwise it will be git ignored via bbi
  if(msf_path_name != toupper(msf_path_name)){
    rlang::abort(
      c(
        glue("The msf file (`{msf_path_name}`) must have an upper case extension"),
        "Otherwise it will be cleaned up after submission"
      )
    )
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

  # Check submission status - this should be done after all control stream inspections
  # to avoid having to unnecessarily re-run the model with required changes.
  if(!model_is_finished(.mod)){
    rlang::abort(
      glue("`new_sim_model` requires a previously submitted `{NM_MOD_CLASS}` object")
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


#' @describeIn new_sim_model Store simulation run details in a specification
#'  file before submission, which is stored in the output directory of the parent
#'  model.
#'
#' @param .sim a `bbi_nmsim_model` object
#' @inheritParams setup_sim_run
#' @param sim_args named list defining the `seed`, and `n` simulations.
#' @keywords internal
make_sim_spec <- function(.sim, sim_args, .overwrite = FALSE){
  check_model_object(.sim, NMSIM_MOD_CLASS)
  checkmate::assert_list(sim_args)

  # Save spec file to output directory of based_on model
  sim_dir <- get_model_working_directory(.sim)
  json_path <- file.path(sim_dir, "bbr_sim_spec.json")

  if(fs::file_exists(json_path) && isFALSE(.overwrite)){
    rlang::abort(
      c(
        glue("A simulation specification file already exists at `{json_path}`"),
        "i" = "Pass `.overwrite = TRUE` to overwrite."
      )
    )
  }

  sim_spec <- list(
    problem = glue("Simulation of model {basename(get_based_on(.sim))}"),
    seed = sim_args$seed,
    n_sim = sim_args$n,
    model_path = get_model_path(.sim),
    based_on_model_path = get_based_on(.sim),
    based_on_data_path = get_data_path(.sim),
    model_md5 = tools::md5sum(get_model_path(.sim)),
    output_dir = sim_dir
  )
  spec_lst <- c(
    sim_spec = list(sim_spec)
  )

  spec_lst_json <- jsonlite::toJSON(
    spec_lst, pretty = TRUE, simplifyVector = TRUE, null = "null"
  )

  writeLines(spec_lst_json, json_path)
  return(invisible(json_path))
}



