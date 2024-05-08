
#' Create a VPC model object from an existing model
#'
#' @param .mod a `bbr` model object
#' @param n number of replicates.
#' @param seed a seed for sampling the data.
#' @param .suffix a suffix for the VPC simulation directory. Will be prefixed by
#'  the model id of `.mod`.
#' @inheritParams copy_model_from
#'
#' @return S3 object of class `bbi_nmsim_model`.
#' @export
new_sim_model <- function(
    .mod,
    n = 100,
    seed = 1234,
    .suffix = "sim",
    .inherit_tags = TRUE,
    .overwrite = FALSE
){

  check_model_object(.mod, NM_MOD_CLASS)

  # Check submission status
  if(!model_is_finished(.mod)){
    rlang::abort(
      glue("`new_sim_model` requires a previously submitted `{NM_MOD_CLASS}` object")
    )
  }

  # Check the .MSF file in $EST
  # Note: file name must be all caps for now; otherwise it will be git ignored
  msf_path <- get_msf_path(.mod)
  if(is.null(msf_path)){
    rlang::abort(
      glue("`new_sim_model` requires an MSF estimation record option")
    )
  }

  # TODO: other checks:
  # - check that MSFI and SIM weren't already records?
  # - anything else?

  msf_path_name <- basename(msf_path)
  checkmate::assert_true(msf_path_name == toupper(msf_path_name)) # TODO: remove this eventually
  if(!fs::file_exists(msf_path)){
    rlang::abort(glue("Could not find referenced MSF path ({msf_path_name}) at `{msf_path}`"))
  }

  # Create new simulation model object
  model_dir <- get_model_working_directory(.mod)
  sim_dir <- glue("{get_model_id(.mod)}-{.suffix}")

  .sim_run <- copy_model_from(
    .parent_mod = .mod,
    .new_model = sim_dir,
    .add_tags = "SIMULATION",
    .inherit_tags = .inherit_tags,
    .update_model_file = TRUE,
    .overwrite = .overwrite
  )

  .sim_run[[YAML_MOD_TYPE]] <- "nmsim"
  .sim_run <- save_model_yaml(.sim_run)

  # Change problem statement
  prob <- glue("VPC Simulation of model {get_model_id(.mod)}")
  modify_prob_statement(.sim_run, prob)

  # Set up simulation
  setup_sim_run(.sim_run, n = n, seed = seed)

  # Return read-in model to get the updated class as part of the model object
  return(read_model(file.path(model_dir, sim_dir)))
}


#' Set up VPC simulation
#'
#' Creates new `$SIMULATION` and `$MSFI` records, and modifies the `$PRED` or
#'  `$ERROR` record (whichever exists) to have the additional line: `REPI=IREP`.
#' @param .sim_run a `bbi_nmsim_model` object
#' @inheritParams new_sim_model
#' @keywords internal
setup_sim_run <- function(.sim_run, n = 100, seed = 1234){
  checkmate::assert_numeric(n, lower = 1)
  checkmate::assert_numeric(seed)

  # Remove the following record types for simulation:
  # $EST, $COV,  $TABLE, $SIMULATION
  records_rm <- c("estimation", "covariance", "table", "simulation")
  purrr::walk(records_rm, function(rec){
    if(mod_has_record(.sim_run, type = rec)) remove_records(.sim_run, type = rec)
  })

  # Remove PK and prior records:
  # $PRIOR, $THETA/$THETAP/$THETAPV, $OMEGA/$OMEGAP/$OMEGAPD, $SIGMA/$SIGMAP/$SIGMAPD
  records_rm <- c("prior", "theta", "thetapv", "omega", "omegapd", "sigma", "sigmapd")
  purrr::walk(records_rm, function(rec){
    if(mod_has_record(.sim_run, type = rec)) remove_records(.sim_run, type = rec)
  })

  # Check for $PRED or $ERROR records (cant be both)
  replicate_opts <- c(
    "pred" = mod_has_record(.sim_run, "pred"),
    "error" = mod_has_record(.sim_run, "error")
  )
  if(all(replicate_opts)){
    dev_error("setup_sim_run only supports $PRED or $ERROR records. Not both.")
  }
  rep_name <- names(replicate_opts[replicate_opts])

  ## Add new variable assignment to $ERROR _or_ $PRED: `REPI=IREP` ##
  # Store current $error or $pred record
  rep_rec <- get_records(.sim_run, rep_name)
  rep_rec <- rep_rec[[length(rep_rec)]]
  rep_lines <- rep_rec$get_lines()
  rep_lines <- rep_lines[rep_lines != ""]
  # TODO: do we need to check if this was already defined?
  rep_lines <- c(rep_lines, "REPI=IREP", "")

  # Remove old record, and add a new one with the modification
  #  - Use existing record name, dont overwrite
  remove_records(.sim_run, rep_name)
  add_new_record(.sim_run, rep_name, rec_name = NULL, lines = rep_lines, after = NULL)

  ## Add new $SIMULATION record (must be _after_ $ERROR or $PRED, but before $TABLE) ##
  sim_lines <- glue("({seed}) ({seed} NORMAL) SUBPROBLEMS={n} TRUE=FINAL")
  add_new_record(.sim_run, "simulation", lines = sim_lines, after = rep_name)

  ## Add new $TABLE record with predefined format and columns (including REPI) ##
  table_name <- glue("{get_model_id(get_based_on(.sim_run))}sim.tab")
  table_lines <- glue("ONEHEADER REPI DV PRED FILE={table_name}")
  add_new_record(.sim_run, "table", lines = table_lines, after = "simulation")

  ## Add new $MSFI record (end of file) ##
  msf_path <- get_msf_path(read_model(get_based_on(.sim_run)))
  msf_path_rel <- fs::path_rel(msf_path, .sim_run[[ABS_MOD_PATH]])
  msfi_lines <- glue("{msf_path_rel} NOMSFTEST")
  add_new_record(.sim_run, "msfi", lines = msfi_lines, after = NULL)

  return(invisible(.sim_run))
}




read_msf_data <- function(
    .sim_run,
    header = TRUE,
    ...,
    simplify = TRUE,
    table_start_pattern="^TABLE NO"
) {
  # Get simulation table path (probably dont want to use based_on for this)
  sim_tab <- glue("{get_model_id(get_based_on(.sim_run))}sim.tab")
  sim_tab_path <- file.path(.sim_run[[ABS_MOD_PATH]], sim_tab)

  # TODO: multiple DV cols
  file_data <- readLines(sim_tab_path)
  new_tables <- grep(x = file_data, pattern = table_start_pattern)
  # make unique for duplicated table names (simulation does)
  file_data[new_tables] <- make.unique(file_data[new_tables])


  ret <- list()
  for (idx in seq_along(new_tables)) {
    start_line <- new_tables[idx] + 1
    end_line <-
      if (idx == length(new_tables)) {
        length(file_data)
      } else {
        new_tables[idx + 1] - 1
      }
    current_table_name <- file_data[new_tables[idx]]

    tmptab <- read.table(
      textConnection(file_data[start_line:end_line]),
      header = header
    )

    # Scan to ensure no remaining column headers or other text pollution
    # Define the regular expression pattern
    pattern <- "^[[:space:]]*([[:alpha:]].*)"

    # Find the indices of rows that match the pattern
    indices <- which(!grepl(pattern, tmptab[[1]]))
    tmptab <- tmptab[indices,]
    tmptab <- as.data.frame(sapply(tmptab, as.numeric))
    ret[[current_table_name]] <- as_tibble(tmptab)
  }

  if (isTRUE(simplify)) {
    ret <- purrr::list_rbind(ret)
  }
}
