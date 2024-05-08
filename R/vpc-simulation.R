
#' Create a VPC model object from an existing model
#'
#' @param .mod a `bbr` model object
#' @param n number of replicates.
#' @param seed a seed for sampling the data.
#' @param .suffix a suffix for the VPC simulation directory. Will be prefixed by
#'  the model id of `.mod`.
#' @inheritParams copy_model_from
#'
#' @return S3 object of class `bbi_vpc_model`.
#' @export
new_vpc_sim <- function(
    .mod,
    n = 100,
    seed = 1234,
    .suffix = "vpc",
    .inherit_tags = TRUE,
    .overwrite = FALSE
){

  check_model_object(.mod, NM_MOD_CLASS)

  # Check submission status
  if(!model_is_finished(.mod)){
    rlang::abort(
      glue("`new_vpc_sim` requires a previously submitted `{NM_MOD_CLASS}` object")
    )
  }

  # Check the .MSF file in $EST
  # Note: file name must be all caps for now; otherwise it will be git ignored
  msf_path <- get_msf_path(.mod)
  if(is.null(msf_path)){
    rlang::abort(
      glue("`new_vpc_sim` requires an MSF estimation record option")
    )
  }

  msf_path_name <- basename(msf_path)
  checkmate::assert_true(msf_path_name == toupper(msf_path_name)) # TODO: remove this eventually
  if(!fs::file_exists(msf_path)){
    rlang::abort(glue("Could not find referenced MSF path ({msf_path_name}) at `{msf_path}`"))
  }

  # Create new vpc model object
  model_dir <- get_model_working_directory(.mod)
  vpc_dir <- glue("{get_model_id(.mod)}-{.suffix}")

  vpc_sim <- copy_model_from(
    .parent_mod = .mod,
    .new_model = vpc_dir,
    .add_tags = "VPC_SIMULATION",
    .inherit_tags = .inherit_tags,
    .update_model_file = TRUE,
    .overwrite = .overwrite
  )

  vpc_sim[[YAML_MOD_TYPE]] <- "nmvpc"
  vpc_sim <- save_model_yaml(vpc_sim)

  # Change problem statement
  prob <- glue("VPC Simulation of model {get_model_id(.mod)}")
  modify_prob_statement(vpc_sim, prob)

  # Remove the following record types for simulation:
  # $EST, $COV,  $TABLE, $SIMULATION
  records_rm <- c("estimation", "covariance", "table", "simulation")
  purrr::walk(records_rm, function(rec){
    if(mod_has_record(vpc_sim, type = rec)) remove_records(vpc_sim, type = rec)
  })

  # Remove PK and prior records:
  # $PRIOR, $THETA/$THETAP/$THETAPV, $OMEGA/$OMEGAP/$OMEGAPD, $SIGMA/$SIGMAP/$SIGMAPD
  records_rm <- c("prior", "theta", "thetapv", "omega", "omegapd", "sigma", "sigmapd")
  purrr::walk(records_rm, function(rec){
    if(mod_has_record(vpc_sim, type = rec)) remove_records(vpc_sim, type = rec)
  })

  # Set up simulation
  setup_vpc_sim(vpc_sim, n = n, seed = seed)

  # Return read-in model to get the updated class as part of the model object
  return(read_model(file.path(model_dir, vpc_dir)))
}


get_msf_path <- function(.mod){
  ctl <- safe_read_ctl(.mod)
  ests <- nmrec::select_records(ctl, "est")

  msf_path <- purrr::map(ests, function(est){
    opt <- nmrec::get_record_option(est, "msf")
    if(!is.null(opt)) opt$value else opt
  }) %>% purrr::list_c()

  if(!is.null(msf_path)){
    if(length(msf_path) != 1){
      msf_txt <- paste0(msf_path, collapse = ", ")
      rlang::abort(
        c(glue("Multiple MSF files found: {msf_txt}"))
      )
    }
    msf_path <- file.path(.mod[[ABS_MOD_PATH]], msf_path)
  }

  return(msf_path)
}


#' Set up VPC simulation
#'
#' Creates new `$SIMULATION` and `$MSFI` records
setup_vpc_sim <- function(.vpc_sim, n = 100, seed = 1234){
  checkmate::assert_numeric(n, lower = 1)
  checkmate::assert_numeric(seed)
  # Check for $PRED or $ERROR records (cant be both)
  after_opts <- c(
    "pred" = mod_has_record(.vpc_sim, "pred"),
    "error" = mod_has_record(.vpc_sim, "error")
  )
  if(all(after_opts)){
    dev_error("setup_vpc_sim only supports $PRED or $ERROR records. Not both.")
  }
  after_name <- names(after_opts[after_opts])


  ## Add new variable assignment to $ERROR _or_ $PRED: `REPI=IREP` ##
  # Store current $error or $pred record
  after_rec <- get_records(.vpc_sim, after_name)
  after_rec <- after_rec[[length(after_rec)]]
  after_lines <- after_rec$get_lines()
  pred_error_lines <- after_lines[after_lines != ""]
  # TODO: do we need to check if this was already defined?
  pred_error_lines <- c(pred_error_lines, "REPI=IREP", "")

  # Remove old record, and add a new one with the modification
  #  - Use existing record name, dont overwrite
  remove_records(.vpc_sim, after_name)
  add_new_record(.vpc_sim, after_name, rec_name = NULL, lines = pred_error_lines, after = NULL)

  ## Add new $SIMULATION record (must be after $ERROR or $PRED, but before $TABLE) ##
  sim_lines <- glue("({seed}) ({seed} NORMAL) SUBPROBLEMS={n} TRUE=FINAL")
  add_new_record(.vpc_sim, "simulation", lines = sim_lines, after = after_name)

  ## Add new $TABLE record with predefined format and columns (including REPI) ##
  table_name <- glue("{get_model_id(get_based_on(.vpc_sim))}sim.tab")
  table_lines <- glue("ONEHEADER REPI DV PRED FILE={table_name}")
  add_new_record(.vpc_sim, "table", lines = table_lines, after = "simulation")

  ## Add new $MSFI record (end of file) ##
  msf_path <- get_msf_path(read_model(get_based_on(.vpc_sim)))
  msf_path_rel <- fs::path_rel(msf_path, .vpc_sim[[ABS_MOD_PATH]])
  msfi_lines <- glue("{msf_path_rel} NOMSFTEST")
  add_new_record(.vpc_sim, "msfi", lines = msfi_lines, after = NULL)

  return(invisible(.vpc_sim))
}
