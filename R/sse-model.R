#' Create an SSE run from an existing model
#'
#' Creates a new `bbi_nmsse_model` object, from an existing `bbi_nonmem_model`
#' object. This function creates a new control stream, that is a copy of `.mod`
#' with the `$TABLE` and `$COV` records optionally removed (see `remove_cov` and
#' `remove_tables` arguments). The object returned from this must then be passed
#' to [setup_sse_run()] before submission (see examples).
#'
#' @inheritParams new_analysis_run
#' @param remove_msf If `TRUE`, the default, remove any `MSFO` options from
#'  `$EST` records.
#'
#' @examples
#' \dontrun{
#'
#' # Denote N number of simulations
#' N_SIM <- 20
#'
#' # Simulate data
#' add_simulation(.mod, n = N_SIM, .mode = "local")
#' sim_data <- nm_join_sim(.mod)
#'
#' # Create new SSE object
#' .sse_run <- new_sse_run(.mod)
#'
#' # Set up the run
#' setup_sse_run(
#'   .sse_run,
#'   data = sim_data,
#'   n = N_SIM,
#'   sample_size = 30,
#'   sim_col = "nn"
#' )
#' }
#' @return S3 object of class `bbi_nmsse_model`.
#' @export
new_sse_run <- function(
    .mod,
    .suffix = "sse",
    .inherit_tags = TRUE,
    .overwrite = FALSE,
    remove_cov = TRUE,
    remove_tables = TRUE,
    remove_msf = TRUE
){

  sse_run <- new_analysis_run(
    .mod,
    .suffix = .suffix,
    .type = "nmsse",
    .add_tags = "SSE_SUBMISSION",
    .inherit_tags = .inherit_tags,
    .overwrite = .overwrite,
    remove_cov = remove_cov,
    remove_tables = remove_tables
  )

  # Check for MSF path and remove if present
  # - MSF may have been used if add_simulation was used as the simulation
  # method for the based on model. We dont want to generate this for each
  # individual SSE model run
  if(isTRUE(remove_msf)){
    msf_path <- get_msf_path(sse_run, .check_exists = FALSE)
    if(!is.null(msf_path)){
      ctl <- get_model_ctl(sse_run)
      ests <- nmrec::select_records(ctl, "est")

      purrr::walk(ests, function(est){
        opt <- nmrec::get_record_option(est, "msf")
        if(!is.null(opt)) opt$value <- NULL
      })
      mod_path <- get_model_path(sse_run)
      nmrec::write_ctl(ctl, mod_path)
    }
  }

  return(sse_run)
}



#' Set up a SSE model run
#'
#' This function takes a `bbi_nmsse_model` (created by a previous
#' [new_sse_run()] call) and creates `n` new model objects and re-sampled
#' datasets in a subdirectory. The control stream found at
#' `get_model_path(.sse_run)` is used as the "template" for these new model
#' objects, and the new datasets are sampled from the dataset passed to `data`.
#'
#' @param .sse_run A `bbi_nmsse_model` object.
#' @param n Number of data sets and model runs to generate.
#' @param data A dataset to resample from.
#' @inheritParams setup_analysis_run
#'
#' @details
#' Once you have run this function, you can execute your SSE with
#' [submit_model()]. You can use [get_model_status()] to check on your submitted
#' SSE run. Once all models have finished, use [summarize_sse_run()]
#' to view the results. See examples below.
#'
#'
#' @seealso [new_sse_run()] [summarize_sse_run()] [submit_model()]
#'
#' @examples
#' \dontrun{
#'
#' # Simulate data
#' N_SIM <- 1000
#' add_simulation(.mod, n = N_SIM, .mode = "local")
#' sim_data <- nm_join_sim(.mod)
#'
#' # Setup
#' .sse_run <- new_sse_run(.mod)
#' .sse_run <- setup_sse_run(
#'   .sse_run,
#'   data = sim_data,
#'   sim_col = "nn",
#'   n = N_SIM,
#'   sample_size = 50,
#'   strat_cols = c("STUDY", "SEX")
#' )
#'
#' # Submit
#' submit_model(.sse_run)
#'
#' # Check status of runs during submission
#' get_model_status(.sse_run)
#'
#' # Summarize results, once all runs have finished
#' if (check_nonmem_finished(.sse_run)) {
#'   .sse_sum <- summarize_sse_run(.sse_run)
#' }
#' }
#' @export
setup_sse_run <- function(
    .sse_run,
    n = 200,
    sample_size = NULL,
    strat_cols = NULL,
    replace = TRUE,
    seed = 1234,
    data = NULL,
    sim_col = "nn",
    .bbi_args = list(
      no_grd_file = TRUE,
      no_shk_file = TRUE
    ),
    .overwrite = FALSE
){
  check_model_object(.sse_run, NMSSE_MOD_CLASS)

  .sse_run <- setup_analysis_run(
    .sse_run, run_type = "sse", n = n,
    sample_size = sample_size, replace = replace,
    strat_cols = strat_cols, seed = seed, data = data, sim_col = sim_col,
    .bbi_args = .bbi_args, .overwrite = .overwrite
  )
  return(invisible(.sse_run))
}


#' Summarize an SSE run
#'
#' Summarize the parameter estimates, run details, and any heuristics of a
#' SSE run, saving the results to a `sse_summary.RDS` data file within the
#' SSE run directory.
#'
#' @inheritParams setup_sse_run
#' @param force_resummarize Logical (T/F). If `TRUE`, force re-summarization.
#'  Will _only_ update the saved out `RDS` file when specified via
#'  `summarize_sse_run()`. See details for more information.
#'
#' @details
#'  - `summarize_sse_run()` does the following things:
#'     - Tabulates run details and heuristics.
#'     - Calls `summary_log()` and binds the results to the parameter estimates.
#'       information if a `sse_summary.RDS` data file exists.
#'     - Either saves this data out to `sse_summary.RDS`, or reads it in if it
#'     already exists (see section below).
#'     - Formats the returned object as a `bbi_nmsse_summary` S3 object, and
#'     displays key summary information when printed to the console.
#'
#' ## Saved out data file:
#' The first time `summarize_sse_run()` is called (or if
#' `force_resummarize = TRUE`), it will save the results to a `sse_summary.RDS`
#' data file within the SSE run directory. If one already exists, that data
#' set will be read in by default instead of being re-summarized.
#'  - The purpose of this is functionality two fold. For one, it helps avoid the
#'  need of re-executing `model_summary()` calls for a large number of runs. It
#'  also helps to reduce the number of files you need to commit via version
#'  control (see `cleanup_sse_run()`).
#'
#' @seealso [cleanup_sse_run()] [new_sse_run()] [setup_sse_run()]
#'
#' @examples
#' \dontrun{
#'
#' .sse_run <- read_model(file.path(MODEL_DIR, "1-sse"))
#'
#' # Create full summary object and save results
#' sse_sum <- summarize_sse_run(.sse_run)
#'
#' # Read in just the parameter estimates
#' sse_estimates(.sse_run)
#'
#'
#' # Optionally compare to initial estimates of "based on" model
#' initial_estimates_compare(sse_sum)
#'
#' }
#'
#' @name summarize_sse
NULL


#' @describeIn summarize_sse Summarize an SSE run and store results
#' @importFrom tidyselect any_of
#' @export
summarize_sse_run <- function(
    .sse_run,
    force_resummarize = FALSE
){
  check_model_object(.sse_run, NMSSE_MOD_CLASS)
  sse_sum_path <- get_analysis_sum_path(.sse_run, .check_exists = FALSE)

  if(!fs::file_exists(sse_sum_path) || isTRUE(force_resummarize)){
    sse_sum <- summarize_analysis_run(
      .sse_run,
      .bbi_args = list(
        no_grd_file = TRUE, no_shk_file = TRUE
      )
    )

    # Add parameter estimates
    # - Append standard error
    #   - Get parameter estimates again from summary log because we need to tabulate
    #     standard error for SSE runs, which param_estimates_batch doesnt support
    # - return as long format
    sum_log <- sse_sum$analysis_summary
    class(sum_log) <- c(SUM_LOG_CLASS, class(sum_log))
    param_ests <- get_from_summay_log(sum_log, "parameter_estimates")
    sse_sum$parameter_estimates <- param_ests

    # Add comparison of sse estimates to initial estimates of based_on model
    #  - This gets done here instead of the print method, because we dont want
    #    the printing of the _saved_ model summary object to be tied to the
    #    existence of the based_on model.
    sse_compare <- initial_estimates_compare(sse_sum)
    sse_sum$sse_compare <- sse_compare

    saveRDS(sse_sum, sse_sum_path)
  }else{
    verbose_msg(
      glue("Reading in SSE summary: {fs::path_rel(sse_sum_path, getwd())}\n\n")
    )
    sse_sum <- readRDS(sse_sum_path)
  }

  # reset model path to current absolute path on this system (instead of what's pulled from RDS/JSON)
  sse_sum[[ABS_MOD_PATH]] <- .sse_run[[ABS_MOD_PATH]]

  # if parent model is present, reset based on paths as well
  based_on_path <- get_based_on(.sse_run)[[1]]
  if (fs::file_exists(paste0(based_on_path, ".yaml"))) {
    based_on_mod <- read_model(based_on_path)
    sse_sum$based_on_model_path <- get_model_path(based_on_mod)
    sse_sum$based_on_data_set <- get_data_path(based_on_mod)
  } else {
    # if not, set to "<not found>" to avoid confusion with stale paths
    rlang::warn(glue("SSE run {get_model_id(.sse_run)} cannot find parent model. Expected to be found at {based_on_path}"))
    sse_sum$based_on_model_path <- "<not found>"
    sse_sum$based_on_data_set <- "<not found>"
  }

  # Assign class and return
  class(sse_sum) <- c(NMSSE_SUM_CLASS, class(sse_sum))
  return(sse_sum)
}


#' @describeIn summarize_sse Tabulate parameter estimates for each model
#'  submission in an SSE run
#' @export
sse_estimates <- function(
    .sse_run,
    force_resummarize = FALSE
){
  param_ests <- analysis_estimates(.sse_run, force_resummarize)
  return(param_ests)
}

#' @describeIn summarize_sse Read in all SSE run model objects
#' @export
get_sse_models <- function(.sse_run){
  get_analysis_models(.sse_run)
}


#' Cleanup SSE run directory
#'
#' This will delete all child models, and only keep the information
#' you need to read in estimates or summary information
#'
#' @details
#' The intent of this function is to help reduce the number of files you need to
#' commit via version control. Collaborators will be able to read in the
#' SSE model and summary objects without needing individual run files.
#'  - Note that this will prevent `force_resummarize = TRUE` from working
#'
#' **This should only be done** if you no longer need to re-summarize, as this
#'  will clean up (delete) the *individual* SSE model files
#'
#' @examples
#' \dontrun{
#'
#' .sse_run <- read_model(file.path(MODEL_DIR, "1-sse"))
#' cleanup_sse_run(.sse_run)
#' }
#'
#' @inheritParams setup_sse_run
#' @inheritParams delete_models
#' @seealso [summarize_sse_run()]
#'
#' @export
cleanup_sse_run <- function(.sse_run, .force = FALSE){
  cleanup_analysis_run(.sse_run, .force = .force)
}





#' Compare SSE results to initial (or "true") estimates.
#'
#' @param .sse_sum A `bbi_nmsse_summary` object.
#' @inheritParams param_estimates_compare
#' @param .orig_mod `bbi_model` object to compare `.sse_sum` against. This will
#'   be automatically set if passing in a `bbi_nmsse_summary` object.
#' @export
initial_estimates_compare <- function(
    .sse_sum,
    .orig_mod = NULL,
    probs = c(.5, 0.025, 0.975),
    na.rm = FALSE
){

  # Attempt to read in based_on model
  if(is.null(.orig_mod)){
    orig_mod_path <- fs::path_ext_remove(.sse_sum$based_on_model_path)
    # make sure based_on model still exists
    .orig_mod <- tryCatch(
      read_model(orig_mod_path),
      error = function(cond) NULL
    )
    if(is.null(.orig_mod)){
      rlang::warn(
        c(
          glue("The original model no longer exists at {orig_mod_path}"),
          "Cannot compare to original model"
        )
      )
    }
  }

  # Dont pass .compare_cols here, as we can only use columns in parameter_names,
  # which could only be the default columns.
  # Dont pass .orig_mod because we want to compare the initial estimates
  comp_df <- param_estimates_compare.default(
    .sse_sum$analysis_summary, .orig_mod = NULL, probs = probs, na.rm = na.rm
  )

  if (!is.null(.orig_mod)) {
    mod_df <- initial_estimates(.orig_mod)

    comp_df <- mod_df %>%
      select("parameter_names", "init") %>%
      rename(initial = "init") %>%
      left_join(comp_df, by = "parameter_names")
  }

  return(comp_df)
}
