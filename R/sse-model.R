#' Create an SSE run from an existing model
#'
#' Creates a new `bbi_nmsse_model` object, from an existing `bbi_nonmem_model`
#' object. This function creates a new control stream, that is a copy of `.mod`
#' with the `$TABLE` and `$COV` records optionally removed (see `remove_cov` and
#' `remove_tables` arguments). The object returned from this must then be passed
#' to [setup_sse_run()] before submission (see examples).
#'
#' @inheritParams new_analysis_run
#'
#' @examples
#' \dontrun{
#'
#' # Create new bootstrap object
#' .sse_run <- new_sse_run(.mod)
#'
#' # Set up the run
#' setup_sse_run(.sse_run)
#' }
#' @return S3 object of class `bbi_nmsse_model`.
#' @export
new_sse_run <- function(
    .mod,
    .suffix = "sse",
    .inherit_tags = TRUE,
    .overwrite = FALSE,
    remove_cov = TRUE,
    remove_tables = TRUE
){

  new_analysis_run(
    .mod,
    .suffix = .suffix,
    .type = "nmsse",
    .add_tags = "SSE_SUBMISSION",
    .inherit_tags = .inherit_tags,
    .overwrite = .overwrite,
    remove_cov = remove_cov,
    remove_tables = remove_tables
  )
}



#' Set up a bootstrap model run
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
#' bootstrap run. Once all models have finished, use [summarize_sse_run()]
#' to view the results. See examples below.
#'
#'
#' @seealso [new_sse_run()] [summarize_sse_run()] [submit_model()]
#'
#' @examples
#' \dontrun{
#'
#' # Setup
#' .sse_run <- new_sse_run(.mod)
#' .sse_run <- setup_sse_run(
#'   .sse_run,
#'   n = 1000,
#'   sample_size = 50,
#'   seed = 1234,
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
    replace = FALSE,
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
