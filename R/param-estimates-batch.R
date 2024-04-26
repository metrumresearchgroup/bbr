# Batch Processing of Parameter Estimates in R

#' Batch Processing of Parameter Estimates
#'
#' Calls out to `bbi nonmem params` and returns a tibble of parameter estimates.
#' The primary reason for this function is for things like bootstraps or
#' simulations where parameter estimates for a large number of models (hundreds
#' or thousands) need to be pulled in together. In those cases, this function is
#' _much_ faster than using [model_summaries()] and [param_estimates()],
#' because it does _not_ parse all of the other information contained in a
#' `bbi_nonmem_summary` object.
#'
#' @details
#'
#' The tibble will always have columns `absolute_model_path`, `run`,
#' `error_msg`, and `termination_code`. All other column names are those of the
#' parameter estimates found in the `.ext` files detected in the directory
#' passed. Note: the passed directory is _not_ searched recursively. Passed
#' directory will be searched for model files and output directories for found
#' models will be searched for `.ext` files.
#'
#' `error_msg`: column will be `NA` if error is not detected. Column will contain message if known error is detected. The known errors
#' are
#'   - `"no ext file contents"`: `.ext` file detected, but contains nothing
#'   - `"no estimation output detected"`: `.ext` file detected, has contents, but is missing line `-1000000000` which are the final parameter estimates
#'
#' `termination_code`: parsed from line `-1000000007` of the `.ext`
#'
#' **parameter columns:** naming will be the same as names found in `.ext` file.
#' If `.ext` files within the directory contain differing parameter names, the
#' output tibble will show all parameter names across files and NA values for
#' the files where a given parameter is missing.
#'
#' @importFrom dplyr rename select mutate everything across
#' @importFrom data.table fread
#' @importFrom tibble as_tibble
#'
#' @param .path a path to a directory containing model sudirectories for batch parameter processing.
#' @param ... args passed through to [bbi_exec()]
#' @param .dry_run show what the command would be without actually running it
#' @export
param_estimates_batch <- function(.path,
                                  ...,
                                  .dry_run = FALSE) {

  assert_bbi_version(.min_version = "3.1.0", .function = "param_estimates_batch")

  .path <- fs::path_abs(.path)
  if (!dir_exists(.path)) {
    err_msg <-
      glue("param_estimates_batch('{.path}') failed; unable to locate {.path}")
    stop(err_msg, call. = FALSE)
  }

  # path containing .ext file
  ext_file_dir <- basename(.path)

  # for bbi nonmem params, need to pass the directory containing .ext
  # cd to the root directory containing directory with .ext files
  .path <- dirname(.path)

  cmd_args <- c("nonmem", "params", "--dir", ext_file_dir)

  # if .dry_run return output call
  if (.dry_run) {
    res <- bbi_dry_run(cmd_args, .path)
    return(res)
  }

  res <- tryCatch(
    bbi_exec(cmd_args, .dir = .path, ..., .wait = TRUE),
    error = function(e) {
      err_msg <-
        glue(
          "param_estimates_batch('{.path}') failed with the following error. This may be because the modeling run has not finished successfully.\n\nERROR: \n{e}"
        )
      stop(err_msg, call. = FALSE)
    }
  )

  df <- as_tibble(fread(text = res$stdout, header = TRUE))

  # throw out extra column that gets created if no models succeeded
  if ("V4" %in% names(df)) df <- select(df, -"V4")

  # reformat parameter names to conform with `param_estimates()` output
  names(df) <- gsub("\\(([0-9]+)_([0-9]+)\\)", "(\\1,\\2)", names(df))

  # format and return
  df %>%
    rename(
      absolute_model_path = "dir",
      error_msg = "error",
      termination_code = "termination"
    ) %>%
    mutate(
      across(everything(), ~ ifelse(. == "", NA, .)),
      absolute_model_path = file.path(.path, .data$absolute_model_path),
      run = basename(.data$absolute_model_path)
    ) %>%
    select(
      "absolute_model_path",
      "run",
      "error_msg",
      "termination_code",
      everything()
    )
}


#' Compare parameter estimates
#'
#' Summarizes each parameter estimate, showing you the quantiles passed to
#' `probs`, and optionally the estimates of `.orig_mod`.
#'
#' @param .boot_sum Either a `bbi_nmboot_summary` object, or a tibble with
#'   columns for each parameter and rows for each model (like what's returned
#'   from [param_estimates_batch()]).
#' @param .orig_mod `bbi_model` object to compare `.boot_sum` against. This will
#'   be automatically set if passing in a `bbi_nmboot_summary` object.
#' @param .compare_cols An expression that can be passed to [dplyr::select()] to
#'   select which columns in `.boot_sum` will be pivoted and summarized. See
#'   [?tidyselect::language](https://tidyselect.r-lib.org/reference/language.html)
#'    for more details and options. **Only used** if `.boot_sum` is a tibble.
#' @param probs Numeric vector with values between 0 and 1 to be passed through to
#'   [stats::quantile()]. Represents the quantiles to calculate for parameter
#'   estimates in `.boot_sum`.
#' @param na.rm Logical scalar, passed through to [stats::quantile()].
#'
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr summarise across starts_with select rename left_join
#' @importFrom stats quantile
#'
#' @return A tibble containing quantiles for each parameter estimate, optionally
#' compared to the estimates from `.orig_mod`.
#' @seealso summarize_bootstrap_run
#' @examples
#' \dontrun{
#'
#' # Via a bootstrap run
#' boot_run <- read_model(file.path(MODEL_DIR, "1-boot"))
#' boot_sum <- summarize_bootstrap_run(boot_run)
#' param_estimates_compare(boot_sum)
#'
#' # Via a custom table
#' orig_mod <- read_model(file.path(MODEL_DIR, "1"))
#' param_df <- param_estimates_batch(MODEL_DIR)
#' param_estimates_compare(param_df, .orig_mod = orig_mod)
#' }
#'
#' @export
param_estimates_compare <- function(
    .boot_sum,
    .orig_mod = NULL,
    .compare_cols = starts_with(c("THETA", "SIGMA", "OMEGA")),
    probs = c(.5, 0.025, 0.975),
    na.rm = FALSE
){
  UseMethod("param_estimates_compare")
}

#' @rdname param_estimates_compare
#' @export
param_estimates_compare.bbi_nmboot_summary <- function(
    .boot_sum,
    .orig_mod = NULL,
    .compare_cols = NULL,
    probs = c(.5, 0.025, 0.975),
    na.rm = FALSE
){

  # Attempt to read in based_on model
  if(is.null(.orig_mod)){
    orig_mod_path <- fs::path_ext_remove(.boot_sum$based_on_model_path)
    # make sure based_on model still exists
    .orig_mod <- tryCatch(
      read_model(orig_mod_path),
      error = function(cond) NULL
    )
    if(!is.null(.orig_mod)){
      orig_config_path <- file.path(
        get_output_dir(.orig_mod, .check_exists = FALSE), "bbi_config.json"
      )
      if (fs::file_exists(orig_config_path)) {
        res <- check_up_to_date(.orig_mod)
        if(any(res[!res])){
          rlang::warn("The original model is not up to date")
        }
      }else{
        # Set the original model to NULL if the run directory/config file didnt
        # exist
        rlang::warn("The original model has not been run yet. Cannot compare estiamtes.")
        .orig_mod <- NULL
      }
    }else{
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
  param_estimates_compare(
    .boot_sum$boot_summary, .orig_mod = .orig_mod, probs = probs, na.rm = na.rm
  )
}


#' @rdname param_estimates_compare
#' @export
param_estimates_compare.default <- function(
    .boot_sum,
    .orig_mod = NULL,
    .compare_cols = starts_with(c("THETA", "SIGMA", "OMEGA")),
    probs = c(.5, 0.025, 0.975),
    na.rm = FALSE
){

  comp_df <- .boot_sum %>%
    select({{ .compare_cols }})

  if (!is.null(.orig_mod)) {
    if (!inherits(.orig_mod, NM_SUM_CLASS)) {
      .orig_mod <- model_summary(.orig_mod)
    }
    mod_df <- param_estimates(.orig_mod)


    # check that param names match
    n1 <- sort(mod_df$parameter_names)
    n2 <- sort(names(comp_df))
    .same <- suppressSpecificWarning(
      all(n1 == n2),
      "is not a multiple"
    )
    if (!.same) {
      stop(paste(
        glue("The models passed to `.boot_sum` and `.orig_mod` do not have the same parameters."),
        glue("  `.boot_sum` parameter names: {paste(n2, collapse = ', ')}"),
        glue("  `.orig_mod` parameter names: {paste(n1, collapse = ', ')}"),
        sep = "\n"
      ))
    }
  }

  # summarize comp to quantiles

  quantile_fn <- function(x)  {
    quantile(x, probs = probs, na.rm = na.rm)
  }

  comp_df <- comp_df %>%
    reframe(across(.cols = everything(), .fns = quantile_fn)) %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column() %>% tibble::as_tibble()
  colnames(comp_df) <- c("parameter_names", paste0("p", probs * 100))

  # join quantiles to original
  if (!is.null(.orig_mod)) {
    comp_df <- mod_df %>%
      select("parameter_names", "estimate") %>%
      rename(original_estimate = "estimate") %>%
      left_join(comp_df, by = "parameter_names")
  }

  return(comp_df)
}

