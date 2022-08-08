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

  df <- as_tibble(fread(text = res$stdout))

  # throw out extra column that gets created if no models succeeded
  if ("V4" %in% names(df)) df <- select(df, -.data$V4)

  # reformat parameter names to conform with `param_estimates()` output
  names(df) <- gsub("\\(([0-9]+)_([0-9]+)\\)", "(\\1,\\2)", names(df))

  # format and return
  df %>%
    rename(
      absolute_model_path = .data$dir,
      error_msg = .data$error,
      termination_code = .data$termination
    ) %>%
    mutate(
      across(everything(), ~ ifelse(. == "", NA, .)),
      absolute_model_path = file.path(.path, .data$absolute_model_path),
      run = basename(.data$absolute_model_path)
    ) %>%
    select(
      .data$absolute_model_path,
      .data$run,
      .data$error_msg,
      .data$termination_code,
      everything()
    )
}


#' Compare parameter estimates
#'
#' Summarizes the parameter estimates from tibble like what's returned from
#' [param_estimates_batch()], showing you the quantiles passed to `probs`,
#' likely interpreted as confidence intervals for your parameter estimates.
#' Optionally takes an "original model" to compare these quantiles against.
#' Originally conceived for comparing a model to multiple runs of the "same"
#' model, for example in a bootstrap or simulation.
#'
#' @return A tibble with the first column containing the parameter names (that
#'   were column names in `.param_df`), the second column optionally containing
#'   original parameter estimates (if `.orig_mod` is passed), and subsequent
#'   columns containing the quantiles specified in `probs`. You can think of
#'   this as essentially `.param_df`, summarized by quantiles across all rows,
#'   and then pivoted.
#'
#'
#' @param .param_df A tibble with columns for each parameter and rows for each
#'   model (like what's returned from [param_estimates_batch()])
#' @param .orig_mod `bbi_model` object to compare `.param_df` against. If
#'   `NULL`, the default, only returns quantiles from `.param_df`. If passed,
#'   will display an additional `original_estimate` column.
#' @param .compare_cols An expression that can be passed to [dplyr::select()] to
#'   select which columns in `.param_df` will be pivoted and summarized.
#'   Defaults to `dplyr::starts_with(c("THETA", "SIGMA", "OMEGA"))` (designed
#'   for NONMEM runs), but consider using [dplyr::matches()] to select columns
#'   using regex. Also see
#'   [?tidyselect::language](https://tidyselect.r-lib.org/reference/language.html)
#'    for more details and options.
#' @param probs Numeric vector with values between 0 and 1 to be passed through to
#'   [stats::quantile()]. Represents the quantiles to calculate for parameter
#'   estimates in `.param_df`.
#' @param na.rm Logical scalar, passed through to [stats::quantile()].
#'
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr summarise across starts_with select rename left_join
#' @importFrom stats quantile
#' @export
param_estimates_compare <- function(
  .param_df,
  .orig_mod = NULL,
  .compare_cols = starts_with(c("THETA", "SIGMA", "OMEGA")),
  probs = c(.5, 0.025, 0.975),
  na.rm = FALSE
) {

  comp_df <- .param_df %>%
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
        glue("The models passed to `.param_df` and `.orig_mod` do not have the same parameters."),
        glue("  `.param_df` parameter names: {paste(n2, collapse = ', ')}"),
        glue("  `.orig_mod` parameter names: {paste(n1, collapse = ', ')}"),
        sep = "\n"
      ))
    }
  }

  # summarize comp to quantiles
  comp_df <- comp_df %>%
    summarise(across(
      .fns = quantile,
      probs = probs,
      na.rm = na.rm
    )) %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column()
  colnames(comp_df) <- c("parameter_names", paste0(probs * 100, "%"))

  # join quantiles to original
  if (!is.null(.orig_mod)) {
    comp_df <- mod_df %>%
      select(.data$parameter_names, .data$estimate) %>%
      rename(original_estimate = .data$estimate) %>%
      left_join(comp_df, by = "parameter_names")
  }

  return(comp_df)
}

