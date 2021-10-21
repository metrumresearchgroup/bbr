#' NONMEM Covariance and Correlation Matrices
#'
#' Pulls in the covariance and correlation matrices (for the final estimation method) for NONMEM models.
#' If no `.cov` or `.cor` files are found for the relevant model, will error informing the user.
#'
#' @return A named list of matrices.
#'
#' @seealso [model_summary()]
#' @param .mod Model to check.
#' @param .threshold Numeric scalar between 0 and 1 (defaults to `0.95`). Will
#'   print a warning if the absolute values of any of the _off-diagonals_ in the
#'   correlation matrix are above this threshold.
#' @param ... args passed through to [bbi_exec()]
#' @param .dry_run show what the command would be without actually running it
#' @export
cov_cor <- function(
  .mod,
  .threshold = 0.95,
  ...,
  .dry_run = FALSE
) {
  UseMethod("cov_cor")
}

#' @describeIn cov_cor Get cov and cor from `bbi_nonmem_model` object
#' @importFrom checkmate assert_numeric
#' @export
cov_cor.bbi_nonmem_model <- function(
  .mod,
  .threshold = 0.95,
  ...,
  .dry_run = FALSE
) {

  # check inputs
  check_yaml_in_sync(.mod)
  checkmate::assert_numeric(.threshold, lower = 0, upper = 1, len = 1, null.ok = TRUE)

  # build args for bbi
  .path <- get_output_dir(.mod)
  cmd_args <- c("nonmem", "covcor", get_model_id(.mod))

  # if .dry_run return output call
  if (.dry_run) {
    res <- bbi_dry_run(cmd_args, .path)
    return(res)
  }

  # otherwise, execute
  res <- tryCatch(
    bbi_exec(cmd_args, .dir = .path, ..., .wait = TRUE),
    error = function(e) {
      err_msg <- glue("cov_cor('{get_model_id(.mod)}') failed with the following error. \n\nERROR: \n{e}")
      stop(err_msg, call. = FALSE)
    }
  )

  res_list <- res$stdout %>%
    paste(collapse="") %>%
    jsonlite::fromJSON(simplifyDataFrame = FALSE)

  num_est <- length(res_list$covariance_theta)
  out <- list(
    cov_theta = matrix(
      data = res_list$covariance_theta[[num_est]]$values,
      nrow = res_list$covariance_theta[[num_est]]$dim,
      ncol = res_list$covariance_theta[[num_est]]$dim
    ),
    cor_theta = matrix(
      data = res_list$correlation_theta[[num_est]]$values,
      nrow = res_list$correlation_theta[[num_est]]$dim,
      ncol = res_list$correlation_theta[[num_est]]$dim
    )
  )

  # warn if over threshold
  if (!is.null(.threshold)) {
    check_cor_threshold(out$cor_theta, .threshold)
  }

  return(out)
}

#' @describeIn cov_cor Get cov and cor from `bbi_nonmem_summary` object (output of `model_summary()`)
#' @importFrom checkmate assert_numeric
#' @export
cov_cor.bbi_nonmem_summary <- function(
  .mod,
  .threshold = 0.95,
  ...,
  .dry_run = FALSE
) {
  .mod <- read_model(.mod[[ABS_MOD_PATH]])
  cov_cor(
    .mod = .mod,
    .threshold = .threshold,
    ...,
    .dry_run = .dry_run
  )
}


#' Check if any correlations are over threshold and warn if so
#' @keywords internal
check_cor_threshold <- function(cor_mat, threshold) {

  bool_mat <- abs(cor_mat) > threshold

  # make all the diagonals (and upper triangle) FALSE because we only care about off-diagonals
  .dim <- nrow(bool_mat)
  for (.i in 1:.dim) {
    bool_mat[.i, .i:.dim] <- FALSE
  }

  if (any(bool_mat)) {

    losers <- map_chr(which(bool_mat), ~ {
      .col <- ceiling(.x/.dim)
      .row <- .x - ((.col-1)*.dim)
      glue("({.row},{.col})")
    })

    warning(paste(
      glue("Off-diagonal theta correlations above specified threshold of {.threshold}\n\n"),
      paste(losers, collapse = "\n ")
    ), call. = FALSE)
  }
}
