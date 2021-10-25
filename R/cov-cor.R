#' NONMEM Covariance and Correlation Matrices
#'
#' Pulls in the covariance and correlation matrices (for the final estimation method) for NONMEM models.
#' If no `.cov` or `.cor` files are found for the relevant model, will error informing the user.
#'
#' @return A named list of matrices.
#'
#' @seealso [model_summary()], [check_cor_threshold()]
#' @param .mod Model to check.
#' @param .threshold Numeric scalar between 0 and 1. Will print a warning if the
#'   absolute values of any of the _off-diagonals_ in the correlation matrix are
#'   above this threshold. If `NULL`, the default, skips this check.
#' @param ... args passed through to [bbi_exec()]
#' @param .dry_run show what the command would be without actually running it
#' @export
cov_cor <- function(
  .mod,
  .threshold = NULL,
  ...,
  .dry_run = FALSE
) {
  UseMethod("cov_cor")
}

#' @describeIn cov_cor Get cov and cor from `bbi_nonmem_model` object
#' @importFrom checkmate assert_numeric
#' @importFrom stringr str_subset
#' @importFrom jsonlite fromJSON
#' @export
cov_cor.bbi_nonmem_model <- function(
  .mod,
  .threshold = NULL,
  ...,
  .dry_run = FALSE
) {
  check_bbi_version_constraint(.min_version = "2.3.0", .function = "cov_cor")

  # check inputs
  check_yaml_in_sync(.mod)

  # build args for bbi
  .path <- get_output_dir(.mod)
  cmd_args <- c("nonmem", "covcor", get_model_id(.mod))

  # if .dry_run return output call
  if (.dry_run) {
    res <- bbi_dry_run(cmd_args, .path)
    return(res)
  }

  # parse thetas with bbi
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

  # parse full matrices and assemble output
  out <- list(
    cov = parse_cov_cor_full_file(.mod, ".cov"),
    cor = parse_cov_cor_full_file(.mod, ".cor")
  )

  theta_names <- str_subset(dimnames(out$cov)[[1]], "THETA")
  out$cov_theta <- matrix(
    data = res_list$covariance_theta[[num_est]]$values,
    nrow = res_list$covariance_theta[[num_est]]$dim,
    ncol = res_list$covariance_theta[[num_est]]$dim,
    dimnames = list(theta_names, theta_names)
  )
  out$cor_theta <- matrix(
    data = res_list$correlation_theta[[num_est]]$values,
    nrow = res_list$correlation_theta[[num_est]]$dim,
    ncol = res_list$correlation_theta[[num_est]]$dim,
    dimnames = list(theta_names, theta_names)
  )

  # warn if over threshold
  if (!is.null(.threshold)) {
    check_cor_threshold(out$cor, .threshold)
  }

  return(out)
}

#' @describeIn cov_cor Get cov and cor from `bbi_nonmem_summary` object (output of `model_summary()`)
#' @importFrom checkmate assert_numeric
#' @export
cov_cor.bbi_nonmem_summary <- function(
  .mod,
  .threshold = NULL,
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


#' Check for high correlations
#'
#' Issues a warning if the absolute value of any of the
#'   _off-diagonal_ elements of `.cor_mat` exceed `.threshold`.
#' @param .cor_mat Matrix of correlations to check
#' @inheritParams cov_cor
#' @seealso [cov_cor()]
#' @export
check_cor_threshold <- function(.cor_mat, .threshold = 0.95) {

  checkmate::assert_numeric(.threshold, lower = 0, upper = 1, len = 1, null.ok = TRUE)

  bool_mat <- abs(.cor_mat) > .threshold

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


#' Helper to pull full matrix from final estimation method from `.cov` or `.cor` files
#' @param .mod the model object
#' @param .suffix either `.cov` or `.cor`
#' @importFrom readr read_table read_lines cols
#' @importFrom stringr str_detect
#' @keywords internal
parse_cov_cor_full_file <- function(.mod, .suffix) {

  .f <- build_path_from_model(.mod, .suffix)
  .lines <- read_lines(.f)

  # get final estimation method only
  .t <- which(str_detect(.lines, "^TABLE"))
  .t <- .t[length(.t)]

  df <- read_table(
    I(.lines[(.t+1):length(.lines)]),
    col_types = readr::cols()
  ) %>%
    select(-NAME)

  df %>%
    as.matrix() %>%
    matrix(dimnames = list(names(df), names(df)), nrow = nrow(df))
}
