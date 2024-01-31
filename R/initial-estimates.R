#' Retrieve and format initial parameter estimates
#'
#' @param .mod a `bbr` model object
#' @param flag_fixed Logical (`TRUE`/`FALSE`). If `TRUE`, return which
#'        parameters are fixed.
#'
#' @details
#' `NA` values indicate that they were not specified in the control stream file.
#' This is true for `THETA` bounds and whether a given parameter is `FIXED` or not.
#'
#' If you would like the `OMEGA` or `SIGMA` records formatted as *full matrices*,
#' they are stored as attributes:
#' ```
#'  initial_est <- initial_estimates(.mod)
#'  attr(initial_est, "omega_mat")
#'  attr(initial_est, "sigma_mat")
#' ```
#'
#' @examples
#' mod1 <- read_model(
#'   system.file("model", "nonmem", "basic", "1", package = "bbr")
#' )
#'
#' initial_estimates(mod1)
#'
#' @export
initial_estimates <- function(.mod, flag_fixed = FALSE){
  initial_est <- get_initial_est(.mod, flag_fixed = flag_fixed)

  # THETA labels
  theta_inits <- initial_est$thetas
  theta_inits$parameter_names <- sprintf("THETA(%d)", 1:nrow(theta_inits))
  theta_inits <- theta_inits %>%
    dplyr::rename("lower_bound" = "low", "upper_bound" = "up")

  # Format OMEGA
  omega_inits <- matrix_to_df(initial_est$omegas, type = "omega")
  # Append record number
  omega_inits$record_number <- fmt_record_num(initial_est$omegas)
  if(isTRUE(flag_fixed)){
    omega_inits <- omega_inits %>% dplyr::left_join(
      matrix_to_df(attr(initial_est$omegas, "nmrec_flags")$fixed, type = "omega") %>%
        dplyr::rename("fixed"="init"),
      by = c("parameter_names")
    )
  }

  # Format SIGMA
  sigma_inits <- matrix_to_df(initial_est$sigmas, type = "sigma")
  # Append record number
  sigma_inits$record_number <- fmt_record_num(initial_est$sigmas)
  if(isTRUE(flag_fixed)){
    sigma_inits <- sigma_inits %>% dplyr::left_join(
      matrix_to_df(attr(initial_est$sigmas, "nmrec_flags")$fixed, type = "sigma") %>%
        dplyr::rename("fixed"="init"),
      by = c("parameter_names")
    )
  }

  # Combine
  initial_est_df <- dplyr::bind_rows(
    theta_inits %>% dplyr::mutate(record_type = "theta"),
    omega_inits %>% dplyr::mutate(record_type = "omega"),
    sigma_inits %>% dplyr::mutate(record_type = "sigma")
  ) %>% dplyr::relocate("parameter_names") %>%
    dplyr::relocate("record_number", .after = "record_type")

  # Filter out NA values, as these were not specified in the control stream file
  initial_est_df <- initial_est_df %>% dplyr::filter(!is.na(.data$init))

  # Add original matrices as attributes if needed
  attr(initial_est_df, "omega_mat") <- suppressWarnings(get_initial_est(.mod)) %>%
    purrr::pluck("omegas")
  attr(initial_est_df, "sigma_mat") <- suppressWarnings(get_initial_est(.mod)) %>%
    purrr::pluck("sigmas")

  return(initial_est_df)
}


#' Retrieve initial parameter estimates
#'
#' @inheritParams initial_estimates
#'
#' @keywords internal
get_initial_est <- function(.mod, flag_fixed = FALSE){

  test_nmrec_version(.min_version = "0.3.0.8001")
  check_model_object(.mod, "bbi_nonmem_model")


  ctl <- nmrec::read_ctl(get_model_path(.mod))

  if (isTRUE(using_old_priors(ctl))) {
    rlang::warn(
      c(
        "!" = "This model appears to be using $THETA, $OMEGA, and/or $SIGMA to specify priors.",
        "i" = " - That will cause this function to extract and display priors as if they are initial parameter estimates.",
        "i" = " - Consider changing the model to use the more specific prior record names (such as $THETAP and $THETAPV)."
      ))
  }


  # Set flags
  mark_flags <- if(isTRUE(flag_fixed)) "fix" else NULL

  # Get initial estimates
  theta_inits <- get_theta_inits(ctl, mark_flags = mark_flags)
  omega_inits <- nmrec::extract_omega(ctl, mark_flags = mark_flags)
  sigma_inits <- nmrec::extract_sigma(ctl, mark_flags = mark_flags)

  # Format as list
  inits <- list(
    thetas = theta_inits,
    omegas = omega_inits,
    sigmas = sigma_inits
  )

  return(inits)
}


#' Helper function for getting the initial theta values, including bounds and
#' the presence of `FIXED` parameters.
#'
#' @param ctl an `nmrec` ctl object.
#' @param mark_flags TODO
#'
#' @keywords internal
get_theta_inits <- function(ctl, mark_flags = NULL){

  # Tabulate initial values and bounds
  theta_inits <- purrr::map_dfc(c("init", "low", "up"), function(type){
    theta_inits <- nmrec::extract_theta(ctl, type = type)
    tibble::tibble(!!rlang::sym(type) := theta_inits)
  })

  # Tabulate presence of FIXED parameters
  theta_inits_fixed <- nmrec::extract_theta(ctl, mark_flags = mark_flags)
  theta_inits$fixed <- attr(theta_inits_fixed, "nmrec_flags")$fixed

  # Tabulate record number
  theta_inits$record_number <- fmt_record_num(theta_inits_fixed)

  return(theta_inits)
}



#' Format OMEGA or SIGMA matrix as data.frame
#'
#' @param mat a matrix
#' @param type one of `c("omega","sigma")`
#'
#' @keywords internal
matrix_to_df <- function(mat, type = c("omega","sigma")) {
  type <- toupper(match.arg(type))

  n <- nrow(mat)
  indices <- lower.tri(mat, diag = TRUE)

  row_indices <- row(indices)[indices]
  col_indices <- col(indices)[indices]

  parameter_names <- sprintf(paste0(type,"(%d,%d)"), row_indices, col_indices)

  values <- mat[indices]

  result_df <- tibble::tibble(
    parameter_names = parameter_names,
    init = values
  )

  return(result_df)
}


#' Get record number of initial estimates
#'
#' @param inits initial estimates object as returned by `nmrec::extract_*`
#'  functions. Should be a vector or matrix with `nmrec_record_size` attributes.
#'
#' @details
#' `attr(inits, "nmrec_record_size")` stores a vector of sizes, one for each
#' occurrence of a given record type.
#' This function creates a vector indicating the which record the initial
#' estimate belongs to, and correlates to the order of the initial estimates.
#' - For `THETA` records, this is simply `rep(seq_along(sizes), sizes)`
#' - For `OMEGA` or `SIGMA` records, it's a bit more complicated. Here we need
#' to subset the columns (not the rows) of the full lower triangular matrix to
#' update the `size` to include values created when diagonally concatenating
#' multiple matrix-type records.
#'
#' This function is intended to be used with `matrix_to_df()` for matrix-type
#' records.
#'
#' @returns a vector indicating the which record the initial estimate belongs to
#'
#' @keywords internal
fmt_record_num <- function(inits){
  sizes <- attr(inits, "nmrec_record_size")

  # If matrix, we need to take the full lower triangular matrix into account here,
  # and cannot treat each sub-matrix as a complete matrix, or some off-diagonals
  # will be missing when we go to bind this to the table returned by `matrix_to_df`
  if(inherits(inits, "matrix")){
    full_lower_tri <- lower.tri(inits, diag = TRUE)
    sizes <- purrr::imap_dbl(sizes, function(.x, .y){
      mat_start <- if(.y > 1) sum(sizes[1:(.y-1)]) + 1 else 1
      rec_lower_tri <- full_lower_tri[, mat_start:(mat_start+.x-1)]
      sum(rec_lower_tri)
    })
  }

  rep(seq_along(sizes), sizes)
}


#' Check if *non-informative* priors are being used
#'
#' @param ctl An `nmrec_ctl_records` object.
#'
#' @keywords internal
using_old_priors <- function(ctl) {
  # pull prior related records
  prior_recs <- nmrec::select_records(ctl, "prior")
  subroutine_recs <- nmrec::select_records(ctl, "sub")

  # Check for PRIOR subroutines
  if(length(subroutine_recs) > 0){
    prior_subs <- purrr::map_lgl(subroutine_recs, function(prior_sub){
      prior_sub_str <- prior_sub$get_lines()
      # filter out comments
      prior_sub_str <- gsub(";.*", "", prior_sub_str)
      # look for priors
      any(stringr::str_detect(prior_sub_str, "(?i)PRIOR"))
    })
    prior_subs <- any(prior_subs)
  }else{
    prior_subs <- FALSE
  }

  # Check if INFORMATIVE style is present
  prior_names <- c("thetap", "thetapv", "omegap", "omegapd", "sigmap", "sigmapd")
  informative_priors <- purrr::map(prior_names, function(.x){
    recs <- nmrec::select_records(ctl, .x)
    if(length(recs) == 0) return(NULL) else return(recs)
  }) %>% purrr::list_c()

  # If priors are specified AND _not_ using informative style, then assume using old style
  if ((length(prior_recs) > 0 || isTRUE(prior_subs)) && length(informative_priors) == 0){
    return(TRUE)
  }

  return(FALSE)
}
