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
  # Order by row, column (must happen after `fmt_record_num`)
  omega_inits <- omega_inits %>% dplyr::arrange(.data$row, .data$col)
  # Optionally append fix column
  if(isTRUE(flag_fixed)){
    omega_inits <- omega_inits %>% dplyr::left_join(
      matrix_to_df(attr(initial_est$omegas, "nmrec_flags")$fixed, type = "omega") %>%
        dplyr::rename("fixed"="init"),
      by = c("parameter_names", "row", "col")
    )
  }

  # Format SIGMA
  sigma_inits <- matrix_to_df(initial_est$sigmas, type = "sigma")
  # Append record number
  sigma_inits$record_number <- fmt_record_num(initial_est$sigmas)
  # Order by row, column (must happen after `fmt_record_num`)
  sigma_inits <- sigma_inits %>% dplyr::arrange(.data$row, .data$col)
  # Optionally append fix column
  if(isTRUE(flag_fixed)){
    sigma_inits <- sigma_inits %>% dplyr::left_join(
      matrix_to_df(attr(initial_est$sigmas, "nmrec_flags")$fixed, type = "sigma") %>%
        dplyr::rename("fixed"="init"),
      by = c("parameter_names", "row", "col")
    )
  }

  # Combine
  initial_est_df <- dplyr::bind_rows(
    theta_inits %>% dplyr::mutate(record_type = "theta"),
    omega_inits %>% dplyr::mutate(record_type = "omega"),
    sigma_inits %>% dplyr::mutate(record_type = "sigma")
  )  %>% dplyr::select(-c("row", "col")) %>%
    dplyr::relocate("parameter_names") %>%
    dplyr::relocate("record_number", .after = "record_type")

  # Filter out NA values, as these were not specified in the control stream file
  initial_est_df <- initial_est_df %>% dplyr::filter(!is.na(.data$init))

  # Add original matrices as attributes if needed
  attr(initial_est_df, "omega_mat") <- initial_est$omegas
  attr(initial_est_df, "sigma_mat") <- initial_est$sigmas

  return(initial_est_df)
}


#' Retrieve initial parameter estimates
#'
#' @inheritParams initial_estimates
#'
#' @noRd
get_initial_est <- function(.mod, flag_fixed = FALSE){

  test_nmrec_version(.min_version = "0.4.0")
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

  # Get matrix types and options - assign as attributes
  mat_opts <- get_matrix_opts(.mod)
  attr(omega_inits, "mat_opts") <- mat_opts %>% filter(.data$record_type == "omega")
  attr(sigma_inits, "mat_opts") <- mat_opts %>% filter(.data$record_type == "sigma")

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
#' @param mark_flags 	A vector of NONMEM flags (i.e. valueless options such as
#'   FIXED or SD). For each specified flag, construct a boolean vector (for THETA)
#'   or matrix (for OMEGA and SIGMA) indicating whether the flag is "active" for
#'   the value. Any valid spelling of the flag name is allowed.
#'   See `?nmrec::extract_theta` for more details.
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
#' @noRd
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
    init = values,
    row = row_indices,
    col = col_indices
  )

  return(result_df)
}


#' Get options for matrix-type records
#'
#' @details
#' Returns a tibble indicating the record type and occurrence (`record_number`),
#' along with the matrix type (diagonal vs block), and value types for both
#' diagonal (variance vs standard deviation) and off-diagonal (covariance vs
#' correlation) options.
#'
#' ### Example:
#' ```
#' > get_matrix_opts(.mod)
#' # A tibble: 3 × 5
#' record_type record_number mat_type diag     off_diag
#' <chr>       <chr>         <chr>    <chr>    <chr>
#' 1 omega       1             diagonal variance covariance
#' 2 omega       2             block    standard correlation
#' 3 sigma       1             diagonal variance covariance
#' ```
#'
#' @inheritParams initial_estimates
#'
#' @keywords internal
get_matrix_opts <- function(.mod){

  check_model_object(.mod, "bbi_nonmem_model")
  ctl <- nmrec::read_ctl(get_model_path(.mod))

  # Function to grab flags
  get_flag_opts <- function(rec){
    purrr::keep(rec$values, function(opt){
      inherits(opt, "nmrec_option_flag") &&
        !inherits(opt, "nmrec_option_record_name")
    })
  }

  # Handling for nested options (diagonal matrix-type records)
  get_nested <- function(rec){
    purrr::keep(rec$values, function(opt){
      inherits(opt, "nmrec_option_nested")
    })
  }

  extract_mat_opts <- function(ctl, type = c("omega", "sigma")){
    type <- match.arg(type)
    recs <- nmrec::select_records(ctl, type)

    # Handling if record type doesn't exist
    if(length(recs) == 0){
      return(
        tibble::tibble(
          record_type = type, record_number = NA, mat_type = NA,
          "diag" = NA, "off_diag" = NA
        )
      )
    }

    mat_types <- get_matrix_types(recs)
    purrr::imap_dfr(recs, function(rec, rec_num){
      mat_type <- mat_types[rec_num]
      if(mat_type == "block"){
        rec_flags <- get_flag_opts(rec)
      }else if(mat_type == "diagonal"){
        rec$parse()
        nested_recs <- get_nested(rec)
        rec_flags <- unlist(purrr::map(nested_recs, get_flag_opts))
      }

      rec_flag_names <- purrr::map_chr(rec_flags, function(rec_flag){
        rec_flag$name
      })

      # If no flags found, return defaults
      if(length(rec_flag_names) == 0){
        mat_opts <- c("diag" = "variance", "off_diag" = "covariance")
      }else{

        # cholesky is applied to the full matrix
        if(any(str_detect(rec_flag_names, "cholesky"))){
          mat_opts <- c("diag" = "cholesky", "off_diag" = "cholesky")
        }else{
          # Handle diagonal
          if(any(str_detect(rec_flag_names, "standard"))){
            mat_opts <- c("diag" = "standard")
          }else{
            mat_opts <- c("diag" = "variance")
          }

          # Handle off-diagonal
          if(any(str_detect(rec_flag_names, "correlation"))){
            mat_opts <- c(mat_opts, "off_diag" = "correlation")
          }else{
            mat_opts <- c(mat_opts, "off_diag" = "covariance")
          }
        }
      }

      c(record_type = type, record_number = rec_num, mat_type = mat_type, mat_opts)
    })
  }

  bind_rows(
    extract_mat_opts(ctl, "omega"),
    extract_mat_opts(ctl, "sigma")
  )
}


#' Get matrix type for omega and sigma records
#'
#' @param records Either a list of records, or a single `nmrec_record` object
#'
#' @keywords internal
get_matrix_types <- function(records){
  if(!inherits(records, "list")) records <- list(records)

  purrr::map_chr(records, function(rec){
    rec$parse()
    rec_label <- purrr::keep(rec$values, function(rec_opt){
      inherits(rec_opt, "nmrec_option_value")
    })

    if(rlang::is_empty(rec_label)){
      return("diagonal")
    }else{
      rec_label[[1]]$name
    }
  })
}


#' Get record number of initial estimates
#'
#' @param inits initial estimates object as returned by `nmrec::extract_*`
#'  functions. Should be a vector or matrix with `nmrec_record_size` attributes.
#'
#' @details
#' `attr(inits, "nmrec_record_size")` stores a vector of sizes, one for each
#' occurrence of a given record type.
#' This function creates a vector indicating which record the initial
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


#' Check if *non-specific* priors are being used.
#'
#' The *non-specific* method of priors means using `$THETA` instead of `$THETAPV`
#' to specify `prior` records.
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

  # Check if SPECIFIC style is present
  prior_names <- c("thetap", "thetapv", "omegap", "omegapd", "sigmap", "sigmapd")
  specific_priors <- purrr::map(prior_names, function(.x){
    recs <- nmrec::select_records(ctl, .x)
    if(length(recs) == 0) return(NULL) else return(recs)
  })
  specific_priors <- Filter(Negate(is.null), specific_priors)

  # If priors are specified AND _not_ using specific style, then assume using old style
  if ((length(prior_recs) > 0 || isTRUE(prior_subs)) && length(specific_priors) == 0){
    return(TRUE)
  }

  return(FALSE)
}
