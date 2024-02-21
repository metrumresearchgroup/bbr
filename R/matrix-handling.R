
#' Check and modify a full matrix for positive-definiteness
#'
#' Splits a full matrix into sub-matrices per record, checks for positive-definiteness,
#' and attempts to find a near positive-definite matrix if not. If one cant be found,
#' the initial estimates shouldnt be tweaked in an automated fasion.
#'
#' @param full_mat initial estimates object as returned by `nmrec::extract_omega`
#'  or `nmrec::extract_sigma`. Should be a matrix with `nmrec_record_size` attributes.
#' @inheritParams tweak_initial_estimates
#'
#' @keywords internal
validate_matrix_pd <- function(full_mat, digits){

  record_type <- attr(full_mat, "record_type")
  mat_opts <- attr(full_mat, "mat_opts")

  # Separate full matrix into sub-matrices per record
  sub_mats <- get_sub_mat(full_mat)

  # Check for positive-definiteness, and attempt to make it positive-definite
  # if not (accounting for rounding). `SAME` records are skipped here.
  sub_mats_chkd <- purrr::imap(sub_mats, function(mat, rec_n){
    check_and_modify_pd(mat, mat_opt = mat_opts[rec_n,], digits = digits)
  })

  # If any are NULL, a positive-definite matrix could not be found
  # revert to original and message
  sub_mat_failed_pd <- purrr::map_lgl(sub_mats_chkd, is.null)
  if(any(sub_mat_failed_pd)){
    message(
      paste(
        "A record could not be rounded while ensuring positive-definiteness.",
        glue("Reverting to the original {record_type} record")
      )
    )
    # Replace NULL elements with original matrices
    sub_mats_chkd <- purrr::map2(sub_mats, sub_mats_chkd, function(mat, mat_chkd){
      if(is.null(mat_chkd)) return(mat) else return(mat_chkd)
    })
  }

  # Check that combined 'variance-covariance' matrix is positive-definite when rounding
  # We revert individual records that could not be made positive-definite above,
  # but here we need to check the combined matrix, regardless of what happened to
  # the sub-matrices. Must be checked in the variance-covariance domain.
  sub_mats_var <- purrr::imap(sub_mats_chkd, function(mat, rec_n){
    # make sub-matrices variance-covariance
    mod_matrix(mat, mat_opt = mat_opts[rec_n,])
  })

  # Diagonally concatenate values, while parsing `same` blocks
  mat_var_cat <- cat_mat_diag(sub_mats_var, mat_opts = mat_opts)
  # Round and make symmetric
  mat_var_cat <- signif(fmt_mat_zeros(mat_var_cat)$mat, digits)
  # Check that combined 'variance-covariance' matrix is positive-definite
  if(!is_positive_definite(mat_var_cat)){
    # If sub-matrices were all positive-definite, but combined matrix is not, we
    # need to look further into it. Shouldn't be possible.
    if(!any(sub_mat_failed_pd)){
      dev_error("Sub matrices were positive-definite, but the combined matrix was not.")
    }
    return(NULL)
  }

  # Diagonally concatenate actual sub matrices
  # Dont pass `same` vector here, as we want those indices to remain `NA`
  mat_cat <- cat_mat_diag(sub_mats_chkd) %>% signif(digits = digits)

  # Get indices of NAs from original matrix to preserve original NAs
  na_mat <- fmt_mat_zeros(full_mat)$na_indices
  mat_cat[na_mat] <- NA

  return(mat_cat)
}


#' Check and modify a matrix for positive-definiteness
#'
#' @param mat a symmetric matrix, or a matrix with only lower triangular values
#'  specified (assumed to be symmetric).
#' @param mat_opt tibble of matrix options. Should be a single row returned from
#'   `get_matrix_opts()`.
#' @inheritParams tweak_initial_estimates
#'
#' @keywords internal
check_and_modify_pd <- function(mat, mat_opt, digits = 3) {

  # Make sure only one record was passed in
  checkmate::assert_true(nrow(mat_opt) == 1)

  # Dont handle SAME records
  if(mat_opt$same) return(mat)

  # Change to variance-covariance matrix
  mat_var <- mod_matrix(mat, mat_opt = mat_opt)

  # Coerce NA values to 0 for checking positive-definiteness
  # pass in original matrix to determine placement of NA values
  mat_zero_spec <- fmt_mat_zeros(mat_var)
  mat_var_init <- mat_zero_spec$mat

  # Check if variance-covariance sub-matrix is positive-definite
  # Attempt to make positive-definite if not
  if (!is_positive_definite(signif(mat_var_init, digits))) {
    mat_var_init <- tryCatch({
      Matrix::nearPD(
        mat_var_init, base.matrix = TRUE,
        corr = FALSE, ensureSymmetry = TRUE)$mat
    }, error = function(cond) NULL)

    # Check for positive-definiteness when rounded
    if(is.null(mat_var_init) || !is_positive_definite(round(mat_var_init, digits))){
      return(NULL)
    }
  }

  # Revert matrix to original form and round
  mat_init <- mod_matrix(mat_var_init, mat_opt = mat_opt, inverse = TRUE) %>%
    signif(digits)

  # Reset original NA values
  mat_init[mat_zero_spec$na_indices] <- NA
  return(mat_init)
}


#' Modify a matrix to be a variance-covariance matrix, or the inverse operation
#'
#' @param mat a matrix
#' @inheritParams check_and_modify_pd
#' @param inverse Logical. If `TRUE`, perform the inverse operation. This assumes
#'  the matrix is currently a variance-covariance matrix.
#'
#' @details
#'
#' ### Example
#' **Setup:**
#' ```
#' # Get initial estimates
#' initial_est <- get_initial_est(.mod)
#'
#' # Get omega matrices from model (one matrix for each record)
#' sub_mats <- get_sub_mat(initial_est$omegas)
#'
#' # Get matrix options
#' mat_opts <- attr(initial_est$omegas, "mat_opts")
#' ```
#'
#' **Modify the matrices:**
#' ```
#' # Make sub-matrices variance-covariance
#' sub_mats_var <- purrr::imap(sub_mats, function(mat, rec_n){
#'   mod_matrix(mat, mat_opt = mat_opts[rec_n,])
#' })
#'
#' # Inverse operation
#' sub_mats_inv <- purrr::imap(mat_var_cat, function(mat, rec_n){
#'   mod_matrix(mat, mat_opt = mat_opts[rec_n,], inverse = TRUE)
#' })
#'
#' # Inverse operation returns matrices to original format
#' purrr::walk2(sub_mats_inv, sub_mats, function(mat, mat_orig){
#'   all.equal(mat_orig, mat)
#' })
#' ```
#'
#' @keywords internal
mod_matrix <- function(mat, mat_opt, inverse = FALSE){

  # Make sure only one record was passed in
  checkmate::assert_true(nrow(mat_opt) == 1)

  # Dont handle SAME records
  if(mat_opt$same) return(mat)

  # Get standard deviations of diagonals before any modifications
  diag_std_devs <- if(!inverse){
    if(mat_opt$diag == "standard") diag(mat) else sqrt(diag(mat))
  }else{
    if(mat_opt$diag == "standard") sqrt(diag(mat)) else diag(mat)
  }

  # Dont overwrite original matrix
  mat_mod <- mat

  if(mat_opt$diag == "cholesky"){
    if(!inverse){
      mat_zero_spec <- fmt_mat_zeros(mat)
      # Starting with a cholesky matrix
      # Set current NA values to 0 (if any)
      # - these matrices are NOT symmetric. Unspecified values are assumed to be 0
      mat_mod[is.na(mat_mod)] <- 0
      # Cholesky decomposition: `t(L) %*% L == mat`, where L is `chol(mat)`
      # We have lower triangular matrices, so we have to reverse the operation
      # i.e. transpose the second matrix, and not the first.
      mat_mod <- mat_mod %*% t(mat_mod)
      mat_mod[mat_zero_spec$na_indices] <- NA
    }else{
      mat_zero_spec <- fmt_mat_zeros(mat)
      # Starting with a variance-covariance matrix
      # Cholesky decomposition for -lower- triangular matrix
      mat_mod <- t(Matrix::chol(mat_zero_spec$mat))
      mat_mod[mat_zero_spec$na_indices] <- NA
    }
  }

  # Modify off-diagonal values between covariance and correlation
  if(mat_opt$off_diag == "correlation"){
    if(!inverse){
      # Set diagonals to 1 for correlation matrix. This will overwrite the diagonal,
      # but that's fine since we already tabulated the standard deviations via
      # the `diag_std_devs` object.
      diag(mat_mod) <- 1
      # convert correlation to covariance
      mat_zero_spec <- fmt_mat_zeros(mat_mod)
      mat_mod <- diag(diag_std_devs) %*% mat_zero_spec$mat %*% diag(diag_std_devs)
      # revert NAs
      mat_mod[mat_zero_spec$na_indices] <- NA
    }else{
      # convert covariance to correlation
      cov_mat <- stats::cov2cor(mat_mod)
      # only take the off-diagonals, since diagonals are never specified as correlation
      mat_mod[lower.tri(mat_mod)] <- cov_mat[lower.tri(cov_mat)]
    }
  }

  # Modify diagonal values between standard deviation and variance
  if(mat_opt$diag == "standard"){
    if(!inverse){
      diag(mat_mod) <- diag_std_devs^2
    }else{
      diag(mat_mod) <- diag_std_devs
    }
  }

  return(mat_mod)
}


#' Separate a matrix into sub matrices
#'
#' @inheritParams validate_matrix_pd
#'
get_sub_mat <- function(full_mat){
  sizes <- attr(full_mat, "nmrec_record_size")

  indices <- purrr::imap(sizes, function(.x, .y){
    mat_start <- if(.y > 1) sum(sizes[1:(.y-1)]) + 1 else 1
    c(mat_start, (mat_start+.x-1))
  })

  sub_matrices <- purrr::map(indices, function(.x){
    full_mat[.x[1]:.x[2], .x[1]:.x[2]]
  })

  return(sub_matrices)
}

#' Diagonally concatenate a list of matrices
#'
#' @param sub_mats a list of matrices to diagonally concatenate
#' @param mat_opts tibble of matrix options. Should include all rows from
#'   `get_matrix_opts()` for a given record type (i.e. have one row per element
#'   of `sub_mats`). If `NULL`, simply concatenate the matrices.
#'
#' @noRd
cat_mat_diag <- function(sub_mats, mat_opts = NULL){
  if(is.null(mat_opts)){
    as.matrix(Matrix::bdiag(sub_mats))
  }else{
    checkmate::assert_true(length(sub_mats) == nrow(mat_opts))

    sub_mats_new <- sub_mats
    for(rec_n in seq_along(sub_mats)){
      same.i <- mat_opts$same[rec_n]
      same_n.i <- mat_opts$same_n[rec_n]
      if(isTRUE(same.i) && rec_n == 1){
        stop("The first record of a given record type, cannot be a `SAME` block")
      }else if(isTRUE(same.i)){
        if(same_n.i == 1){
          sub_mats_new[[rec_n]] <- sub_mats_new[[rec_n-1]]
        }else{
          # Replicate previous matrix `same_n.i` number of times
          same_mats <- replicate(same_n.i, sub_mats_new[[rec_n-1]], simplify = FALSE)
          # Create combined `same` matrix
          sub_mats_new[[rec_n]] <- as.matrix(Matrix::bdiag(same_mats))
        }
      }
    }
    as.matrix(Matrix::bdiag(sub_mats_new))
  }
}


#' Check if matrix is positive-definite
#'
#' Assumes matrix is a square matrix, is symmetric, and contains only numeric
#' values
#'
#' @details
#' References:
#' https://github.com/TomKellyGenetics/matrixcalc/blob/master/R/is.positive.definite.R
#' https://www.geeksforgeeks.org/fixing-non-positive-definite-correlation-matrices-using-r/
#'
#' @param mat a square matrix
#'
#' @keywords internal
is_positive_definite <- function(mat){
  eigenvalues <- eigen(mat, only.values = TRUE)$values
  # Matrix::chol(mat) could also be a good check
  return(all(eigenvalues > 0))
}


#' Make matrix symmetric and format other NA values as zeros
#'
#' @inheritParams check_and_modify_pd
#'
#' @returns a list containing the formatted matrix, and original `NA` indices
#'
#' @keywords internal
fmt_mat_zeros <- function(mat){
  # Store *original* NA indices
  na_indices <- is.na(mat)
  # make matrix symmetric
  mat_sym <- make_mat_symmetric(mat)
  # Set current NA values to 0 (if any)
  mat_sym[is.na(mat_sym)] <- 0

  # Return new matrix and indices of NA values
  return(
    list(
      mat = mat_sym,
      na_indices = na_indices
    )
  )
}


#' Make a lower triangular matrix a symmetric one
#'
#' @inheritParams check_and_modify_pd
#'
#' @noRd
make_mat_symmetric <- function(mat){
  mat[upper.tri(mat)] <- t(mat)[upper.tri(mat)]
  return(mat)
}

#' Check if matrix is symmetric (with no tolerance)
#' NOT USED currently
#' @param mat a square matrix
#'
#' @noRd
mat_is_symmetric <- function(mat){
  Matrix::isSymmetric(mat, checkDN = FALSE, tol = 0)
}


