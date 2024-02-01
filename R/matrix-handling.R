
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

  # Separate full matrix into sub-matrices per record
  sub_mats <- get_sub_mat(full_mat)

  # Check for positive-definiteness, and attempt to make it positive-definite
  # if not (accounting for rounding)
  sub_mats_chkd <- purrr::map(sub_mats, function(mat){
    check_and_modify_pd(mat, digits = digits)
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
    sub_mats_chkd <- purrr::map2(sub_mats, sub_mats_chkd, function(mat, mat_chkd){
      if(is.null(mat_chkd)) return(mat) else return(mat_chkd)
    })
  }

  # Get indices of NAs from original matrix
  na_mat <- fmt_mat_zeros(full_mat)$na_indices

  # Diagonally concatenate sub matrices
  mat_cat <- as.matrix(Matrix::bdiag(sub_mats_chkd))

  # Check that combined matrix is positive-definite
  if(is_positive_definite(fmt_mat_zeros(mat_cat)$mat)){
    # Preserve original NAs
    mat_cat[na_mat] <- NA
    return(mat_cat)
  }else{
    if(!any(sub_mat_failed_pd)){
      dev_error("Sub matrices were positive-definite, but the combined matrix is not.")
    }
    return(NULL)
  }
}


#' Check and modify a matrix for positive-definiteness
#'
#' @param mat a symmetric matrix, or a matrix with only lower triangular values
#'  specified (assumed to be symmetric).
#' @param corr logical indicating if the matrix should be a correlation matrix.
#' @inheritParams tweak_initial_estimates
#'
#' @keywords internal
check_and_modify_pd <- function(mat, corr = FALSE, digits = 3) {

  # Coerce NA values to 0 for checking positive-definiteness
  # pass in original matrix to determine placement of NA values
  mat_zero_spec <- fmt_mat_zeros(mat)
  mat_init <- mat_zero_spec$mat

  # TODO: check if matrix is correlation or covariance matrix, and pass to nearPD
  if (!is_positive_definite(signif(mat_init, digits))) {
    mat_init <- Matrix::nearPD(
      mat_init, base.matrix = TRUE,
      corr = corr, ensureSymmetry = TRUE)$mat

    # Attempt to round matrix if it can still be positive-definite
    if(is_positive_definite(round(mat_init, digits))){
      mat_init <- signif(mat_init, digits)
    }else{
      return(NULL)
    }
  }

  # Reset original NA values
  mat_init[mat_zero_spec$na_indices] <- NA
  return(mat_init)
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
#'
#' @param mat a square matrix
#'
#' @noRd
mat_is_symmetric <- function(mat){
  Matrix::isSymmetric(mat, checkDN = FALSE, tol = 0)
}



get_matrix_types <- function(.mod){

  check_model_object(.mod, "bbi_nonmem_model")
  ctl <- nmrec::read_ctl(get_model_path(.mod))

  extract_mat_types <- function(ctl, type = c("omega", "sigma")){
    type <- match.arg(type)
    recs <- nmrec::select_records(ctl, type)

    purrr::imap_dfr(recs, function(rec, rec_num){
      rec$parse()
      rec_flags <- purrr::keep(rec$values, function(opts){
        inherits(opts, "nmrec_option_flag") && !inherits(opts, "nmrec_option_record_name")
      })

      rec_flag_names <- purrr::map_chr(rec_flags, function(rec_flag){
        rec_flag$name
      })

      # If no flags found, return defaults
      if(length(rec_flag_names) == 0){
        mat_types <- c("diag" = "variance", "off_diag" = "covariance")
      }else{
        # Handle diagonal
        if(str_detect(rec_flag_names, "standard")){
          mat_types <- c("diag" = "standard")
        }else{
          mat_types <- c("diag" = "variance")
        }

        # Handle off-diagonal
        if(str_detect(rec_flag_names, "correlation")){
          mat_types <- c(mat_types, "off_diag" = "correlation")
        }else{
          mat_types <- c(mat_types, "off_diag" = "covariance")
        }
      }

      c(record_type = type, record_number = rec_num, mat_types)
    })
  }

  bind_rows(
    extract_mat_types(ctl, "omega"),
    extract_mat_types(ctl, "sigma")
  )
}

