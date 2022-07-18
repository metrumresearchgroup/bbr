#' Return theta values as a named vector
#'
#' @param .summary  A `bbi_nonmem_summary` or `bbi_summary_list` object.
#' @export
get_theta <- function(.summary){

  if (inherits(.summary, "bbi_nonmem_summary")) {
    .summary <- list(.summary)
    check_model_object_list(.summary, .mod_types = c(NM_SUM_CLASS))
  }else if(inherits(.summary, "bbi_summary_list")){
    check_model_object(.summary, .mod_types = c("bbi_summary_list"))
  }

  thetas <- map(.summary, ~ {
    # unpack bbi_summary_list element
    if (!is.null(.x$bbi_summary)) .x <- .x$bbi_summary
    param_names <- .x[[SUMMARY_PARAM_NAMES]]
    theta <- .x[[SUMMARY_PARAM_DATA]][[length(.x[[SUMMARY_PARAM_DATA]])]]$estimates$theta
    names(theta) <- param_names$theta
    theta
  })


  if(length(thetas) == 1) thetas <- thetas[[1]]

  return(thetas)
}


#' Return omega values as a matrix
#'
#' @param .summary  A `bbi_nonmem_summary` or `bbi_summary_list` object.
#' @export
get_omega <- function(.summary){

  if (inherits(.summary, "bbi_nonmem_summary")) {
    .summary <- list(.summary)
    check_model_object_list(.summary, .mod_types = c(NM_SUM_CLASS))
  }else if(inherits(.summary, "bbi_summary_list")){
    check_model_object(.summary, .mod_types = c("bbi_summary_list"))
  }

  omegas <- map(.summary, ~ {
    # unpack bbi_summary_list element
    if (!is.null(.x$bbi_summary)) .x <- .x$bbi_summary

    param_names <- .x[[SUMMARY_PARAM_NAMES]]
    omega <- with(
      .x[[SUMMARY_PARAM_DATA]][[length(.x[[SUMMARY_PARAM_DATA]])]],
      format_matrix(estimates$omega, param_names$omega, .type = "OMEGA")
    )
    omega
  })


  if(length(omegas) == 1) omegas <- omegas[[1]]

  return(omegas)
}


#' Return sigma values as a matrix
#'
#' @param .summary  A `bbi_nonmem_summary` or `bbi_summary_list` object.
#' @export
get_sigma <- function(.summary){

  if (inherits(.summary, "bbi_nonmem_summary")) {
    .summary <- list(.summary)
    check_model_object_list(.summary, .mod_types = c(NM_SUM_CLASS))
  }else if(inherits(.summary, "bbi_summary_list")){
    check_model_object(.summary, .mod_types = c("bbi_summary_list"))
  }

  sigmas <- map(.summary, ~ {
    # unpack bbi_summary_list element
    if (!is.null(.x$bbi_summary)) .x <- .x$bbi_summary

    param_names <- .x[[SUMMARY_PARAM_NAMES]]
    sigma <- with(
      .x[[SUMMARY_PARAM_DATA]][[length(.x[[SUMMARY_PARAM_DATA]])]],
      format_matrix(estimates$sigma, param_names$sigma, .type = "SIGMA")
    )
    sigma
  })


  if(length(sigmas) == 1) sigmas <- sigmas[[1]]

  return(sigmas)
}


#' Format vector into matrix using positional vector
#'
#' @details
#' The only information gathered from `.labels` is the positioning (i.e. (2,3), (1,1), etc.)
#'
#' @param .values vector of values. Values should be in the same order presented in model_summary
#' @param .labels parameter names corresponding to the values. Order matters here as well.
#' @keywords internal
format_matrix <- function(.values, .labels, .type = c("OMEGA", "SIGMA")){

  .type <- match.arg(.type)

  # Get positions from labels
  matrix_pos <- gsub(.type, "",.labels)
  matrix_pos <- gsub("[()]", "", matrix_pos)
  matrix_pos <- strsplit(matrix_pos, ",")
  .rows <- map(matrix_pos, function(.x){ .x[1] }) %>% unlist() %>% as.numeric()
  .cols <- map(matrix_pos, function(.x){ .x[2] }) %>% unlist() %>% as.numeric()

  # Assign values to corresponding location
  .mat <- matrix(rep(0,max(.rows, .cols)*2), nrow = max(.rows), ncol = max(.cols))
  for(i in 1:length(.rows)){
    .mat[.rows[i],.cols[i]] <- .values[i]
    .mat[.cols[i],.rows[i]] <- .values[i]
  }

  # Assign row and column names



  dimnames(.mat) <- list(
    sprintf(paste0("%s_",seq(1,max(.cols)),""), .type),
    sprintf(paste0("%s_",seq(1,max(.rows)),""), .type)
  )

  return(.mat)
}

