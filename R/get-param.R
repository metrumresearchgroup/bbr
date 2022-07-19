
#' Return a formatted object for a given parameter
#'
#' @param .summary a `bbi_nonmem_summary` or `bbi_summary_list` object.
#' @param .param the parameter to retrieve and format. One of `c("omega", "sigma", "theta")`
#'
#' @export
get_param <- function(.summary, .param = c("omega", "sigma", "theta")){

  if (inherits(.summary, NM_SUM_CLASS)) {
    .summary <- list(.summary)
    check_model_object_list(.summary, .mod_types = c(NM_SUM_CLASS))
  }else if(inherits(.summary, SL_CLASS)){
    check_model_object(.summary, .mod_types = c(SL_CLASS))
  }else{
    stop("Please pass in a `", NM_SUM_CLASS, "` or `", SL_CLASS, "` type object.")
  }

  .param <- match.arg(.param)

  if(.param %in% c("omega", "sigma")){
    param_obj <- map(.summary, ~ {
      # unpack bbi_summary_list element
      if (!is.null(.x$bbi_summary)) .x <- .x$bbi_summary

      param_names <- .x[[SUMMARY_PARAM_NAMES]]
      matrix <- with(
        .x[[SUMMARY_PARAM_DATA]][[length(.x[[SUMMARY_PARAM_DATA]])]],
        format_matrix(estimates[[.param]], param_names[[.param]], .type = toupper(.param))
      )
      matrix
    })

  }else if(.param %in% c("theta")){
    param_obj <- map(.summary, ~ {
      # unpack bbi_summary_list element
      if (!is.null(.x$bbi_summary)) .x <- .x$bbi_summary

      param_names <- .x[[SUMMARY_PARAM_NAMES]]
      theta <- .x[[SUMMARY_PARAM_DATA]][[length(.x[[SUMMARY_PARAM_DATA]])]]$estimates$theta
      names(theta) <- param_names$theta
      theta
    })
  }

  if(length(param_obj) == 1) param_obj <- param_obj[[1]]

  return(param_obj)
}


#' @describeIn get_param Return theta values as a named vector
#' @export
get_theta <- function(.summary){

  thetas <- get_param(.summary, "theta")

  return(thetas)
}



#' @describeIn get_param Return omega values as a labeled symmetric matrix
#' @export
get_omega <- function(.summary){

  omegas <- get_param(.summary, "omega")

  return(omegas)
}


#' @describeIn get_param Return sigma values as a labeled symmetric matrix
#' @export
get_sigma <- function(.summary){

  sigmas <- get_param(.summary, "sigma")

  return(sigmas)
}


#' Format vector into matrix using positional vector
#'
#' @details
#' The only information gathered from `.labels` is the positioning (i.e. (2,3), (1,1), etc.)
#'
#' @param .values vector of values. Values should be in the same order presented in model_summary
#' @param .labels parameter names corresponding to the values. Order matters here as well.
#' @param .type matrix type. Either "OMEGA" or "SIGMA"
#' @keywords internal
format_matrix <- function(.values, .labels, .type = c("OMEGA", "SIGMA")){

  .type <- match.arg(.type)

  # Get positions from labels
  matrix_pos <- gsub(.type, "",.labels)
  matrix_pos <- gsub("[()]", "", matrix_pos)
  matrix_pos <- strsplit(matrix_pos, ",")
  .rows <-  map(matrix_pos, 1) %>% unlist() %>% as.numeric()
  .cols <- map(matrix_pos, 2) %>% unlist() %>% as.numeric()

  # Assign values to corresponding location
  .mat <- matrix(rep(0,max(.rows, .cols)*2), nrow = max(.rows), ncol = max(.cols))
  for(i in seq_along(.rows)){
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

