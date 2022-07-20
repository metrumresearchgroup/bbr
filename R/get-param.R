
#' Return a formatted object for a given parameter
#'
#' @param .summary a `bbi_nonmem_summary` or `bbi_summary_list` object.
#' @param .param the parameter(s) to retrieve and format. Any subset of `c("omega", "sigma", "theta")`
#'
#' @importFrom stats setNames
#' @keywords internal
get_param <- function(.summary, .param = c("omega", "sigma", "theta")){

  if (inherits(.summary, NM_SUM_CLASS)) {
    .summary <- list(.summary)
    check_model_object_list(.summary, .mod_types = c(NM_SUM_CLASS))
  }else if(inherits(.summary, SL_CLASS)){
    check_model_object(.summary, .mod_types = c(SL_CLASS))
  }else{
    stop("Please pass in a `", NM_SUM_CLASS, "` or `", SL_CLASS, "` type object.")
  }

  assert_true(all(.param %in% c("omega", "sigma", "theta")))

  param_obj <- map(.param, function(param.i){

    if(param.i %in% c("omega", "sigma")){
      param_obj_i <- map(.summary, ~ {
        # unpack bbi_summary_list element
        if (!is.null(.x$bbi_summary)) .x <- .x$bbi_summary

        param_names <- .x[[SUMMARY_PARAM_NAMES]]
        estimates <- .x[[SUMMARY_PARAM_DATA]][[length(.x[[SUMMARY_PARAM_DATA]])]]$estimates
        format_matrix(estimates[[param.i]], param_names[[param.i]], .type = toupper(param.i))
      })

    }else if(param.i %in% c("theta")){
      param_obj_i <- map(.summary, ~ {
        # unpack bbi_summary_list element
        if (!is.null(.x$bbi_summary)) .x <- .x$bbi_summary

        param_names <- .x[[SUMMARY_PARAM_NAMES]]
        theta <- .x[[SUMMARY_PARAM_DATA]][[length(.x[[SUMMARY_PARAM_DATA]])]]$estimates$theta
        names(theta) <- param_names$theta
        theta
      })
    }

    if(length(param_obj_i) == 1) param_obj_i <- param_obj_i[[1]]
    param_obj_i
  }) %>% setNames(.param)

  return(param_obj)
}



#' Return a formatted object for a given parameter
#'
#' @description
#'
#' * [get_omega()]: Return omega values as a labeled symmetric matrix
#' * [get_sigma()]: Return sigma values as a labeled symmetric matrix
#' * [get_theta()]: Return theta values as a named vector
#'
#' @param .summary a `bbi_nonmem_summary` or `bbi_summary_list` object.
#'
#' @section Examples:
#'
#' ```{r, include = FALSE}
#' knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
#' options('bbr.bbi_exe_path' = read_bbi_path())
#' MODEL_DIR <- system.file("model", "nonmem", "complex",   package = "bbr")
#' mod <- read_model(file.path(MODEL_DIR, "example2_saemimp"))
#' ```
#' ```{r, comment = "#>", collapse = TRUE}
#' sum <- mod %>% model_summary()
#'
#' sum %>% get_omega()
#'
#'
#' sum %>% get_sigma()
#'
#'
#' sum %>% get_theta()
#' ```
#'
#'
#' @export
get_omega <- function(.summary){

  omegas <- get_param(.summary, "omega")$omega

  return(omegas)
}


#' @rdname get_omega
#' @export
get_sigma <- function(.summary){

  sigmas <- get_param(.summary, "sigma")$sigma

  return(sigmas)
}

#' @rdname get_omega
#' @export
get_theta <- function(.summary){

  thetas <- get_param(.summary, "theta")$theta

  return(thetas)
}


#' Format vector into matrix using positional vector
#'
#' @details
#' The only information gathered from `.labels` is the positioning (i.e. (2,3), (1,1), etc.)
#'
#' @param .values vector of values. Values should be in the same order presented in model_summary
#' @param .labels parameter names corresponding to the values. Order matters here as well.
#' @param .type matrix type. Either "OMEGA" or "SIGMA"
#'
#' @importFrom glue glue
#'
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
    glue("{.type}_{seq(1,max(.cols))}"),
    glue("{.type}_{seq(1,max(.rows))}")
  )

  return(.mat)
}
