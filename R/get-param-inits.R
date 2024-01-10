
#' Retrieve and format the initial estimates
#'
#' @param .mod
#'
#' @export
get_param_inits <- function(.mod){

  test_nmrec_version(.min_version = "0.3.0")
  check_model_object(.mod, "bbi_nonmem_model")


  ctl <- nmrec::read_ctl(get_model_path(.mod))

  recs <- list(
    thetas = nmrec:::create_param_index(ctl, "theta"),
    omegas = nmrec:::create_param_index(ctl, "omega"),
    sigmas = nmrec:::create_param_index(ctl, "sigma")
  )

  recs_values <- list(
    theta = fmt_record_values(param_index = recs$thetas),
    omega = fmt_record_values(param_index = recs$omegas),
    sigma = fmt_record_values(param_index = recs$sigmas)
  )


  return(recs_values)
}


#' Format the initial estimates for each record
#'
#' Matrix type records will be formatted as a block diagonal matrix. Multiple
#' records of the same type will be concatenated diagonally.
#'
#' @param param_index object returned from `create_param_index`.
#'
#' @keywords internal
fmt_record_values <- function(param_index){

  details <- param_index$details
  records <- param_index$records
  name <- param_index$name

  # Only grab initial values for theta bounds
  val_recs <- purrr::map(details, function(detail){
    val_rec <- purrr::map_dfr(detail$popts, function(.x){
      vals <- purrr::keep(.x$values, function(x_vals){
        inherits(x_vals, "nmrec_option")
      })

      purrr::map_dfr(vals, function(val){
        tibble::tibble(name = val$name, value = as.character(val$value))
      }) %>% tidyr::pivot_wider()
    })

    if (!identical(name, "theta")) {
      len_expected <- detail$size
      val_rec <- vector_to_matrix_ltri(
        as.numeric(val_rec$init), n = len_expected, type = detail$type
      )
    }
    return(val_rec)
  })

  # TODO: filter out or error if prior records found before this step/function as a whole
  if (identical(name, "theta")) {
    val_recs_fmt <- val_recs %>% purrr::list_rbind()  %>% dplyr::mutate(index = 1:n())
  }else{
    val_recs_fmt <- list(
      matrices = val_recs,
      fixed = purrr::map(records, matrix_is_fixed)
    )

    # Create combined matrix if multiple records found
    if(length(val_recs) != 1){
      val_recs_fmt$full_matrix <- as.matrix(Matrix::bdiag(val_recs))
    }
  }

  return(val_recs_fmt)
}


#' Format vector as a lower diagonal matrix
#'
#' @param vec a vector in row-major order of values.
#' @param n matrix dimension.
#' @param type type of matrix. Either "diagonal" or "block".
#'
#' @keywords internal
vector_to_matrix_ltri <- function(vec, n, type = c("diagonal", "block")) {
  type <- match.arg(type)
  mat <- matrix(0, nrow = n, ncol = n)
  if(type == "diagonal"){
    diag(mat) <- vec
  }else{
    mat[upper.tri(mat, diag = TRUE)] <- vec
  }
  return(t(mat))
}


matrix_is_fixed <- function(.record){
  any_fixed <- purrr::some(.record$values, function(rec_opt){
    if(inherits(rec_opt, "nmrec_option_nested")){
      # Handling for diagonal matrices
      purrr::some(rec_opt$values, function(rec_opt){
        inherits(rec_opt, "nmrec_option_flag") && rec_opt$name == "fixed"
      })
    }else{
      # Handling for block matrices
      inherits(rec_opt, "nmrec_option_flag") && rec_opt$name == "fixed"
    }
  })
}
