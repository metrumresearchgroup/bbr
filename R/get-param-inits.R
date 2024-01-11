
#' Retrieve and format the initial estimates
#'
#' @param .mod a `bbr` model object
#' @param flag_fixed Logical (`TRUE`/`FALSE`). If `TRUE`, return which
#'        parameters are fixed.
#'
#' @export
get_param_inits <- function(.mod, flag_fixed = FALSE){

  test_nmrec_version(.min_version = "0.3.0")
  check_model_object(.mod, "bbi_nonmem_model")


  ctl <- nmrec::read_ctl(get_model_path(.mod))

  # Set flags
  mark_flags <- if(isTRUE(flag_fixed)) "fix" else NULL

  # Get initial estimates
  theta_inits <- get_theta_inits(ctl, mark_flags = mark_flags)
  omega_inits <- nmrec::get_omega(ctl, mark_flags = mark_flags)
  sigma_inits <- nmrec::get_sigma(ctl, mark_flags = mark_flags)

  # TODO: consider formatting this better before returning
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
get_theta_inits <- function(ctl, mark_flags){

  # Tabulate initial values and bounds
  theta_inits <- purrr::map_dfc(c("init", "low", "up"), function(type){
    theta_inits <- nmrec::get_theta(ctl, type = type)
    tibble::tibble(!!rlang::sym(type) := theta_inits)
  })

  # Tabulate presence of FIXED parameters
  theta_inits_fixed <- nmrec::get_theta(ctl, mark_flags = mark_flags)
  theta_inits$fixed <- attr(theta_inits_fixed, "nmrec_flags")$fixed

  return(theta_inits)
}









