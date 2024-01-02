#' Inherit parameter estimates
#'
#' Set the initial parameter estimates of a model using the estimates of a
#' previously executed model. Often this is used to carry forward the final
#' estimates of a "parent" model to be used as the initial estimates of the
#' "child" model, for example a model created with [copy_model_from()].
#'
#' @param .mod model object to update.
#' @param .parent_mod Either a model object, or path to a model to inherit
#'   properties from.
#' @param inherit type of estimates to inherit from parent model. Defaults to
#'   replacing all of THETA, SIGMA, and OMEGA
#' @param bounds Whether to keep or discard the existing bounds when setting
#'   the initial estimates in THETA records.
#' @param digits Number of significant digits to round estimates to.
#'
#'
#' @details
#'  ## Constraints and limitations
#'
#'  * These functions update initial estimates only if they are **explicitly**
#'    defined in the control stream.
#'
#'    For example, consider the update of `$THETA (1)x4`. That defines four
#'    initial estimates, but only the first explicitly appears. Calling
#'    `inherit_param_estimates()` with theta estimates `c(5, 6, 7, 8)` would
#'    return a result of `(5)x4`.
#'
#'   * Using additional parameter records for priors is not supported and will
#'   lead to a size mismatch between the parameter and its records. Instead use
#'   informative prior record names (such as `THETAP` and `THETAPV`).
#'
#' @examples
#'
#' \dontrun{
#' base_mod <- read_model(file.path(MODEL_DIR, "1"))
#'
#' mod2 <- copy_model_from(base_mod, "mod2") %>%
#'   inherit_param_estimates()
#'
#' }
#' @export
inherit_param_estimates <- function(
    .mod,
    .parent_mod = get_based_on(.mod)[1],
    inherit = c("theta", "sigma", "omega"),
    bounds = c("keep", "discard"),
    digits = 3
){

  test_nmrec_version(.min_version = "0.3.0")

  bounds <- match.arg(bounds)

  check_model_object(.mod, "bbi_nonmem_model")
  checkmate::assert_true(all(inherit %in% BBR_ESTIMATES_INHERIT))

  if(!inherits(.parent_mod, "bbi_nonmem_model")){
    # Confirm .parent_mod path is valid
    validate_parent_mod(.parent_mod)
    .parent_mod <- read_model(.parent_mod)
  }

  # Inherit model objects
  mod_path <- get_model_path(.mod)
  mod_lines <- nmrec::read_ctl(mod_path)

  # Parent model objects
  based_on_sum <- model_summary(.parent_mod)


  fmt_digits <- paste0("%.",digits,"G")

  # Update THETA Block
  if("theta" %in% inherit){
    new_thetas <- based_on_sum %>% get_theta() %>% signif(digits = digits) %>% unname()
    nmrec::set_theta(
      mod_lines, values = new_thetas, bounds = bounds,
      fmt = fmt_digits
    )
  }

  # Update OMEGA Block
  if("omega" %in% inherit){
    new_omegas <- based_on_sum %>% get_omega() %>% signif(digits = digits)
    nmrec::set_omega(
      mod_lines, values = new_omegas, representation = "reset",
      fmt = fmt_digits
    )
  }

  # Update SIGMA Block
  if("sigma" %in% inherit){
    new_sigmas <- based_on_sum %>% get_sigma() %>% signif(digits = digits)
    nmrec::set_sigma(
      mod_lines, values = new_sigmas, representation = "reset",
      fmt = fmt_digits
    )
  }

  # Write out mod_lines to new model
  nmrec::write_ctl(mod_lines, mod_path)

  return(.mod)
}


#' Confirm parent model path is valid, and error informatively if not
#'
#' @param .parent_mod Path to a parent model to inherit properties from.
#'
#' @keywords internal
validate_parent_mod <- function(.parent_mod){
  fmt_error <- function(msg) gsub("\n", " ", stringr::str_wrap(msg, width = 100))
  if(is.null(.parent_mod) || !fs::file_exists(.parent_mod)){
    mod_exists <- fs::file_exists(ctl_ext(.parent_mod)) ||
      fs::file_exists(mod_ext(.parent_mod))

    msg_info <- c("i" = "To inherit parameter estimates from a parent model,
                  `.parent_mod` must be a file path to a previously executed model.")
    if(is.null(.parent_mod)){
      # If the model wasnt created via copy_model_from, or the `based_on` field
      # is otherwise empty
      msg_prefix <- glue::glue("`get_based_on(.mod)` did not return any parent models.
                 Please specify `.parent_mod` directly, or update the `based_on`
                 attribute of .mod. See ?add_based_on for more details.")
      msg <- c("x" = msg_prefix)
    }else if(mod_exists && !fs::dir_exists(.parent_mod)){
      msg_prefix <- glue::glue("Parent model ({basename(ctl_ext(.parent_mod))}) exists,
                 but has not been executed.")
      msg <- c("x" = msg_prefix, msg_info)
    }else{
      msg_prefix <- glue::glue("Parent model does not exist at: {.parent_mod}")
      msg <- c("x" = msg_prefix, msg_info)
    }

    rlang::abort(fmt_error(msg))
  }

  return(invisible(TRUE))
}

