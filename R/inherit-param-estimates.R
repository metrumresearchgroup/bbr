#' Inherit parameter estimates
#'
#' Set the initial parameter estimates of a model using the estimates of a
#' previously executed model.
#'
#' @param .mod new model object to overwrite.
#' @param .parent_mod Either a model object, or path to a model to inherit properties from.
#' @param .inherit type of estimates to inherit from parent model.
#' @param .bounds Whether to keep or discard the existing bounds when setting
#'   the initial estimates in THETA records.
#' @param .representation Whether to keep alternative representation options like
#'   SD and CORRELATION or reset to the default representation (variance and
#'   covariance).
#'
#'   For `OMEGA` and `SIGMA` records, `NONMEM` supports specifying diagonal and
#'   off-diagonal initial estimates in a different representation than the
#'   default, variance and covariance. If `values` are the final estimates from
#'   a previous NONMEM run, the alternative representation options should be
#'   discarded because NONMEM always outputs variances and covariances.
#' @param .digits Number of significant digits to round estimates to.
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
#'
#' @export
inherit_param_estimates <- function(
    .mod,
    .parent_mod = get_based_on(.mod),
    .inherit = c("theta", "sigma", "omega"),
    .bounds = c("keep", "discard"),
    .representation = c("keep", "reset"),
    .digits = 3
){

  test_nmrec_version(.min_version = "0.2.0.8000")

  .bounds <- match.arg(.bounds)

  checkmate::assert_true(all(.inherit %in% BBR_ESTIMATES_INHERIT))

  if(inherits(.parent_mod, "bbi_nonmem_model")){
    parent_mod_path <- .parent_mod$absolute_model_path
  }else{
    parent_mod_path <- .parent_mod
    # Confirm .parent_mod path is valid
    validate_parent_mod(parent_mod_path)
    .parent_mod <- read_model(parent_mod_path)
  }

  # Inherit model objects
  .mod_path <- .mod$absolute_model_path
  mod_lines <- nmrec::read_ctl(ctl_ext(.mod_path))

  # Parent model objects
  based_on_sum <- model_summary(.parent_mod)


  fmt_digits <- paste0("%.",.digits,"G")

  # Update THETA Block
  if("theta" %in% .inherit){
    new_thetas <- based_on_sum %>% get_theta() %>% signif(digits = .digits) %>% unname()
    nmrec::set_theta(
      mod_lines, values = new_thetas, bounds = .bounds,
      fmt = fmt_digits
    )
  }

  # Update OMEGA Block
  if("omega" %in% .inherit){
    new_omegas <- based_on_sum %>% get_omega() %>% signif(digits = .digits)
    nmrec::set_omega(
      mod_lines, values = new_omegas, representation = .representation,
      fmt = fmt_digits
    )
  }

  # Update SIGMA Block
  if("sigma" %in% .inherit){
    new_sigmas <- based_on_sum %>% get_sigma() %>% signif(digits = .digits)
    nmrec::set_sigma(
      mod_lines, values = new_sigmas, representation = .representation,
      fmt = fmt_digits
    )
  }

  # Write out mod_lines to new model
  nmrec::write_ctl(mod_lines, ctl_ext(.mod_path))

  return(.mod)
}


#' Confirm parent model path is valid, and error informatively if not
#'
#' @param .parent_mod Path to a parent model to inherit properties from.
#'
#' @keywords internal
validate_parent_mod <- function(.parent_mod){
  if(is.null(.parent_mod) || !fs::file_exists(.parent_mod)){
    mod_exists <- fs::file_exists(ctl_ext(.parent_mod)) ||
      fs::file_exists(mod_ext(.parent_mod))

    msg_info <- c("i" = "To inherit parameter estimates from a parent model, {.code .parent_mod}
        must be a file path to a previously executed model.")
    if(is.null(.parent_mod)){
      # If the model wasnt created via copy_model_from, or the `based_on` field
      # is otherwise empty
      msg_prefix <- glue::glue("{.code get_based_on(.mod)} did not return any parent models.
                 Please specify {.code .parent_mod} directly, or update the {.emph based_on}
                 attribute of `.mod`. See {.code ?add_based_on} for details.",
                 .open = "{{", .close = "}}")
      msg <- c("x" = msg_prefix)
    }else if(mod_exists && !fs::dir_exists(.parent_mod)){
      msg_prefix <- glue::glue("Parent model ({.code {{basename(ctl_ext(.parent_mod))}}}) exists,
                 but {.emph has not been executed.}", .open = "{{", .close = "}}")
      msg <- c("x" = msg_prefix, msg_info)
    }else{
      msg_prefix <- glue::glue("Parent model does not exist at: {.emph {{.parent_mod}}}", .open = "{{", .close = "}}")
      msg <- c("x" = msg_prefix, msg_info)
    }

    cli::cli_div(theme = list(span.emph = list(color = "red"), span.code = list(color = "blue")))
    cli::cli_abort(msg)
  }

  return(invisible(TRUE))
}

