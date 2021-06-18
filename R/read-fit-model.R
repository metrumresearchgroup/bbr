#' Reads a fitted model object from disk
#'
#' For some a types of models, an object representing the "fitted" model is
#' persisted on disk when the model is run. This function will load that object
#' into memory and return it. **Note: this is currently only implemented for
#' `bbi_stan_model` objects.**
#'
#' @return
#' **Stan:** Returns a `cmdstanr` fit object of class `"CmdStanMCMC"`. See the
#' `?cmdstanr::CmdStanMCMC` docs for methods and information on this object.
#' _Note: currently [model_summary]`.bbi_stan_model()` calls this under the hood
#' because it contains methods to summarize model outputs and no similar methods
#' exist yet in `bbr` for Stan._
#'
#' @param .mod a `.bbi_{.model_type}_model` object
#' @param ... arguments passed through to methods (currently none).
#' @export
read_fit_model <- function(.mod, ...) {
  UseMethod("read_fit_model")
}

#' @describeIn read_fit_model Takes a file path that will be passed to
#'   [read_model()]. The loaded model is then passed directly to the relevant
#'   `read_fit_model()` dispatch.
#' @importFrom checkmate assert_string
#' @export
read_fit_model.character <- function(.mod, ...) {
  checkmate::assert_string(.mod)
  read_fit_model(read_model(.mod))
}

#' @describeIn read_fit_model Returns a `cmdstanr::CmdStanMCMC` object.
#' @export
read_fit_model.bbi_stan_model <- function(.mod, ...) {
  readRDS(build_path_from_model(.mod, STAN_MODEL_FIT_RDS))
}
