#############################
# PRIVATE HELPER FUNCTIONS
# for submitting Stan models
#############################

#' Pull in Stan init file
#'
#' Sources the `-init.R` file associated with a model
#' and returns whatever is returned by the the `make_init()`
#' function it contains.
#'
#' @param .mod a `bbi_stan_model` object
#' @param .standata the data list that is returned from [build_data()] for this model
#'
#' @keywords internal
import_stan_init <- function(.mod, .standata) {
  # check for <run>-init.R file
  staninit_path <- build_path_from_model(.mod, STANINIT_SUFFIX)
  if (!fs::file_exists(staninit_path)) {
    message(paste("Using default initial values because no file found at", staninit_path))
    return(NULL)
  }

  # source and call function
  make_init <- safe_source_function(staninit_path, "make_init")
  init_res <- safe_call_sourced(
    .func = make_init,
    .args = list(.data = .standata),
    .file = staninit_path
  )

  # MAYBE FIRST DO SOME CHECKING?
  # that the returned value is actually something cmdstanr::sample() can take...
  # either a function or a list of lists that all have the same keys, etc.?
  return(init_res)
}


#' Parse args to be passed to cmdstanr
#'
#' Any arguments destined for [cmdstanr::sample()] will be passed in
#' via `...`. This function checks that those are valid and then writes
#' the resulting list of args to a file for reproducibility checking.
#'
#' @importFrom rlang list2
#' @importFrom digest digest
#'
#' @param .mod the `bbi_stan_model` object
#' @param .valid_stanargs A character vector of valid arguments to pass
#'   through to [cmdstanr::sample()]
#' @param ... The arguments to capture and check
#'
#' @return the named list of parsed and checked args
#' @keywords internal
parse_stanargs <- function(.mod, valid_stanargs, ...) {

  stanargs <- rlang::list2(...)
  if (length(stanargs) == 0) {
    return(stanargs)
  }

  if (any(names(stanargs) %in% STAN_RESERVED_ARGS)) {
    stop(paste(
      "Cannot pass any of the following through submit_model() to cmdstanr",
      glue("because they are parsed internally from the model object: {paste(STAN_RESERVED_ARGS, collapse = ', ')}")
    ))
  }

  invalid_stanargs <- setdiff(names(stanargs), valid_stanargs)
  if (length(invalid_stanargs) > 0) {
    stop(paste(
      "Attempting to pass invalid arguments through to Stan via submit_model(...)",
      "  The following are not accepted by cmdstanr::sample():",
      paste(invalid_stanargs, collapse = ", "),
      sep = "\n"
    ), call. = FALSE)
  }

  # reorder list and write to disk
  stanargs <- stanargs[order(names(stanargs))]
  dput(stanargs, build_path_from_model(.mod, STANARGS_SUFFIX))

  return(stanargs)
}


#' Private helper to compile a stan model and save a gitignore that ignores the
#' binary and posterior csv's
#' @keywords internal
compile_stanmod <- function(.mod) {
  # compile model
  stanmod <- cmdstanr::cmdstan_model(build_path_from_model(.mod, STANMOD_SUFFIX))

  # add to gitignore, if not already present
  gitignore <- file.path(.mod[[ABS_MOD_PATH]], ".gitignore")
  out_dir_str <- as.character(glue("{get_model_id(.mod)}-output/*csv"))

  if (!fs::file_exists(gitignore)) {
    readr::write_lines(paste(
      "# ignore model binary",
      get_model_id(.mod), "",
      "# ignore csv posterior output",
      out_dir_str,
      sep = "\n"
    ), gitignore)
  } else {
    gitignore_lines <- readr::read_lines(gitignore)
    # if either line is missing, append it
    if (!any(grepl(glue("^{get_model_id(.mod)}$"), gitignore_lines))) {
      readr::write_lines(glue("\n\n# ignore model binary\n{get_model_id(.mod)}"), gitignore, append = TRUE)
    }
    if (!any(grepl(out_dir_str, gitignore_lines, fixed = TRUE))) {
      readr::write_lines(glue("\n\n# ignore csv posterior output\n{out_dir_str}"), gitignore, append = TRUE)
    }
  }

  return(stanmod)
}

#' Private helper to save cmdstanr model fit object to RDS
#'
#' This intentionally does _not_ collect the posteriors with `$draws()` because
#' those are also saved to disk separately. This _does_ allow a user to reload
#' this object into memory though. (We will likely want a helper function to do
#' that. Maybe that's what `model_summary()` should do?...)
#' @param .stanmod a `cmdstanr` fit object of class `"CmdStanMCMC"`
#' @param .out_path path to save the model to. Within `submit_model()` we pass
#'   `build_path_from_model(.mod, STAN_MODEL_FIT_RDS)` but could be anywhere.
#' @keywords internal
save_fit_stanmod <- function(.stanmod, .out_path) {
  try(.stanmod$sampler_diagnostics(), silent = TRUE)
  try(.stanmod$init(), silent = TRUE)
  saveRDS(.stanmod, file = .out_path)
}


#' Build bbi_config.json for Stan models
#'
#' Contains information, including hashes and configuration,
#' for successfully run models.
#' @param .mod a `bbi_stan_model` object
#' @keywords internal
build_stan_bbi_config <- function(.mod, .write) {

  out_dir <- get_output_dir(.mod)
  data_path <- fs::path_rel(
    build_path_from_model(.mod, STANDATA_JSON_SUFFIX),
    start = out_dir
  )

  stan_config <- rlang::list2(
    "output_dir"          = out_dir,
    !!CONFIG_DATA_PATH   := data_path,
    !!CONFIG_MODEL_MD5   := tools::md5sum(get_model_path(.mod)),
    !!CONFIG_DATA_MD5    := tools::md5sum(build_path_from_model(.mod, STANDATA_JSON_SUFFIX)),
    !!STANCFG_DATA_MD5   := tools::md5sum(build_path_from_model(.mod, STANDATA_R_SUFFIX)),
    !!STANCFG_INIT_MD5   := tools::md5sum(build_path_from_model(.mod, STANINIT_SUFFIX)),
    !!STANCFG_ARGS_MD5   := tools::md5sum(build_path_from_model(.mod, STANARGS_SUFFIX)),
    "configuration" = rlang::list2(
      "cmdstan_version"     = cmdstanr::cmdstan_version(),
      "cmdstanr_version"    = as.character(utils::packageVersion('cmdstanr')),
    )
  )

  # write to disk
  stan_json <- jsonlite::toJSON(stan_config, pretty = TRUE, auto_unbox = TRUE)
  readr::write_lines(stan_json, file.path(get_output_dir(.mod), "bbi_config.json"))
}
