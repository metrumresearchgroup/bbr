
#' start the callr::r_bg() process that will submit the batches
#'
#' @details
#' Make sure to set `Sys.setenv('BBR_DEV_LOAD_PATH' = here::here())`
#' if you are in a development setting. This will allow package functions to be
#' accessible in the background process, which is not an issue when the package
#' is installed.
#' @inheritParams submit_model
#' @importFrom callr r_bg
#' @param stdout_path Path to a write stdout and stderr from the background
#'   process
#' @noRd
submit_batch_callr <- function (
    .mods,
    .batch_size,
    .bbi_args,
    .mode,
    .overwrite,
    .config_path,
    stdout_path
) {
  verbose_msg(glue("submitting {length(.mods)} models in batches of {.batch_size}"))

  # Check for env var in development setting (see @details above)
  env <- callr::rcmd_safe_env()
  load_path <- Sys.getenv("BBR_DEV_LOAD_PATH")
  if (nzchar(load_path)) {
    env <- c(env, "BBR_DEV_LOAD_PATH" = load_path)
  }

  callr::r_bg(
    batch_submit_bg,
    args = list(
      .mods = .mods,
      .batch_size = .batch_size,
      .bbi_args = .bbi_args,
      .mode = .mode,
      .overwrite = .overwrite,
      .config_path = .config_path,
      bbi_exe_path = getOption("bbr.bbi_exe_path")
    ),
    env = env,
    error = "error",
    stdout = stdout_path,
    stderr = "2>&1",
    package = "bbr",
    supervise = FALSE,
    cleanup = FALSE
  )
}


#' Submit in batches, from a background process
#' @inheritParams submit_batch_callr
#' @param bbi_exe_path Pulled from getOption('bbr.bbi_exe_path') in the caller
#'   and passed through
#' @noRd
batch_submit_bg <- function(
    .mods,
    .batch_size,
    .bbi_args,
    .mode,
    .overwrite, # TODO: should handle this differently, especially for restarts
    .config_path,
    bbi_exe_path
) {

  message(paste("Running bbr batch submission in pid", Sys.getpid()))
  message(paste("#####", Sys.time(), "#####"))

  # For Development Environment, must load the package
  load_path <- Sys.getenv("BBR_DEV_LOAD_PATH")
  if (nzchar(load_path)) {
    message("Loading ", load_path)
    devtools::load_all(load_path)
  }

  options("bbr.bbi_exe_path" = bbi_exe_path)

  submitted_i <- c()
  to_submit_i <- 1:.batch_size

  while (TRUE) {
    if (!is.null(to_submit_i)) {
      res <- submit_models(
        .mods[to_submit_i], .bbi_args, .mode,
        .overwrite = .overwrite, .config_path = .config_path,
        .wait = FALSE
      )
      batch_submit_iter_msg(.mods[to_submit_i])

      # update vector of submitted models
      submitted_i <- c(submitted_i, to_submit_i)
    }

    # check on submitted models, break if all done
    Sys.sleep(3) ### avoid missing .lst file error
    message(paste("#####", Sys.time(), "#####"))
    stat_res <- get_model_status(.mods[submitted_i])
    if (sum(stat_res$finished) == length(.mods)) {
      verbose_msg(glue("Finished {length(.mods)} models from batch submission"))
      break
    }

    # check if there are more models to submit
    if (length(submitted_i) == length(.mods)) {
      to_submit_i <- NULL
    } else {
      # if fewer models running than .batch_size, submit more
      shortfall <- .batch_size - sum(!stat_res$finished)
      if (shortfall > 0) {
        most_recent <- max(submitted_i)
        to_submit_i <- (most_recent + 1):min(length(.mods), (most_recent + shortfall))
      } else {
        to_submit_i <- NULL
      }
    }

    # wait 30 seconds before checking again
    Sys.sleep(27)
  }
}

batch_submit_iter_msg <- function(mod_list) {
  mod_names <- purrr::map(mod_list, get_model_id)
  name_msg <- ifelse(
    length(mod_names) > 1,
    glue::glue("{mod_names[1]} through {mod_names[length(mod_names)]}"),
    mod_names
  )

  message(glue(" - model(s) {name_msg}"))
}
