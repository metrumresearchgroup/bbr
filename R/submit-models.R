
#########################################################
# S3 dispatches for submitting multiple models in batch
#########################################################

#' Submit models to be run in batch
#'
#' Submits a group of models to be run in batch by calling out to `bbi` in as
#' few external calls as possible (see "Details").
#'
#' @details The number of `bbi` calls to make is determined by the number of
#' distinct sets of `bbi` arguments passed to the submission calls, either
#' explicitly through `.bbi_args`, as specified in the `bbi_args` field of the
#' model YAML, or specified globally in `bbi.yaml`.
#'
#' @template nonmem-mod-ext
#'
#' @seealso [submit_model()]
#' @param .mods The model objects to submit.
#' @inheritParams submit_model
#' @export
submit_models <- function(
  .mods,
  .bbi_args = NULL,
  .mode = getOption("bbr.bbi_exe_mode"),
  ...,
  .overwrite = NULL,
  .config_path = NULL,
  .wait = TRUE,
  .dry_run=FALSE
) {
  UseMethod("submit_models")
}

#' @describeIn submit_models Takes a list of `bbi_base_model` objects.
#' @importFrom purrr map map_lgl
#' @export
submit_models.list <- function(
  .mods,
  .bbi_args = NULL,
  .mode = getOption("bbr.bbi_exe_mode"),
  ...,
  .overwrite = NULL,
  .config_path = NULL,
  .wait = TRUE,
  .dry_run=FALSE
) {
  # check that each element is a model object
  check_model_object_list(.mods)

  # check that all are the same type of model object
  all_model_types <- map(.mods, function(.x) { .x[[YAML_MOD_TYPE]] })
  uniq_model_types <- all_model_types %>% unlist() %>% unique()
  if (length(uniq_model_types) != 1) {
    stop(paste(
      glue("Passed vector `.mods` must contain all the same type of models, but found {length(uniq_model_types)} different classes of model:"),
      paste(uniq_model_types, collapse = ", ")
    ))
  }
  .model_type <- uniq_model_types

  # submit models
  class(.mods) <- paste0("bbi_", .model_type, "_models")
  submit_models(.mods,
                .bbi_args = .bbi_args,
                .mode = .mode,
                ...,
                .overwrite = .overwrite,
                .config_path = .config_path,
                .wait = .wait,
                .dry_run = .dry_run)
}

#' @export
submit_models.default <- function(.mods, ...) {
  stop("Unsupported model type: ", class(.mods))
}

#' @importFrom stringr str_detect
#' @importFrom tools file_path_sans_ext
#' @importFrom purrr map
#' @importFrom rlang %||%
#' @export
submit_models.bbi_nonmem_models <- function(.mods,
                                            .bbi_args = NULL,
                                            .mode = getOption("bbr.bbi_exe_mode"),
                                            ...,
                                            .overwrite = NULL,
                                            .config_path = NULL,
                                            .wait = TRUE,
                                            .dry_run = FALSE) {
  check_model_object_list(.mods, NM_MOD_CLASS)

  # check against YAML
  for (.mod in .mods) { check_yaml_in_sync(.mod) }

  # check for valid .mode arg
  check_mode_argument(.mode)

  # get unique sets of params
  if (!is.null(.overwrite)) {
    checkmate::assert_logical(.overwrite)
    .bbi_args[["overwrite"]] <- .overwrite
  }
  param_list <- build_bbi_param_list(.mods, .bbi_args)

  # a .run is a group of models that can be passed in a single bbi call
  cmd_args_list <- map(param_list, function(.run) {
    cmd_args <- c(
      "nonmem",
      "run",
      .mode,
      purrr::map_chr(.run[["models"]], get_model_path),
      .run[["bbi_args"]]
    )

    model_dir <- get_model_working_directory(.run[["models"]][[1L]])

    .path_exists <- file_exists(.config_path %||% file.path(model_dir, "bbi.yaml"))
    if(!.path_exists){
      stop(paste("No bbi configuration was found in the execution directory.",
                 "Please run `bbi_init()` with the appropriate directory to continue."))
    }

    if (!is.null(.config_path)) {
      cmd_args <- c(
        cmd_args,
        sprintf("--config=%s", normalizePath(.config_path))
      )
    }

    return(list(cmd_args = cmd_args, model_dir = model_dir))
  })

  message(glue("Submitting {length(.mods)} models with {length(cmd_args_list)} unique configurations."))

  if (.dry_run) {
    # construct fake res object
    return(map(
      cmd_args_list,
      function(.run) { bbi_dry_run(.run$cmd_args, .run$model_dir) }
    ))
  }

  # launch models
  res_list <- map(
    cmd_args_list,
    function(.run) { bbi_exec(.run$cmd_args, .wait = .wait, .dir = .run$model_dir, ...) },
    ...
  )

  return(res_list)
}

