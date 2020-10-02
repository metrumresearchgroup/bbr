
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
#' model YAML, or specified globally in `babylon.yaml`.
#' @param .mods The model objects to submit.
#' @inheritParams submit_model
#' @export
submit_models <- function(
  .mods,
  .bbi_args = NULL,
  .mode = c("sge", "local"),
  ...,
  .config_path = file.path(get_model_directory() %||% ".", "babylon.yaml"),
  .wait = TRUE,
  .dry_run=FALSE,
  .directory = NULL
) {
  UseMethod("submit_models")
}

#' @describeIn submit_models Takes a list of `bbi_nonmem_model` objects.
#' @importFrom purrr map map_lgl
#' @export
submit_models.list <- function(
  .mods,
  .bbi_args = NULL,
  .mode = c("sge", "local"),
  ...,
  .config_path = file.path(get_model_directory() %||% ".", "babylon.yaml"),
  .wait = TRUE,
  .dry_run=FALSE,
  .directory = NULL
) {

  if (!is.null(.directory)) {
    warning(paste(glue("Passed `.directory = {.directory}` to submit_models.list().") ,
                  "This argument is only valid when passing a path to `submit_models()`.",
                  "Directory will be extracted from each model object."))
  }

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
  if (.model_type == "nonmem") {
    res_list <- submit_nonmem_models(.mods,
                                     .bbi_args = .bbi_args,
                                     .mode = .mode,
                                     ...,
                                     .config_path = .config_path,
                                     .wait = .wait,
                                     .dry_run = .dry_run)
  } else if (.model_type == "stan") {
    stop(NO_STAN_ERR_MSG)
  } else {
    stop(glue("Passed `{.model_type}`. Valid options: `{paste(SUPPORTED_MOD_TYPES, collapse = ', ')}`"))
  }
  return(res_list)
}


#####################################
# Private implementation function(s)
#####################################

#' Submit multiple NONMEM models in batch via babylon
#'
#' Private implementation function called by `submit_models()` dispatches.
#' @param .mods A list of S3 objects of class `bbi_nonmem_model`
#' @inheritParams submit_models
#' @importFrom stringr str_detect
#' @importFrom tools file_path_sans_ext
#' @importFrom purrr map
#' @importFrom rlang is_bare_list
#' @return A list of S3 objects of class `babylon_process`
#' @keywords internal
submit_nonmem_models <- function(.mods,
                                 .bbi_args = NULL,
                                 .mode = c("sge", "local"),
                                 ...,
                                 .config_path = file.path(get_model_directory() %||% ".", "babylon.yaml"),
                                 .wait = TRUE,
                                 .dry_run = FALSE) {

  # check input list (this is a private method so if these fail there is a bug somewhere that calls this)
  if (!is_bare_list(.mods)) {
    stop(glue("USER SHOULDN'T SEE THIS ERROR: Can only pass a list of {NM_MOD_CLASS} objects to submit_nonmem_models. Passed object of class {paste(class(.mods), collapse = ', ')}"))
  }
  check_model_object_list(.mods, NM_MOD_CLASS)

  # check against YAML
  for (.mod in .mods) { check_yaml_in_sync(.mod) }

  # check for valid type arg
  .mode <- match.arg(.mode)

  # get unique sets of params
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

    # TODO: change this once we have the helper
    model_dir <- get_model_working_directory(.run[["models"]][[1L]])

    # check for babylon.yaml config
    .config_path <- find_config_file_path(.config_path, model_dir)

    if (.config_path != "babylon.yaml") {
      cmd_args <- c(cmd_args, sprintf("--config=%s", .config_path))
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

