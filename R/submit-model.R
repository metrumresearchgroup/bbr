###############################################
# S3 dispatches for submitting a single model
###############################################

#' S3 generic method for submit_model
#' @param .mod generic model to submit
#' @param .bbi_args A named list specifying arguments to pass to babylon formatted like `list("nm_version" = "nm74gf_nmfe", "json" = T, "threads" = 4)`. Run `print_nonmem_args()` to see valid arguments.
#' @param .mode Either "local" for local execution or "sge" to submit model(s) to the grid
#' @param ... args passed through to `bbi_exec()`
#' @param .config_path Optionally specify a path to a babylon.yml config. If not specified, the config in the model directory will be used by default. Path MUST be either an absolute path or relative to the model directory.
#' @param .wait Boolean for whether to wait for the bbi process to return before this function call returns.
#' @param .dry_run Returns an object detailing the command that would be run, insted of running it. This is primarily for testing but also a debugging tool.
#' @param .directory Model directory which `.mod` path is relative to. Defaults to `options('rbabylon.model_directory')`, which can be set globally with `set_model_directory()`. Only used when passing a path for `.mod` instead of a `bbi_{.model_type}_model` object.
#' @export
#' @rdname submit_model
submit_model <- function(
  .mod,
  .bbi_args = NULL,
  .mode = c("sge", "local"),
  ...,
  .config_path = file.path(get_model_directory() %||% ".", "babylon.yaml"),
  .wait = TRUE,
  .dry_run=FALSE,
  .directory = NULL
) {
  UseMethod("submit_model")
}

#' S3 dispatch for submit_model from bbi_nonmem_model
#' @param .mod S3 object of class `bbi_nonmem_model` to submit
#' @param .directory This argument is only used when passing a path to `submit_model()`. When `bbi_nonmem_model` object is passed, `.directory` is inferred from the model object.
#' @export
#' @rdname submit_model
submit_model.bbi_nonmem_model <- function(
  .mod,
  .bbi_args = NULL,
  .mode = c("sge", "local"),
  ...,
  .config_path = file.path(get_model_directory() %||% ".", "babylon.yaml"),
  .wait = TRUE,
  .dry_run=FALSE,
  .directory = NULL
) {

  if (!is.null(.directory)) {
    warning(paste(glue("Passed `.directory = {.directory}` to submit_model.bbi_nonmem_model().") ,
                  "This argument is only valid when passing a path to `submit_model()`.",
                  "`bbi_nonmem_model` object was passed, so `.directory` inferred from `.mod${WORKING_DIR}`"))
  }

  res <- submit_nonmem_model(.mod,
                             .bbi_args = .bbi_args,
                             .mode = .mode,
                             ...,
                             .config_path = .config_path,
                             .wait = .wait,
                             .dry_run = .dry_run)
  return(res)
}

#' S3 dispatch for submit_model from character scaler.
#' Should be path to yaml (with or without .yaml extension), or a valid model file (control stream, etc.).
#' @param .mod Path to YAML or model file
#' @param .directory Model directory which `.mod` path is relative to. Defaults to `options('rbabylon.model_directory')`, which can be set globally with `set_model_directory()`.
#' @importFrom fs file_exists
#' @export
#' @rdname submit_model
submit_model.character <- function(
  .mod,
  .bbi_args = NULL,
  .mode = c("sge", "local"),
  ...,
  .config_path = file.path(get_model_directory() %||% ".", "babylon.yaml"),
  .wait = TRUE,
  .dry_run=FALSE,
  .directory = get_model_directory()
) {

  # check for .directory and combine with .mod
  .mod <- combine_directory_path(.directory, .mod)

  if (!(is_valid_yaml_extension(.mod) || is_valid_nonmem_extension(.mod) || tools::file_ext(.mod) == "")) {
    stop(glue("Unsupported file type passed to submit_models(): `{.mod}`. Valid options are `.yaml`, `.yml`, `.mod`, and `.ctl` (or no extension, which will infer .yaml or .yml)"))
  }

  # attempt to read model from path
  .mod <- read_model(.mod)

  # submit model
  .model_type <- .mod[[YAML_MOD_TYPE]]
  if (.model_type == "nonmem") {
    res <- submit_nonmem_model(.mod,
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
  return(res)
}


#' S3 dispatch for submit_model from numeric input
#' This will only work if you are calling from the same directory as the models, or if you have set the model directory with `set_model_directory()`
#' @param .mod Integer that corresponds to the name of a YAML and model file
#' @param .directory Model directory containing the files referenced by `.mod`. Defaults to `options('rbabylon.model_directory')`, which can be set globally with `set_model_directory()`.
#' @export
#' @rdname submit_model
submit_model.numeric <- function(
  .mod,
  .bbi_args = NULL,
  .mode = c("sge", "local"),
  ...,
  .config_path = file.path(get_model_directory() %||% ".", "babylon.yaml"),
  .wait = TRUE,
  .dry_run=FALSE,
  .directory = get_model_directory()
) {
  # convert to character
  .mod <- as.character(.mod)

  # call character dispatch
  res <- submit_model(
    .mod = .mod,
    .bbi_args = .bbi_args,
    .mode = .mode,
    ...,
    .config_path = .config_path,
    .wait = .wait,
    .dry_run = .dry_run,
    .directory = .directory
  )

  return(res)
}


#' Submit a NONMEM model via babylon
#' @param .mod An S3 object of class `bbi_nonmem_model`, for example from `new_model()`, `read_model()` or `copy_model_from()`
#' @importFrom stringr str_detect
#' @importFrom tools file_path_sans_ext
#' @return An S3 object of class `babylon_process`
#' @rdname submit_model
submit_nonmem_model <- function(.mod,
                                .bbi_args = NULL,
                                .mode = c("sge", "local"),
                                ...,
                                .config_path = file.path(get_model_directory() %||% ".", "babylon.yaml"),
                                .wait = TRUE,
                                .dry_run=FALSE) {

  # check against YAML
  check_yaml_in_sync(.mod)

  # check for valid type arg
  .mode <- match.arg(.mode)

  # build command line args
  .bbi_args <- parse_args_list(.bbi_args, .mod[[YAML_BBI_ARGS]])
  args_vec <- check_nonmem_args(.bbi_args)
  cmd_args <- c("nonmem", "run", .mode, .mod[[YAML_MOD_PATH]], args_vec)

  # define working directory
  model_dir <- .mod[[WORKING_DIR]]

  # check for babylon.yaml config
  .config_path <- find_config_file_path(.config_path, model_dir)

  if (.config_path != "babylon.yaml") {
    cmd_args <- c(cmd_args, sprintf("--config=%s", .config_path))
  }

  if (.dry_run) {
    # construct fake res object
    return(bbi_dry_run(cmd_args, model_dir))
  }

  # launch model
  res <- bbi_exec(cmd_args, .wait = .wait, .dir = model_dir, ...)

  return(res)
}


#########################################################
# S3 dispatches for submitting multiple models in batch
#########################################################

#' S3 generic method for submit_models
#' @param .mods generic list or vector of models to submit
#' @param .bbi_args A named list specifying arguments to pass to babylon formatted like `list("nm_version" = "nm74gf_nmfe", "json" = T, "threads" = 4)`. Run `print_nonmem_args()` to see valid arguments.
#' @param .mode Either "local" for local execution or "sge" to submit model(s) to the grid
#' @param ... args passed through to `bbi_exec()`
#' @param .config_path Optionally specify a path to a babylon.yml config. If not specified, the config in the model directory will be used by default. Path MUST be either an absolute path or relative to the model directory.
#' @param .wait Boolean for whether to wait for the bbi process to return before this function call returns.
#' @param .dry_run Returns an object detailing the command that would be run, insted of running it. This is primarily for testing but also a debugging tool.
#' @param .directory Model directory which `.mod` path is relative to. Defaults to `options('rbabylon.model_directory')`, which can be set globally with `set_model_directory()`. Only used when passing a path for `.mod` instead of a `bbi_{.model_type}_model` object.
#' @export
#' @rdname submit_models
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

#' S3 dispatch for submit_model from bbi_nonmem_model
#' @param .mods a list of S3 object of class `bbi_nonmem_model` to submit
#' @param .directory This argument is only used when passing a path to `submit_model()`. When `bbi_nonmem_model` object is passed, `.directory` is inferred from the model object.
#' @importFrom purrr map map_lgl
#' @export
#' @rdname submit_models
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
    warning(paste(glue("Passed `.directory = {.directory}` to submit_models.bbi_nonmem_model().") ,
                  "This argument is only valid when passing a path to `submit_models()`.",
                  "`bbi_nonmem_model` objects were passed, so `.directory` inferred from `.mod${WORKING_DIR}`"))
  }

  # check that each element is a model object
  all_models_bool <- map_lgl(.mods, function(.x) { inherits(.x, VALID_MOD_CLASSES) })
  if (isFALSE(all(all_models_bool))) {
    losers <- which(!all_models_bool)
    stop(paste(
      glue("Passed list must contain only model objects, but found {length(losers)} invalid objects at indices:"),
      paste(losers, collapse = ", ")
    ))
  }

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


#' S3 dispatch for submit_models from character vector
#'   Should be a vector of paths to yaml (with or without .yaml extension), or a valid model file (control stream, etc.).
#' @param .mods Character vector of paths to YAML or model file
#' @param .directory Model directory which `.mods` paths are relative to. Defaults to `options('rbabylon.model_directory')`, which can be set globally with `set_model_directory()`.
#' @importFrom fs file_exists
#' @importFrom purrr map map_chr
#' @export
#' @rdname submit_models
submit_models.character <- function(
  .mods,
  .bbi_args = NULL,
  .mode = c("sge", "local"),
  ...,
  .config_path = file.path(get_model_directory() %||% ".", "babylon.yaml"),
  .wait = TRUE,
  .dry_run=FALSE,
  .directory = get_model_directory()
) {

  # check for .directory and combine with .mods
  .mods <- map_chr(.mods, function(.mod) { combine_directory_path(.directory, .mod) })

  # attempt to load model objects
  .mods <- map(.mods, function(.mod) {
    if (!(is_valid_yaml_extension(.mod) || is_valid_nonmem_extension(.mod) || tools::file_ext(.mod) == "")) {
      stop(glue("Unsupported file type passed to submit_models(): `{.mod}`. Valid options are `.yaml`, `.yml`, `.mod`, and `.ctl` (or no extension, which will infer .yaml or .yml)"))
    }

    # load model from path
    read_model(.mod)
  })

  # pass to submit_models.list
  res_list <- submit_models(.mods,
                            .bbi_args = .bbi_args,
                            .mode = .mode,
                            ...,
                            .config_path = .config_path,
                            .wait = .wait,
                            .dry_run = .dry_run)
  return(res_list)
}


#' S3 dispatch for submit_models from numeric input
#' This will only work if you are calling from the same directory as the models, or if you have set the model directory with `set_model_directory()`
#' @param .mods Integer vector that corresponds to the names of YAML and model files
#' @param .directory Model directory containing the files referenced by `.mods`. Defaults to `options('rbabylon.model_directory')`, which can be set globally with `set_model_directory()`.
#' @export
#' @rdname submit_models
submit_models.numeric <- function(
  .mods,
  .bbi_args = NULL,
  .mode = c("sge", "local"),
  ...,
  .config_path = file.path(get_model_directory() %||% ".", "babylon.yaml"),
  .wait = TRUE,
  .dry_run=FALSE,
  .directory = get_model_directory()
) {
  # convert to character
  .mods <- as.character(.mods)

  # call character dispatch
  res_list <- submit_models(
    .mods = .mods,
    .bbi_args = .bbi_args,
    .mode = .mode,
    ...,
    .config_path = .config_path,
    .wait = .wait,
    .dry_run = .dry_run,
    .directory = .directory
  )

  return(res_list)
}


#' Submits multiple NONMEM models in batch via babylon
#' @param .mods A list of S3 objects of class `bbi_nonmem_model`
#' @importFrom stringr str_detect
#' @importFrom tools file_path_sans_ext
#' @importFrom purrr map
#' @importFrom rlang is_bare_list
#' @return A list of S3 objects of class `babylon_process`
#' @rdname submit_models
submit_nonmem_models <- function(.mods,
                                .bbi_args = NULL,
                                .mode = c("sge", "local"),
                                ...,
                                .config_path = file.path(get_model_directory() %||% ".", "babylon.yaml"),
                                .wait = TRUE,
                                .dry_run=FALSE) {

  # check input list (this is a private method so if these fail there is a bug somewhere that calls this)
  if (!is_bare_list(.mods)) {
    stop(glue("USER SHOULDN'T SEE THIS ERROR: Can only pass a list of bbi_nonmem_model objects to submit_nonmem_models. Passed object of class {paste(class(.mods), collapse = ', ')}"))
  }

  all_models_bool <- map_lgl(.mods, function(.x) { inherits(.x, "bbi_nonmem_model") })
  if (isFALSE(all(all_models_bool))) {
    losers <- which(!all_models_bool)
    stop(paste(
      glue("USER SHOULDN'T SEE THIS ERROR: Passed list must contain only bbi_nonmem_model objects, but found {length(losers)} invalid objects at indices:"),
      paste(losers, collapse = ", ")
    ))
  }

  # check against YAML
  for (.mod in .mods) { check_yaml_in_sync(.mod) }

  # check for valid type arg
  .mode <- match.arg(.mode)

  # get unique sets of params
  param_list <- build_bbi_param_list(.mods, .bbi_args)

  # build command line args
  cmd_args_list <- map(param_list, function(.run) {
    cmd_args <- c("nonmem", "run", .mode, .run[[YAML_MOD_PATH]], .run[[YAML_BBI_ARGS]])

    # define working directory
    model_dir <- .run[[WORKING_DIR]]

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



