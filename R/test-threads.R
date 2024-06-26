#' Takes a model object and runs it with various threads values
#'
#' @param .mod bbi_model object to copy/test.
#' @param .threads Integer vector of threads values to test.
#' @param .bbi_args a named list.
#' @param .cap_iterations Maximum number of iterations for NONMEM to run in test models.
#'         Will update `MAXEVAL`, `NITER`, and `NBURN`, whichever is relevant for each estimation method.
#'         The best number for this is model-dependent. Typically, something between 10 and
#'         100 is good, depending on how long each iteration takes. You want something that
#'         will run for 3-5 minutes total. You can set this argument to `NULL` to run with the
#'         same settings as the original model. Any option set to `0` will not be overwritten.
#' @param ... args passed through to submit_models()
#'
#' @importFrom checkmate assert_list
#' @importFrom stringr str_split
#'
#' @details
#' Unfortunately, there is no easy way to know how many threads are ideal for a given NONMEM model.
#' It is dependent on many factors, including model complexity and the structure of the input data.
#' Typically, the best way to find the ideal number of threads for a given model is to test it empirically.
#' This function is intended to help the user do this.
#'
#' The function will create copies of your model, cap the number of evaluation iterations, and then
#' run these copies with the specified number of threads. The [check_run_times()] helper can then
#' be used to easily see how long each test model ran for, giving you a good sense of what the ideal
#' number of threads might be.
#'
#' @examples
#' \dontrun{
#'
#' mod <- read_model(file.path(MODEL_DIR, 1))
#'
#'
#' mods <- test_threads(.mod = mod, .threads = c(2, 4), .cap_iterations = 10,
#'                      .mode = "local")
#'
#'
#' check_run_times(mods, .wait = TRUE, .time_limit = 100)
#' check_run_times(mods, .wait = FALSE, .return_times = "all")
#'
#' delete_models(.mods = mods)
#'
#'
#' # Dry Run:
#' mods <- test_threads(.mod = mod, .threads = c(2, 4), .cap_iterations = 10,
#'                      .mode = "local", .dry_run = TRUE)
#'
#' delete_models(.mods = mods)
#' }
#'
#' @return A list of the model objects for the submitted models.
#'
#' @export
test_threads <- function(
    .mod,
    .threads = c(2,4),
    .bbi_args = list(),
    .cap_iterations = 10,
    ...
) {

  check_model_object(.mod, NM_MOD_CLASS)
  assert_list(.bbi_args)

  # Duplicate model for each thread scenario
  .mods <- map(.threads, ~ copy_model_from(.mod,
                                           paste0(get_model_id(.mod), "_", .x, "_threads"),
                                           .overwrite = TRUE) %>%
                 add_bbi_args(.bbi_args = c(threads = .x,
                                            .bbi_args,
                                            parallel = TRUE,
                                            overwrite = TRUE)) %>%
                 add_tags(paste("test threads")))

  # Modify Estimation Options
  adjust_estimation_options(.mods, .cap_iterations)

  tryCatch({
    submit_models(.mods, .wait = FALSE, ...)
    return(.mods)
  },
  error = function(cond){
    delete_models(.mods = .mods, .force = TRUE) %>% suppressMessages()
    stop(cond)
  })

}

#' Check estimation time for models run with various threads values
#'
#' @param .mods a bbi model object or list of model objects. Generally created by `test_threads()`.
#' @param .return_times character vector denoting which times from `model_summary()` you want to return.
#'        See details for more information.
#' @param .wait logical. If TRUE, pass `.mods` to `wait_for_nonmem()` before returning results.
#' @param ... args passed through to `wait_for_nonmem()`.
#'
#' @details
#' `.return_times` can be any subset of `c("estimation_time", "covariance_time", "postprocess_time", "cpu_time")`.
#' Users can also specify `"all"`, which is the shorthand method for selecting all 4 of those columns.
#'
#' @examples
#' \dontrun{
#' mods <- test_threads(mod, .threads = c(2, 4))
#'
#' # If models have not finished:
#' check_run_times(mods, .wait = TRUE, .time_limit = 300)
#' check_run_times(mods, .wait = TRUE, .return_times = c("estimation_time", "covariance_time"))
#'
#' # If models have already finished:
#' check_run_times(mods, .wait = FALSE)
#' check_run_times(mods, .wait = FALSE, .return_times = "all")
#' }
#' @return A tibble with columns `threads` (number of threads) and `time`
#'   (elapsed estimation time in seconds for test models).
#'
#' @importFrom checkmate assert_true
#' @importFrom tidyselect all_of
#' @importFrom purrr map_dfr
#'
#' @export
check_run_times <- function(
    .mods,
    .return_times = "estimation_time",
    .wait = TRUE,
    ...
) {

  is_old_bbi <- FALSE
  if (!test_bbi_version(.min_version = "3.2.0")) {
    is_old_bbi <- TRUE
    warning("This function is only compatible with bbi 3.2.0 or later")
  }

  if("all" %in% .return_times){
    .return_times <- c("estimation_time", "covariance_time", "postprocess_time", "cpu_time")
    if(is_old_bbi){
      # This allows the function to work with lower versions of bbi (despite covariance_time being incorrect)
      .return_times <- c("estimation_time", "covariance_time", "cpu_time")
    }
  }else{
    assert_true(all(.return_times %in% c("estimation_time", "covariance_time", "postprocess_time", "cpu_time")))
  }

  if (inherits(.mods, "bbi_model")) {
    if(inherits(.mods, NMBOOT_MOD_CLASS)){
      .mods <- get_boot_models(.mods)
      # If NULL (cleaned up or not set up) exit early
      #  - `get_boot_models` will return a relevant message, so no need to add
      #    an additional one
      if(is.null(.mods)) return(NULL)
    }else{
      .mods <- list(.mods)
    }

    check_model_object_list(.mods, .mod_types = c(NM_MOD_CLASS, NM_SUM_CLASS))
  }else if(inherits(.mods, "bbi_summary_list")){
    check_model_object(.mods, .mod_types = c("bbi_summary_list"))
  }

  if(.wait){
    tryCatch({
      wait_for_nonmem(.mods, ...)
    },
    warning = function(cond){
      message(cond)
      message("\nConsider setting/increasing the `time_limit` in `wait_for_nonmem()`. See ?check_run_times for details")
      return(invisible(TRUE))
    })
  }

  map_dfr(.mods, ~ {
    tryCatch({
      # unpack bbi_summary_list element
      if (!is.null(.x$bbi_summary)) .x <- .x$bbi_summary

      # get model id and convert to summary object if necessary
      model_run <- get_model_id(.x)
      if (inherits(.x, NM_MOD_CLASS)) {
        .x <- model_summary(.x, .bbi_args = list(no_grd_file = TRUE, no_ext_file = TRUE, no_shk_file = TRUE))
      }

      # get number of threads from bbi_config.json
      config <- jsonlite::fromJSON(file.path(.x[[ABS_MOD_PATH]], "bbi_config.json"))
      threads <- ifelse(
        config$configuration$parallel,
        config$configuration$threads,
        1
      )

      data <- tibble::tibble(
        run = model_run,
        threads = threads,
        estimation_time = sum(.x$run_details$estimation_time),
        covariance_time = sum(.x$run_details$covariance_time),
        postprocess_time = .x$run_details$postprocess_time,
        cpu_time = .x$run_details$cpu_time) %>%
        select(run, threads, all_of(.return_times))
      return(data)
    }, error = function(cond){
      data <- data.frame(matrix(ncol = length(.return_times) + 2, nrow = 1)) %>% as_tibble
      colnames(data) <- c('run', 'threads', .return_times)
      data$run <- model_run
      message("Could not access data for model ", model_run)
      return(data)
    }, warning = function(cond){
      data <- data.frame(matrix(ncol = length(.return_times) + 2, nrow = 1)) %>% as_tibble
      colnames(data) <- c('run', 'threads', .return_times)
      data$run <- model_run
      message(cond)
      return(data)
    })
  })

}


#' Remove model files associated with the specified tags
#'
#' @param .mods a bbi model object or list of model objects.
#' @param .tags a character vector identifying the tags of the models you want to delete.
#'              If set to `NULL`, no filtering will be performed and all associated model files will be deleted.
#' @param .force logical (T/F). If `TRUE`, do not prompt the user if they want to delete the models.
#'
#' @importFrom checkmate check_character assert_logical
#' @importFrom tidyr crossing
#' @importFrom dplyr arrange
#' @importFrom utils askYesNo
#'
#' @export
delete_models <- function(.mods, .tags = "test threads", .force = FALSE){
  if(!is.null(.tags)) check_character(.tags)

  if (inherits(.mods, "bbi_model")) {
    .mods <- list(.mods)
  }
  check_model_object_list(
    .mods,
    .mod_types = c(NM_MOD_CLASS, NM_SUM_CLASS, NMBOOT_MOD_CLASS, NMSIM_MOD_CLASS)
  )
  assert_logical(.force, len = 1)

  mod_info <- map_dfr(.mods, function(mod.x){
    mod_tags <- mod.x$tags
    mod_tags <- paste(mod_tags, collapse = ", ")
    mod_tags <- ifelse(mod_tags == "", "NA", mod_tags)
    mod_spec <- if(inherits(mod.x, c(NMSIM_MOD_CLASS, NMBOOT_MOD_CLASS))){
      get_spec_path(mod.x, .check_exists = FALSE)
    }else{
      NA
    }
    tibble::tibble(
      mod_type = mod.x$model_type,
      mod_paths = mod.x$absolute_model_path,
      mod_tags = mod_tags,
      mod_spec = mod_spec
    )
  })

  tag_groups <- if(is.null(.tags)){
    crossing(mod_tags = mod_info$mod_tags, .tags = "NA", found = TRUE) %>%
      left_join_all(mod_info, by = "mod_tags")
  }else{
    tag_levels <- unique(.tags) # message in order of tags
    tag_groups <- crossing(mod_tags = mod_info$mod_tags, .tags) %>%
      left_join_all(mod_info, by = "mod_tags") %>%
      mutate(.tags = ordered(.tags, levels = tag_levels)) %>% arrange(.tags)
    found <- map2(tag_groups$mod_tags, tag_groups$.tags, function(tag.x, .tag){
      grepl(.tag, tag.x)
    }) %>% unlist()

    tag_groups$found <- found

    if(any(tag_groups$found==FALSE)){
      not_found <- c()
      for(tag.x in .tags){
        tag.found <- tag_groups$found[tag_groups$.tags==tag.x]
        if(all(tag.found==FALSE)){
          not_found <- c(not_found, tag.x)
        }
      }
      if(length(not_found) > 0){
        message("The following tags were not found:\n",
                paste("-",unique(not_found),"\n"))
      }
    }
    tag_groups[found,]
  }

  mod_paths <- unique(tag_groups$mod_paths)

  if(length(mod_paths)==0){
    err_msg <- "None of specified tags were found"
    if (.tags == "test threads") {
      err_msg <- paste0(
        err_msg,
        '. bbr::delete_models() defaults to deleting only models with the "test threads" tag.\n',
        "Pass delete_models(..., .tags = NULL) to delete passed models, regardless of tags."
      )
    }
    stop(err_msg)
  }

  mods_removed <- unique(tag_groups$mod_tags)

  if (!isTRUE(.force)) {
    msg_prompt <- paste0(
      paste("Are you sure you want to remove", length(mod_paths), "model(s) with the following tags?: "),
      paste0("`",mods_removed,"`", collapse = ", ")
    )
    delete_prompt <- askYesNo(msg_prompt)
    if (!isTRUE(delete_prompt)) return(invisible(NULL))
  }

  msg_remove <- if (is.null(.tags)) {
    paste("Removed", length(mod_paths), "model(s) (ignoring tags)")
  } else {
    paste0(
      paste("Removed", length(mod_paths), "model(s) with the following tags:\n"),
      paste("-", mods_removed, collapse = "\n")
    )
  }

  for (m in mod_paths) {
    if (fs::file_exists(yaml_ext(m))) fs::file_delete(yaml_ext(m))
    if (fs::file_exists(ctl_ext(m))) fs::file_delete(ctl_ext(m))
    if (fs::dir_exists(m)) fs::dir_delete(m)

    # Handling for simulations: delete specification file in parent directory
    m_spec <- tag_groups$mod_spec[tag_groups$mod_paths == m]
    if (!is.na(m_spec) && fs::file_exists(m_spec)) fs::file_delete(m_spec)
  }
  message(msg_remove)

}


#' Parse model files and overwrite estimation iterations
#'
#' @param .mods a list of model objects. Generally created by `test_threads()`.
#' @param .cap_iterations Max number of iterations for NONMEM to run.
#'
#' @importFrom readr parse_number
#' @importFrom checkmate assert_int
#' @importFrom stringr str_detect
#' @importFrom rlang is_empty
#'
#' @keywords internal
adjust_estimation_options <- function(.mods, .cap_iterations){

  if(!is.null(.cap_iterations)){
    assert_int(.cap_iterations, lower = 1)

    map(.mods, function(.mod){

      mod_path <- get_model_path(.mod)
      ctl <- nmrec::read_ctl(mod_path)

      # Identify EST Blocks
      ests <- nmrec::select_records(ctl, "est")

      if(rlang::is_empty(ests)){
        warning(glue("No Estimation line found in {basename(mod_path)}, so there are no options to cap"))
        return(NULL)
      }

      for(i in seq_along(ests)){
        est_block.i <- ests[[i]]
        est_block.i$parse()

        max_detect <- any(purrr::map_lgl(est_block.i$get_options(), ~ .x$name == "maxevals"))
        niter_detect <- any(purrr::map_lgl(est_block.i$get_options(), ~ .x$name == "niter"))
        nburn_detect <- any(purrr::map_lgl(est_block.i$get_options(), ~ .x$name == "nburn"))

        if(max_detect){
          nmrec::set_record_option(est_block.i, "maxevals", .cap_iterations)
        }else if(niter_detect){
          nmrec::set_record_option(est_block.i, "niter", .cap_iterations)
          if(nburn_detect && !check_record_val(est_block.i, "nburn", 0)) {
            # We don't want to overwrite NBURN if it was set to 0
            nmrec::set_record_option(est_block.i, "nburn", .cap_iterations)
          }else if(!nburn_detect){
            # NITER was specified, but NBURN wasn't (which means the default is being relied on). Force specification to cap.
            message(glue("Adding NBURN={.cap_iterations} declaration to {basename(mod_path)} to avoid using the default value"))
            nmrec::set_record_option(est_block.i, "NBURN", .cap_iterations)
          }
        }
      }
      nmrec::write_ctl(ctl, mod_path)
    })
  }
}


#' Check if a record option matches a specific value
#'
#' @param .record an `nmrec` record object
#' @param .name Name of option to select. Any valid spelling of the option name
#'   is allowed.
#' @param .value Value of option to check for. For value options, the specified value is anything
#' that can be formatted to a string (i.e. `10` in `NBURN=10`). For included flag options, the
#' default value is `TRUE` (`NOABORT`).
#'
#' @keywords internal
check_record_val <- function(.record, .name, .value){
  opt <- nmrec::get_record_option(.record, .name)
  if(!is.null(opt)){
    opt$value == .value
  }else{
    FALSE
  }
}
