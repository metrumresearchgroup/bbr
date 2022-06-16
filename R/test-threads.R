#' Takes a model object and runs it with various threads values
#'
#' @param .mod bbi_model object to copy/test.
#' @param .threads Integer vector of threads values to test.
#' @param .bbi_args a named list.
#' @param .max_eval Max number of iterations for NONMEM to run.
#'         Will update `MAXITER` or `NITER` (whichever is specified) in generated models.
#'         The best number for this is model-dependent. Typically, something between 10 and
#'         100 is good, depending on how long each iteration takes. You want something that
#'         will run for 3-5 minutes total.
#' @param ... args passed through to submit_models()
#'
#' @importFrom checkmate assert_list
#' @importFrom rlang is_empty
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
#' mods <- test_threads(.mod = mod, .threads = c(2, 4), .max_eval = 10,
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
#' mods <- test_threads(.mod = mod, .threads = c(2, 4), .max_eval = 10,
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
  .max_eval = 10,
  ...
) {

  check_model_object(.mod)
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

  # Modify MAXEVAL or NITER
  search_str <- "MAXEVAL|NITER"
  map(.mods, function(.mod){
    mod_path <- get_model_path(.mod)
    mod_lines <- mod_path %>% readLines() %>% suppressSpecificWarning("incomplete final line found")
    str_line_loc <- which(grepl(search_str, mod_lines))

    if(is_empty(str_line_loc)){
      delete_models(.mods = .mods, .force = TRUE) %>% suppressMessages()
      stop("Neither MAXEVAL or NITER were found in the ctl file. Please ensure one is provided.")
    }

    str_values <- str_split(mod_lines[str_line_loc], " ")[[1]]
    str_loc <- grepl(search_str, str_values)

    if(length(str_loc[str_loc]) > 1){
      delete_models(.mods = .mods, .force = TRUE) %>% suppressMessages()
      stop("Both MAXEVAL and NITER were found in the ctl file. Please ensure only one is provided.")
    }

    str_update <- paste0(gsub('[[:digit:]]+', '', str_values[str_loc]), .max_eval)
    str_line_update <- paste(paste(str_values[!str_loc], collapse = " "), str_update, sep = " ")
    mod_lines[str_line_loc] <- str_line_update
    writeLines(mod_lines, mod_path)
  })

  submit_models(.mods, .wait = FALSE, ...)

  .mods
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
#' `.return_times` can be any subset of `c("estimation_time", "covariance_time", "cpu_time")`.
#' Users can also specify `all`, which is the shorthand method for selecting all 3 of those columns.
#'
#' @examples
#' \dontrun{
#' mods <- test_threads(mod, .threads = c(2, 4))
#'
#' If models have not finished:
#' check_run_times(mods, .wait = TRUE, .time_limit = 300)
#' check_run_times(mods, .wait = TRUE, .return_times = c("estimation_time", "covariance_time"))
#'
#' If models have already finished:
#' check_run_times(mods, .wait = FALSE)
#' check_run_times(mods, .wait = FALSE, .return_times = "All")
#' }
#' @return A tibble with columns `threads` (number of threads) and `time`
#'   (elapsed estimation time in seconds for test models).
#'
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

  if("all" %in% .return_times){
    .return_times <- c("estimation_time", "covariance_time", "cpu_time")
  }else{
    assert_true(all(.return_times %in% c("estimation_time", "covariance_time", "cpu_time")))
  }

  if (inherits(.mods, "bbi_model")) {
    .mods <- list(.mods)
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
        estimation_time = .x$run_details$estimation_time,
        covariance_time = .x$run_details$covariance_time,
        cpu_time = .x$run_details$cpu_time) %>%
        select(run, threads, all_of(.return_times))
      return(data)
    }, error = function(cond){
      data <- data.frame(matrix(ncol = length(.return_times) + 2, nrow = 1)) %>% as_tibble
      colnames(data) <- c('run', 'threads', .return_times)
      data$run <- model_run
      message("Could not access data for ", model_run)
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
  check_model_object_list(.mods, .mod_types = c(NM_MOD_CLASS, NM_SUM_CLASS))
  assert_logical(.force, len = 1)

  mod_info <- map_dfr(.mods, function(mod.x){
    mod_tags <- mod.x$tags
    mod_tags <- paste(mod_tags, collapse = ", ")
    mod_tags <- ifelse(mod_tags == "", "NA", mod_tags)
    tibble::tibble(
      mod_paths = mod.x$absolute_model_path,
      mod_thread = mod.x$bbi_args$threads,
      mod_tags = mod_tags
    )
  })

  tag_groups <- if(is.null(.tags)){
    crossing(mod_tags = mod_info$mod_tags, .tags = "NA", found = TRUE) %>% left_join(mod_info, by = "mod_tags")
  }else{
    tag_levels <- unique(.tags) # message in order of tags
    tag_groups <- crossing(mod_tags = mod_info$mod_tags, .tags) %>% left_join(mod_info, by = "mod_tags") %>%
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
    stop("None of specified tags were found")
  }

  mods_removed <- unique(tag_groups$mod_tags)

  if (!isTRUE(.force)) {
    msg_prompt <- paste0(
      paste("Are you sure you want to remove", length(mod_paths), "models with the following tags?: "),
      paste0("`",mods_removed,"`", collapse = ", ")
    )
    delete_prompt <- askYesNo(msg_prompt)
    if (!isTRUE(delete_prompt)) return(invisible(NULL))
  }

  msg_remove <- paste0(
    paste("Removed", length(mod_paths), "models with the following tags:\n"),
    paste("-",mods_removed, collapse = "\n")
  )

  for (m in mod_paths) {
    if (fs::file_exists(yaml_ext(m))) fs::file_delete(yaml_ext(m))
    if (fs::file_exists(ctl_ext(m))) fs::file_delete(ctl_ext(m))
    if (fs::dir_exists(m)) fs::dir_delete(m)
  }
  message(msg_remove)

}


