#' Takes a model object and runs it with various threads values
#'
#' @param .mod bbi_model object to copy/test.
#' @param .threads Integer vector of threads values to test.
#' @param .mode Passed through to bbr::submit_models(.mode).
#' @param .bbi_args a named list.
#' @param .max_eval Max number of iterations for NONMEM to run.
#'         Will update `MAXITER` or `NITER` (whichever is specified) in generated models.
#' @param ... args passed through to submit_models()
#'
#' @importFrom checkmate assert_list
#' @importFrom rlang is_empty
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
#' cleanup_mods(.mods = mods)
#'
#'
#' # Dry Run:
#' mods <- test_threads(.mod = mod, .threads = c(2, 4), .max_eval = 10,
#'                      .mode = "local", .dry_run = TRUE)
#'
#' cleanup_mods(.mods = mods)
#' }
#'
#' @return A list of the model objects for the submitted models.
#'
#' @export
test_threads <- function(
  .mod,
  .threads = c(2,4),
  .mode = getOption("bbr.bbi_exe_mode"),
  .bbi_args = list(),
  .max_eval = 10,
  ...
) {

  check_model_object(.mod)
  assert_list(.bbi_args)

  # Duplicate model for each thread scenario
  .mods <- map(.threads, ~ copy_model_from(.mod, paste0(get_model_id(.mod), "_", .x, "_threads")) %>%
                 add_bbi_args(.bbi_args = c(threads = .x,
                                            .bbi_args,
                                            parallel = TRUE,
                                            overwrite = TRUE)) %>%
                 add_tags(paste("test threads")))

  # Modify MAXEVAL or NITER
  search_str <- "MAXEVAL|NITER"
  map(.mods, function(.mod){
    mod_path <- get_model_path(.mod)
    mod_lines <- mod_path %>% readLines()
    str_line_loc <- which(grepl(search_str, mod_lines))

    if(is_empty(str_line_loc)){
      cleanup_mods(.mods = .mods) %>% suppressMessages()
      stop("Neither MAXEVAL or NITER were found in the ctl file. Please ensure one is provided.")
    }

    str_values <- str_split(mod_lines[str_line_loc], " ")[[1]]
    str_loc <- grepl(search_str, str_values)

    if(length(str_loc[str_loc]) > 1){
      cleanup_mods(.mods = .mods) %>% suppressMessages()
      stop("Both MAXEVAL and NITER were found in the ctl file. Please ensure only one is provided.")
    }

    str_update <- paste0(gsub('[[:digit:]]+', '', str_values[str_loc]), .max_eval)
    str_line_update <- paste(paste(str_values[!str_loc], collapse = " "), str_update, sep = " ")
    mod_lines[str_line_loc] <- str_line_update
    writeLines(mod_lines, mod_path)
  })

  submit_models(.mods, .mode = .mode, .wait = FALSE, ... = ...)

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

  if(!inherits(.mods, "bbi_nonmem_model")){
    check_model_object_list(.mods)
  }else{
    check_model_object(.mods)
    .mods <- list(.mods)
  }


  tryCatch({
    if(.wait) wait_for_nonmem(.mods, ...)

    map_dfr(.mods, ~ {
      s <- model_summary(.x)
      threads <- as.numeric(.x$bbi_args$threads)
      tibble::tibble(
        model_run = basename(.x$absolute_model_path),
        threads = threads,
        estimation_time = s$run_details$estimation_time,
        covariance_time = s$run_details$covariance_time,
        cpu_time = s$run_details$cpu_time) %>%
        select(model_run, threads, all_of(.return_times))
    })
  }, error = function(cond){
    return(NA)
  }, warning = function(cond){
    message(cond)
    message("\nConsider setting/increasing the `time_limit` in `wait_for_nonmem()`. See ?check_run_times for details")
    return(NA)
  }
  )
}

#' Remove model files associated with the specified tags
#'
#' @param .mods a bbi model object or list of model objects.
#' @param .tags a character vector identifying the tags of the models you want to delete.
#'              If set to `NULL`, no filtering will be performed and all associated model files will be deleted.
#' @param .force logical (T/F). If `TRUE`, do not prompt the user if they want to delete the models.
#'
#' @importFrom checkmate check_character
#' @importFrom tidyr crossing
#'
#' @export
cleanup_mods <- function(.mods, .tags = "test threads", .force = FALSE){
  if(!is.null(.tags)) check_character(.tags)
  # Only remove mods with correct tag
  mod_paths <- lapply(.mods, function(mod.x){mod.x$absolute_model_path})
  mod_threads <- lapply(.mods, function(mod.x){mod.x$bbi_args$threads})
  mod_tags <- lapply(.mods, function(mod.x){mod.x$tags})

  tag_groups <- if(is.null(.tags)){
    tidyr::crossing(mod_tags = unlist(mod_tags), .tags = NA, mod_paths = unlist(mod_paths), found = TRUE)
  }else{
    tag_groups <- tidyr::crossing(mod_tags = unlist(mod_tags), .tags, mod_paths = unlist(mod_paths))
    found <- map2(tag_groups$mod_tags, tag_groups$.tags, function(tag.x, .tag){
      grepl(.tag, tag.x)
    }) %>% unlist()
    tag_groups$found <- found
    if(!all(tag_groups$found)){
      message("The following tags were not found:\n",
              paste("-",unique(unlist(tag_groups[found==FALSE,".tags"])),"\n"))
    }
    tag_groups[found,]
  }

  mod_paths <- tag_groups$mod_paths

  if(length(mod_paths)==0){
    stop("None of specified tags were found")
  }


  mods_removed <- tag_groups$mod_tags

  msg_remove <- paste0(
    paste("Removed", length(mods_removed), "models with the following tags:\n"),
    paste("-",unique(mods_removed), collapse = "\n")
  )

  if(.force){
    for (m in mod_paths) {
      if (fs::file_exists(yaml_ext(m))) fs::file_delete(yaml_ext(m))
      if (fs::file_exists(ctl_ext(m))) fs::file_delete(ctl_ext(m))
      if (fs::dir_exists(m)) fs::dir_delete(m)
    }
    message(msg_remove)
  }else{
    msg_prompt <- paste0(
      paste("Are you sure you want to remove", length(mods_removed), "models with the following tags?: "),
      paste0("`",unique(mods_removed),"`", collapse = ", ")
    )
    delete_prompt <- askYesNo(msg_prompt)
    if(delete_prompt){
      for (m in mod_paths) {
        if (fs::file_exists(yaml_ext(m))) fs::file_delete(yaml_ext(m))
        if (fs::file_exists(ctl_ext(m))) fs::file_delete(ctl_ext(m))
        if (fs::dir_exists(m)) fs::dir_delete(m)
      }
      message(msg_remove)
    }

  }
}



