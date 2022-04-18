#' Takes a model object and runs it with various threads values
#'
#' @param .mod bbi_model object to copy/test
#' @param threads Integer vector of threads values to test
#' @param .mode Passed through to bbr::submit_models(.mode)
#' @param .bbi_args a named list.
#'
#' @importFrom assertthat assert_that
#'
#' @return A list of the model objects for the submitted models.
#'
#' @export
test_threads <- function(.mod,
                         threads = c(2,4),
                         .mode = getOption("bbr.bbi_exe_mode"),
                         .bbi_args = list())
{

  assert_that(is.list(.bbi_args))

  .mods <- map(threads, ~ copy_model_from(.mod, paste0(get_model_id(.mod), "_", .x, "_threads")) %>%
                 add_bbi_args(.bbi_args = c(threads = .x,
                                            .bbi_args,
                                            parallel = TRUE,
                                            overwrite = TRUE)) %>%
                 add_tags(paste("test",.x,"threads")))

  mod_paths <- lapply(.mods, function(mod.x){mod.x$absolute_model_path}) %>% unlist()

  submit_models(.mods, .mode = .mode, .wait = FALSE)

  .mods
}

#' Check estimation time for models run with various threads values
#'
#' @param mods list of bbi model objects created by `test_threads()`
#'
#' @return A tibble with columns `threads` (number of threads) and `time`
#'   (elapsed estimation time in seconds for test models).
#'
#' @export
check_threads <- function(mods) {
  purrr::map_dfr(mods, ~ {
    s <- model_summary(.x)
    threads <- as.numeric(stringr::str_extract(s$absolute_model_path, "\\d+(?=_threads$)"))
    tibble::tibble(threads = threads, estimation_time = s$run_details$estimation_time)
  })
}

#' Remove model files created via [test_threads()]
#'
#' @param mods list of bbi model objects created by `test_threads()`
#'
#' @export
cleanup_mods <- function(mods){
  # Only remove mods with correct tag
  mod_paths <- lapply(mods, function(mod.x){mod.x$absolute_model_path})
  mod_threads <- lapply(mods, function(mod.x){mod.x$bbi_args$threads})
  mod_tags <- lapply(mods, function(mod.x){mod.x$tags})

  mods_remove <- map2(mod_tags, mod_threads, function(tag.x, thread.y){
    grepl(paste("test", thread.y, "threads"), tag.x)
  }) %>% unlist()

  mod_paths <- mod_paths[mods_remove] %>% unlist()

  for (m in mod_paths) {
    if (fs::file_exists(yaml_ext(m))) fs::file_delete(yaml_ext(m))
    if (fs::file_exists(ctl_ext(m))) fs::file_delete(ctl_ext(m))
    if (fs::dir_exists(m)) fs::dir_delete(m)
  }
  message("Removed models with the following tags:\n", paste("-",unlist(mod_tags), collapse = "\n"))
}



