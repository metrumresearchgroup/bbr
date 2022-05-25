context("test_threads(.dry_run=T)")

###################################
# testing single model submission
###################################


# define constants
MODEL_DIR_BBI <- file.path(dirname(ABS_MODEL_DIR), "test-workflow-bbi-models")

# cleanup function
cleanup_bbi <- function(.recreate_dir = FALSE) {
  if (fs::dir_exists(MODEL_DIR_BBI)) fs::dir_delete(MODEL_DIR_BBI)
  if (isTRUE(.recreate_dir)) fs::dir_create(MODEL_DIR_BBI)
}
cleanup_bbi(.recreate_dir = TRUE)

withr::with_options(list(bbr.bbi_exe_path = read_bbi_path()), {

  # cleanup when done
  on.exit({
    Sys.sleep(3) # wait for some NONMEM mess to delete itself
    cleanup_bbi()
  })

  if (fs::file_exists(file.path(MODEL_DIR_BBI, "bbi.yaml"))) fs::file_delete(file.path(MODEL_DIR_BBI, "bbi.yaml"))

  # create fake bbi.yaml
  readr::write_file("created_by: test-test-threads", file.path(MODEL_DIR_BBI, "bbi.yaml"))

  # copy model file into new model dir
  fs::file_copy(CTL_TEST_FILE, MODEL_DIR_BBI)

  mod1 <- new_model(
    file.path(MODEL_DIR_BBI, "1"),
    .description = "original test-test-threads model",
    .tags = ORIG_TAGS,
    .bbi_args = list(overwrite = TRUE, threads = 2)
  )

  mods <- test_threads(mod1, .threads = c(2, 4), .max_eval = 100, .mode = "local", .dry_run = TRUE)


  test_that("test_threads(.dry_run=T) creates copy models [BBR-TSTT-001]", {
    mod_ctls <- lapply(mods, function(mod.x){get_model_path(mod.x)}) %>% unlist()
    mod_yamls <- lapply(mods, function(mod.x){mod.x$absolute_model_path}) %>% yaml_ext()

    expect_true(all(fs::file_exists(mod_ctls)))
    expect_true(all(fs::file_exists(mod_yamls)))
  })


  test_that("test_threads(.dry_run=T) correctly changes maxeval/niter [BBR-TSTT-002]", {
    mod_lines <- lapply(mods, function(mod.x){get_model_path(mod.x) %>% readLines()})

    search_str <- "MAXEVAL|NITER"
    max_evals <- map(mods, function(.mod){
      mod_path <- get_model_path(.mod)
      mod_lines <- mod_path %>% readLines()
      str_line_loc <- which(grepl(search_str, mod_lines))
      str_values <- str_split(mod_lines[str_line_loc], " ")[[1]]
      str_loc <- grepl(search_str, str_values)
      as.numeric(gsub('MAXEVAL=', '', str_values[str_loc]))
    }) %>% unlist()

    expect_true(all(max_evals == 100))
  })


  test_that("test_threads(.dry_run=T) threads are set correctly [BBR-TSTT-003]", {
    mod_threads <- lapply(mods, function(mod.x){mod.x$bbi_args$threads}) %>% unlist()
    expect_equal(mod_threads[1], 2)
    expect_equal(mod_threads[2], 4)
  })


  mod_none <- copy_model_from(
    read_model(file.path(MODEL_DIR_BBI, "1")),
    "none"
  ) %>% add_tags("no maxeval")

  mod_both <- copy_model_from(
    read_model(file.path(MODEL_DIR_BBI, "1")),
    "both"
  ) %>% add_tags("maxeval and niter")

  mods_fake <- list(mod_none, mod_both)


  test_that("test_threads(.dry_run=T) correctly errors out if no maxeval, or both maxeval and niter are provided [BBR-TSTT-004]", {

    search_str <- "MAXEVAL|NITER"
    str_replacements <- c("", "MAXEVAL=10 NITER=10")

    map2(mods_fake, str_replacements, function(.mod, str_replace){
      mod_path <- get_model_path(.mod)
      mod_lines <- mod_path %>% readLines()
      str_line_loc <- which(grepl(search_str, mod_lines))
      str_values <- str_split(mod_lines[str_line_loc], " ")[[1]]
      str_loc <- grepl(search_str, str_values)
      str_line_update <- paste(paste(str_values[!str_loc], collapse = " "), str_replace, sep = " ")
      mod_lines[str_line_loc] <- str_line_update
      writeLines(mod_lines, mod_path)
    })

    expect_error(
      test_threads(mod_none, .threads = c(2, 4), .max_eval = 100, .mode = "local", .dry_run = TRUE),
      "Neither MAXEVAL or NITER were found in the ctl file. Please ensure one is provided")
    expect_error(
      test_threads(mod_both, .threads = c(2, 4), .max_eval = 100, .mode = "local", .dry_run = TRUE),
      "Both MAXEVAL and NITER were found in the ctl file. Please ensure only one is provided")
  })


  test_that("check_run_times() returns NA for dry runs [BBR-TSTT-005]", {
    expect_message(
      check_run_times(mod1, .wait = F),
      "Could not access data for 1"
    )
    run_times <- check_run_times(mod1, .wait = F) %>% suppressMessages()
    expect_equal(run_times$run, "1")
    expect_true(is.na(run_times$threads))
    expect_true(is.na(run_times$estimation_time))
  })

  test_that("delete_models() works for models created by test_threads by default [BBR-CLM-001]", {

    mod_ctls <- lapply(mods, function(mod.x){get_model_path(mod.x)}) %>% unlist()
    mod_yamls <- lapply(mods, function(mod.x){mod.x$absolute_model_path}) %>% yaml_ext()

    msg_remove <- paste0(
      paste("Removed", length(mods), "models with the following tags:\n"),
      paste("-","test threads", collapse = "\n")
    )
    expect_message(
      delete_models(mods, .force = T),
      msg_remove
    )
    expect_false(any(fs::file_exists(mod_ctls)))
    expect_false(any(fs::file_exists(mod_yamls)))
  })

  test_that("delete_models() with .tags [BBR-CLM-002]", {

    fake_mod_tags <- lapply(mods_fake, function(mod.x){mod.x$tags}) %>% unlist()
    msg_remove <- paste0(
      paste("Removed", length(mods_fake), "models with the following tags:\n"),
      paste("-",fake_mod_tags, collapse = "\n")
    )

    mod_ctls <- lapply(mods_fake, function(mod.x){get_model_path(mod.x)}) %>% unlist()
    mod_yamls <- lapply(mods_fake, function(mod.x){mod.x$absolute_model_path}) %>% yaml_ext()

    expect_message(
      delete_models(mods_fake, .tags = fake_mod_tags, .force = T),
      msg_remove
    )

    expect_false(any(fs::file_exists(mod_ctls)))
    expect_false(any(fs::file_exists(mod_yamls)))
  })



  test_that("delete_models() with models with multiple tags [BBR-CLM-003]", {
    mod_two_tags <- copy_model_from(read_model(file.path(MODEL_DIR_BBI, "1")), "two_tags") %>%
      add_tags("some tag") %>% add_tags("another tag")
    mod_one_tag <- copy_model_from(read_model(file.path(MODEL_DIR_BBI, "1")), "one_tag") %>%
      add_tags("a tag")
    mods <- list(mod_one_tag, mod_two_tags)

    mod_tags <- lapply(mods, function(mod.x){mod.x$tags})
    mod_tags[[2]] <- paste(mod_tags[[2]], collapse = ", ")
    mod_tags <- unlist(mod_tags)
    msg_remove <- paste0(
      paste("Removed", length(mods), "models with the following tags:\n"),
      paste("-",mod_tags, collapse = "\n")
    )

    mod_ctls <- lapply(mods, function(mod.x){get_model_path(mod.x)}) %>% unlist()
    mod_yamls <- lapply(mods, function(mod.x){mod.x$absolute_model_path}) %>% yaml_ext()

    expect_message(
      delete_models(mods, .tags = mod_tags, .force = T),
      msg_remove
    )

    expect_false(any(fs::file_exists(mod_ctls)))
    expect_false(any(fs::file_exists(mod_yamls)))
  })

  test_that("delete_models() with .tags=NULL [BBR-CLM-004]", {
    mods_threads <- test_threads(mod1, .threads = c(2, 4), .max_eval = 100, .mode = "local", .dry_run = TRUE)
    mod_new <- copy_model_from(read_model(file.path(MODEL_DIR_BBI, "1")), "one_tag") %>%
      add_tags("some tag")
    mod_no_tag <- copy_model_from(read_model(file.path(MODEL_DIR_BBI, "1")), "no_tag")

    mods <- c(mods_threads, list(mod_new), list(mod_no_tag))


    # When using NULL, models are not necessarily deleted in order
    # Only test that correct number of models are deleted
    msg_remove <- paste0(
      paste("Removed", length(mods), "models with the following tags:"))

    mod_ctls <- lapply(mods, function(mod.x){get_model_path(mod.x)}) %>% unlist()
    mod_yamls <- lapply(mods, function(mod.x){mod.x$absolute_model_path}) %>% yaml_ext()

    expect_message(
      delete_models(mods, .tags = NULL, .force = T),
      msg_remove
    )

    expect_false(any(fs::file_exists(mod_ctls)))
    expect_false(any(fs::file_exists(mod_yamls)))
  })

})


