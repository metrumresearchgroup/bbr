context("test_threads(.dry_run=T)")

###################################
# testing single model submission
###################################


# define constants
MODEL_DIR_BBI <- file.path(dirname(ABS_MODEL_DIR), "test-test-threads-models")
CTL_TEST_COMPLEX_FILE <- file.path(MODEL_DIR_X, "acop-fake-bayes.ctl")
CTL_TEST_COMPLEX_FILE2 <- file.path(MODEL_DIR_X, "example2_saemimp.ctl")
CTL_TEST_COMPLEX_FILE3 <- file.path(MODEL_DIR_X, "iovmm.mod")
CTL_TEST_COMPLEX_FILE4 <- file.path(MODEL_DIR_X, "acop-onlysim.ctl")

# cleanup function
cleanup_bbi <- function(.recreate_dir = FALSE) {
  if (fs::dir_exists(MODEL_DIR_BBI)) fs::dir_delete(MODEL_DIR_BBI)
  if (isTRUE(.recreate_dir)) fs::dir_create(MODEL_DIR_BBI)
}
cleanup_bbi(.recreate_dir = TRUE)


get_est_options <- function(search_str = "MAXEVAL|NITER|NBURN", mods){

  max_evals <- map(mods, function(.mod){
    mod_path <- get_model_path(.mod)
    mod_lines <- mod_path %>% readLines()
    str_line_loc <- which(grepl(search_str, mod_lines))
    str_values <- str_split(mod_lines[str_line_loc], " ")

    values <- map(1:length(str_values), ~ {
      est_loc <- grepl("METHOD", str_values[[.x]])
      est_method <- str_values[[.x]][est_loc]
      str_loc <- grepl(search_str, str_values[[.x]])
      eval_method <- gsub('[[:digit:]]+|=', '', str_values[[.x]][str_loc])
      value <- as.numeric(gsub(glue('{search_str}|='), '', str_values[[.x]][str_loc]))
      names(value) = paste0(est_method, ", ",eval_method)
      value
    }) %>% unlist()
  })
  max_evals
}

withr::with_options(list(bbr.bbi_exe_path = read_bbi_path()), {

  # cleanup when done
  on.exit({
    Sys.sleep(3) # wait for some NONMEM mess to delete itself
    cleanup_bbi()
  })

  if (fs::file_exists(file.path(MODEL_DIR_BBI, "bbi.yaml"))) fs::file_delete(file.path(MODEL_DIR_BBI, "bbi.yaml"))

  # create fake bbi.yaml
  readr::write_file("created_by: test-test-threads", file.path(MODEL_DIR_BBI, "bbi.yaml"))

  # copy model files into new model dir
  fs::file_copy(CTL_TEST_FILE, MODEL_DIR_BBI)
  fs::file_copy(CTL_TEST_COMPLEX_FILE, MODEL_DIR_BBI)
  fs::file_copy(CTL_TEST_COMPLEX_FILE2, MODEL_DIR_BBI)
  fs::file_copy(CTL_TEST_COMPLEX_FILE3, MODEL_DIR_BBI)
  fs::file_copy(CTL_TEST_COMPLEX_FILE4, MODEL_DIR_BBI)


  mod1 <- new_model(
    file.path(MODEL_DIR_BBI, "1"),
    .description = "original test-test-threads model",
    .tags = ORIG_TAGS,
    .bbi_args = list(overwrite = TRUE, threads = 2)
  )

  mod_complex <- new_model(
    file.path(MODEL_DIR_BBI, "acop-fake-bayes"),
    .description = "complex test-test-threads model",
    .tags = ORIG_TAGS,
    .bbi_args = list(overwrite = TRUE, threads = 2)
  )

  mod_complex2 <- new_model(
    file.path(MODEL_DIR_BBI, "example2_saemimp"),
    .description = "complex2 test-test-threads model",
    .tags = ORIG_TAGS,
    .bbi_args = list(overwrite = TRUE, threads = 2)
  )

  mod_complex3 <- new_model(
    file.path(MODEL_DIR_BBI, "iovmm"),
    .description = "complex3 test-test-threads model",
    .tags = ORIG_TAGS,
    .bbi_args = list(overwrite = TRUE, threads = 2)
  )

  mod_complex4 <- new_model(
    file.path(MODEL_DIR_BBI, "acop-onlysim"),
    .description = "complex4 test-test-threads model",
    .tags = ORIG_TAGS,
    .bbi_args = list(overwrite = TRUE, threads = 2)
  )

  mods <- test_threads(mod1, .threads = c(2, 4), .cap_iterations = 100, .mode = "local", .dry_run = TRUE)

  test_that("test_threads(.dry_run=T) creates copy models [BBR-TSTT-001]", {
    mod_ctls <- lapply(mods, function(mod.x){get_model_path(mod.x)}) %>% unlist()
    mod_yamls <- lapply(mods, function(mod.x){mod.x$absolute_model_path}) %>% yaml_ext()

    expect_true(all(fs::file_exists(mod_ctls)))
    expect_true(all(fs::file_exists(mod_yamls)))
  })


  test_that("test_threads(.dry_run=T) correctly changes maxeval/niter: changes only one method [BBR-TSTT-002]", {

    max_evals <- get_est_options("MAXEVAL|NITER|NBURN", mods)
    expect_true(all(max_evals == 100))

    # Test that MAX works
    mods_complex3 <- test_threads(mod_complex3, .threads = c(2, 4), .cap_iterations = 100, .mode = "local", .dry_run = TRUE)

    max_evals <- get_est_options("MAXEVAL|NITER|NBURN|MAX", mods_complex3) # No estimation method provided here

    expect_true(all(max_evals == 100))
  })


  test_that("test_threads(.dry_run=T) correctly changes maxeval/niter: changes multiple methods [BBR-TSTT-002]", {

    mods_complex <- test_threads(mod_complex, .threads = c(2, 4), .cap_iterations = 100, .mode = "local", .dry_run = TRUE)

    # Dont overwrite NBURN if set to 0
    max_evals <- get_est_options("MAXEVAL|NITER|NBURN", mods_complex)

    for(i in seq_along(max_evals)){
      expect_identical(unname(max_evals[[i]]), c(100, 0, 100))
      # Confirm that estimation method didnt change, and that MAXEVAL/NITER was preserved
      expect_identical(names(max_evals[[i]]), c("METHOD=1, MAXEVAL", "METHOD=BAYES, NBURN", "METHOD=BAYES, NITER"))
    }
  })

  test_that("test_threads(.dry_run=T) correctly changes maxeval/niter: nburn is handled correctly [BBR-TSTT-002]", {

    expect_message(
      mods_complex2 <- test_threads(mod_complex2, .threads = c(2, 4), .cap_iterations = 100, .mode = "local", .dry_run = TRUE),
      "Adding NBURN declaration"
    )

    # Overwrite NBURN since was set to value other than 0
    max_evals <- get_est_options("MAXEVAL|NITER|NBURN", mods_complex2)

    for(i in seq_along(max_evals)){
      expect_identical(unname(max_evals[[i]]), c(100, 100, 100, 100))
      # Confirm that estimation method didnt change, and that MAXEVAL/NITER was preserved
      # This also confirms that NBURN is added when only NITER is specified (recursive test)
      expect_identical(names(max_evals[[i]]), c("METHOD=SAEM, NBURN", "METHOD=SAEM, NITER", "METHOD=IMP, NITER", "METHOD=IMP, NBURN"))
    }
  })

  test_that("test_threads(.dry_run=T) correctly changes maxeval/niter: keeps original if .cap_iterations = NULL [BBR-TSTT-002]", {

    mods_complex <- test_threads(mod_complex, .threads = c(2, 4), .cap_iterations = NULL, .mode = "local", .dry_run = TRUE)

    max_evals <- get_est_options("MAXEVAL|NITER|NBURN", mods_complex)

    for(i in 1:length(max_evals)){
      expect_equal(unname(max_evals[[i]]), c(9999, 0, 10))
      expect_equal(names(max_evals[[i]]), c("METHOD=1, MAXEVAL", "METHOD=BAYES, NBURN", "METHOD=BAYES, NITER"))
    }
  })

  test_that("test_threads(.dry_run=T) correctly changes maxeval/niter: warns if no $EST line found [BBR-TSTT-002]", {
    expect_warning(
      mods_complex4 <- test_threads(mod_complex4, .threads = c(2, 4), .cap_iterations = 4, .mode = "local", .dry_run = TRUE),
      glue("No Estimation line found")
    )
  })

  test_that("test_threads(.dry_run=T) threads are set correctly [BBR-TSTT-003]", {
    mod_threads <- lapply(mods, function(mod.x){mod.x$bbi_args$threads}) %>% unlist()
    expect_equal(mod_threads[1], 2)
    expect_equal(mod_threads[2], 4)
  })


  test_that("get_block_idx() works correctly: models [BBR-TSTT-004]", {
    mod_lines <- mod_complex %>% get_model_path() %>% readLines()
    expect_equal(get_block_idx(mod_lines) %>% unlist(), c(38, 39, 40))

    mod_lines <- mod_complex2 %>% get_model_path() %>% readLines()
    expect_equal(get_block_idx(mod_lines) %>% unlist(), c(111:115))
    expect_equal(get_block_idx(mod_lines)[[1]], c(111:113))
    expect_equal(get_block_idx(mod_lines)[[2]], c(114:115))

    mod_lines <- mod_complex3 %>% get_model_path() %>% readLines()
    expect_equal(get_block_idx(mod_lines)[[1]], 50)

  })

  test_that("get_block_idx() works correctly: custom text [BBR-TSTT-004]", {
    # Multiple lines per $EST, without leading spaces
    mod_lines <- c(
      "$EST METHOD=SAEM NBURN=3000 NITER=2000 PRINT=10 ISAMPLE=2",
      "ISAMPLE_M1=1 ISAMPLE_M2=1 ISAMPLE_M3=1",
      "CTYPE=3 CITER=10 CALPHA=0.05",
      "$EST METHOD=IMP INTERACTION EONLY=1 NITER=5 ISAMPLE=3000 PRINT=1 SIGL=8 SEED=123334",
      "CTYPE=3 CITER=10 CALPHA=0.05"
    )
    est_idxs <- get_block_idx(mod_lines)
    expect_equal(est_idxs %>% unlist(), c(1:5))
    expect_equal(est_idxs[[1]], c(1:3))
    expect_equal(est_idxs[[2]], c(4:5))

    # Multiple lines per $EST, with variable amount of leading spaces
    mod_lines <- c(
      " $EST METHOD=SAEM NBURN=3000 NITER=2000 PRINT=10 ISAMPLE=2",
      "  ISAMPLE_M1=1 ISAMPLE_M2=1 ISAMPLE_M3=1",
      "   CTYPE=3 CITER=10 CALPHA=0.05",
      "    $EST METHOD=IMP INTERACTION EONLY=1 NITER=5 ISAMPLE=3000 PRINT=1 SIGL=8 SEED=123334",
      "     CTYPE=3 CITER=10 CALPHA=0.05"
    )
    est_idxs <- get_block_idx(mod_lines)
    expect_equal(est_idxs %>% unlist(), c(1:5))
    expect_equal(est_idxs[[1]], c(1:3))
    expect_equal(est_idxs[[2]], c(4:5))

  })


  test_that("check_run_times() returns NA for dry runs [BBR-CRT-007]", {
    skip_if_old_bbi("3.2.0")
    expect_message(
      check_run_times(mod1, .wait = F),
      "Could not access data for 1"
    )
    run_times <- check_run_times(mod1, .wait = F) %>% suppressMessages()
    expect_equal(run_times$run, "1")
    expect_true(is.na(run_times$threads))
    expect_true(is.na(run_times$estimation_time))
  })


  test_that("delete_models() default: works for models created by test_threads [BBR-CLM-001]", {

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

  test_that("delete_models() default: errors informatively when no test_threads tag found [BBR-CLM-001]", {

    mod_fake1 <- copy_model_from(
      read_model(file.path(MODEL_DIR_BBI, "1")),
      "none",
      .overwrite = TRUE,
      .inherit_tags = FALSE
    )
    on.exit({
      .yp <- get_yaml_path(mod_fake1)
      .mp <- get_model_path(mod_fake1)
      unlink(.yp)
      unlink(.mp)
    })

    expect_error(
      delete_models(mod_fake1, .force = T),
      regexp = "defaults to.+test threads.+tags = NULL"
    )

  })

  test_that("delete_models() with .tags [BBR-CLM-002]", {

    mod_fake1 <- copy_model_from(
      read_model(file.path(MODEL_DIR_BBI, "1")),
      "none",
      .overwrite = TRUE
    ) %>% add_tags("fake1")

    mod_fake2 <- copy_model_from(
      read_model(file.path(MODEL_DIR_BBI, "1")),
      "both",
      .overwrite = TRUE
    ) %>% add_tags("fake2")

    mods_fake <- list(mod_fake1, mod_fake2)

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
    mods_threads <- test_threads(mod1, .threads = c(2, 4), .cap_iterations = 100, .mode = "local", .dry_run = TRUE)
    mod_new <- copy_model_from(read_model(file.path(MODEL_DIR_BBI, "1")), "one_tag") %>%
      add_tags("some tag")
    mod_no_tag <- copy_model_from(read_model(file.path(MODEL_DIR_BBI, "1")), "no_tag")

    mods <- c(mods_threads, list(mod_new), list(mod_no_tag))


    # When using NULL, models are not necessarily deleted in order
    # Only test that correct number of models are deleted
    msg_remove <- paste0(
      paste("Removed", length(mods), "models \\(ignoring tags\\)"))

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


