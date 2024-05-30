context("Model status functions")

withr::local_options(list(
  bbr.bbi_exe_path = read_bbi_path(),
  bbr.verbose = TRUE
))
describe("Model status helpers return the correct status", {

  it("Finished Running - bbi_config.json exists", {
    expect_equal(bbi_nonmem_model_status(MOD1), "Finished Running")
    expect_true(model_is_finished(MOD1))
    expect_true(check_nonmem_finished(MOD1))
    expect_message(
      get_model_status(MOD1),
      "The following model(s) have finished: `1`", fixed = TRUE
    )
  })

  # Test un-submitted model
  mod2 <- copy_model_from(MOD1)
  on.exit(delete_models(mod2, .tags = NULL, .force = TRUE))

  # Test un-submitted Bootstrap run
  .boot_run <- new_bootstrap_run(MOD1)
  on.exit(delete_models(.boot_run, .tags = NULL, .force = TRUE), add = TRUE)

  it("Not Run - no output directory", {
    expect_equal(bbi_nonmem_model_status(mod2), "Not Run")
    expect_false(model_is_finished(mod2))
    expect_false(check_nonmem_finished(mod2))
    expect_message(
      get_model_status(mod2),
      "The following model(s) are incomplete or have not yet been run: `2`",
      fixed = TRUE
    )

    # Bootstrap
    expect_equal(bbi_nonmem_model_status(.boot_run), "Not Run")
    expect_false(model_is_finished(.boot_run))
    expect_false(check_nonmem_finished(.boot_run))
    expect_message(
      get_model_status(.boot_run),
      "Bootstrap run `1-boot` has not been set up", fixed = TRUE
    )

    # After setup (only get_model_status changes)
    .boot_run <- setup_bootstrap_run(.boot_run, n = 2)
    expect_equal(bbi_nonmem_model_status(.boot_run), "Not Run")
    expect_false(model_is_finished(.boot_run))
    expect_false(check_nonmem_finished(.boot_run))
    expect_message(
      get_model_status(.boot_run),
      "The following model(s) are incomplete or have not yet been run: `1, 2`",
      fixed = TRUE
    )
    # Individual bootstrap models
    statuses <- purrr::map_chr(get_boot_models(.boot_run), bbi_nonmem_model_status)
    finished <- purrr::map_lgl(get_boot_models(.boot_run), model_is_finished)
    finished_exp <- purrr::map_lgl(get_boot_models(.boot_run), check_nonmem_finished)
    expect_true(all(statuses == "Not Run"))
    expect_false(any(finished))
    expect_false(any(finished_exp))
  })

  it("Incomplete Run - output directory exists", {
    mod2_dir <- get_output_dir(mod2, .check_exists = FALSE)
    fs::dir_create(mod2_dir)
    expect_equal(bbi_nonmem_model_status(mod2), "Incomplete Run")
    expect_false(model_is_finished(mod2))
    expect_false(check_nonmem_finished(mod2))
    expect_message(
      get_model_status(mod2),
      "The following model(s) are incomplete or have not yet been run: `2`",
      fixed = TRUE
    )
  })

  it("list of models", {
    # Uses a complex list (includes a bootstrap model)
    #  - Bootstrap runs get evaluated as a whole when they are part of a list
    mod_list <- list(MOD1, mod2, .boot_run)
    expect_equal(check_nonmem_finished(mod_list), c(TRUE, FALSE, FALSE))
    expect_message(
      get_model_status(mod_list),
      "The following model(s) have finished: `1`", fixed = TRUE
    )
    expect_message(
      get_model_status(mod_list),
      "The following model(s) are incomplete or have not yet been run: `2, 1-boot`",
      fixed = TRUE
    )
  })
})