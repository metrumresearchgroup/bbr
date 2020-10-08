context("submit_model(.dry_run=T)")

###################################
# testing single model submission
###################################

withr::with_options(list(rbabylon.model_directory = NULL), {

  # create fake babylon.yaml
  readr::write_file("created_by: test-submit-model", "babylon.yaml")
  on.exit({ fs::file_delete("babylon.yaml")})

  model_dir <- file.path(getwd(), MODEL_DIR)
  mod_ctl_path <- file.path(model_dir, CTL_FILENAME)

  test_that("submit_model(.dry_run=T) returns correct command string",
            {
              withr::with_options(list(rbabylon.bbi_exe_path = "bbi"), {
                # correctly parsing yaml
                expect_identical(
                  submit_model(MOD1, .dry_run = T)[[PROC_CALL]],
                  as.character(glue("cd {model_dir} ; bbi nonmem run sge {mod_ctl_path} --overwrite --threads=4"))
                )

                # switch to local mode
                expect_identical(
                  submit_model(MOD1, .mode = "local", .dry_run = T)[[PROC_CALL]],
                  as.character(glue("cd {model_dir} ; bbi nonmem run local {mod_ctl_path} --overwrite --threads=4"))
                )

                # over-riding yaml arg with passed args
                expect_identical(
                  submit_model(MOD1,
                               .bbi_args=list(
                                 "json" = T,
                                 "threads" = 2,
                                 "nm_version" = "nm74"
                               ),
                               .dry_run = T)[[PROC_CALL]],
                  as.character(glue("cd {model_dir} ; bbi nonmem run sge {mod_ctl_path} --overwrite --threads=2 --json --nm_version=nm74"))
                )
              })
            })

  test_that("submit_model(.dry_run=T) with bbi_nonmem_model object parses correctly",
            {
              withr::with_options(list(rbabylon.bbi_exe_path = "bbi"), {
                # correctly parsing yaml
                expect_identical(
                  submit_model(MOD1, .dry_run = T)[[PROC_CALL]],
                  as.character(glue("cd {model_dir} ; bbi nonmem run sge {mod_ctl_path} --overwrite --threads=4"))
                )

                # over-riding yaml arg with passed arg
                expect_identical(
                  submit_model(MOD1, list(threads=2), .dry_run = T)[[PROC_CALL]],
                  as.character(glue("cd {model_dir} ; bbi nonmem run sge {mod_ctl_path} --overwrite --threads=2"))
                )

              })
            })

  test_that("submit_model() creates correct call for non-NULL .config_path", {
    temp_config <- tempfile(fileext = ".yaml")
    readr::write_file("foo", temp_config)
    on.exit(fs::file_delete(temp_config))

    res <- submit_model(MOD1, .config_path = temp_config, .dry_run = TRUE)
    expect_identical(
      res[[PROC_CALL]],
      as.character(
        glue::glue(
          "cd {model_dir} ;",
          "bbi nonmem run sge {mod_ctl_path} --overwrite --threads=4",
          "--config={temp_config}",
          .sep = " "
        )
      )
    )
  })

  test_that("submit_model() works with non-NULL .config_path", {
    skip_if(Sys.getenv("SKIP_BBI_TEST") == "true")
    skip_if(Sys.getenv("DRONE") == "true")

    test_dir <- getwd()
    withr::with_tempdir({
      # temporarily put babylon.yaml in the temp dir
      withr::with_options(list(rbabylon.bbi_exe_path = read_bbi_path()), {
        bbi_init(".", "/opt/NONMEM", "nm74gf")
      })

      # copy model, YAML, and data files to the same location
      files_to_copy <- file.path(
        test_dir,
        MODEL_DIR,
        c("1.ctl", "1.yaml", "../data/acop.csv")
      )

      purrr::walk(files_to_copy, fs::file_copy, ".")

      # modify DATA to reflect location in temp dir
      readr::read_file("1.ctl") %>%
        stringr::str_replace("\\$DATA\\s+[^\\s]+", "$DATA ../acop.csv") %>%
        readr::write_file("1.ctl")

      # set up temp config in a different location
      temp_config <- tempfile(fileext = ".yaml")
      fs::file_copy("babylon.yaml", temp_config)
      on.exit(fs::file_delete(temp_config))

      # and delete babylon.yaml from the temp dir
      fs::file_delete("babylon.yaml")

      mod <- read_model("1")
      res <- submit_model(
        mod,
        .mode = "local",
        .bbi_exe_path = read_bbi_path(),
        .config_path = temp_config,
        .wait = TRUE
      )

      expect_true(any(grepl("models completed", res[["stdout"]], fixed = TRUE)))
    })
  })

}) # closing withr::with_options
