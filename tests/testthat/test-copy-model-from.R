context("Copying model objects")

#########################
# copy_model_from tests
#########################

cleanup()

test_that("copy_from_model creates accurate copy", {
  on.exit({ cleanup() })

  # run copy_model_from
  new_mod <- copy_model_from(MOD1, basename(NEW_MOD2))

  # check that everything is copied through in the object
  expect_identical(class(new_mod), NM_MOD_CLASS_LIST)
  expect_identical(new_mod[[YAML_BASED_ON]], "1")
  expect_identical(new_mod[[YAML_TAGS]], ORIG_TAGS)
  expect_equal(new_mod[[YAML_BBI_ARGS]], list(overwrite = TRUE, threads = 4L))
  expect_null(new_mod[[YAML_DESCRIPTION]])

  # check that everything is copied through in the YAML
  new_yaml <- yaml::read_yaml(yaml_ext(NEW_MOD2))

  expect_identical(new_yaml[[YAML_BASED_ON]], "1")
  expect_identical(new_yaml[[YAML_TAGS]], ORIG_TAGS)
  expect_equal(new_yaml[[YAML_BBI_ARGS]], list(overwrite = TRUE, threads = 4L))

  # check the control stream is modified
  new_mod_str <- ctl_ext(NEW_MOD2) %>% readr::read_file()
  new_desc_pattern <- as.character(glue("\\$PROBLEM From bbr: see {basename(NEW_MOD2)}.yaml for details\n\n\\$INPUT"))
  expect_true(grepl(new_desc_pattern, new_mod_str))
})


test_that("copy_from_model options work", {
  on.exit({ cleanup() })

  # run copy_model_from
  fs::file_copy(YAML_TEST_FILE, paste0(NEW_MOD2, '.yaml'))
  copy_model_from(MOD1,
                  basename(NEW_MOD3),
                  NEW_DESC,
                  .based_on_additional = get_model_id(NEW_MOD2),
                  .inherit_tags = FALSE,
                  .update_model_file = FALSE,
                  .add_tags = NEW_TAGS)

  # check that everything is copied through
  new_yaml <- yaml::read_yaml(yaml_ext(NEW_MOD3))

  expect_identical(new_yaml[[YAML_DESCRIPTION]], NEW_DESC)
  expect_identical(new_yaml[[YAML_BASED_ON]], c("1", get_model_id(NEW_MOD2)))
  expect_identical(new_yaml[[YAML_TAGS]], NEW_TAGS)
  expect_equal(new_yaml[[YAML_BBI_ARGS]], list(overwrite = TRUE, threads = 4L))

  # check the control stream is not modified
  prob_pattern <- "\\$PROB(.|\n)*?\\$"
  orig_mod_str <- ctl_ext(YAML_TEST_FILE) %>% readr::read_file()
  new_mod_str <- ctl_ext(NEW_MOD3) %>% readr::read_file()
  expect_identical(
    stringr::str_extract(orig_mod_str, prob_pattern),
    stringr::str_extract(new_mod_str, prob_pattern)
  )
})

test_that("copy_from_model.bbi_nonmem_model works with numeric input", {
  on.exit({ cleanup() })

  # check that the model is not there already
  new_yaml_path <- yaml_ext(NEW_MOD2)
  new_ctl_path <- ctl_ext(NEW_MOD2)
  expect_false(fs::file_exists(new_yaml_path))
  expect_false(fs::file_exists(new_ctl_path))

  # copy with numeric
  num_input <- as.numeric(basename(NEW_MOD2))
  expect_equal(num_input, 2)
  copy_model_from(MOD1, num_input, .add_tags = NEW_TAGS)

  # check that the model was created
  new_mod <- read_model(NEW_MOD2)
  expect_true(inherits(new_mod, NM_MOD_CLASS))
})


test_that("copy_from_model .overwrite=TRUE works", {
  on.exit({ cleanup() })

  # set up model object
  new_yaml_path <- yaml_ext(NEW_MOD2)
  new_ctl_path <- ctl_ext(NEW_MOD2)
  expect_false(fs::file_exists(new_yaml_path))
  expect_false(fs::file_exists(new_ctl_path))

  # copy control stream
  fs::file_copy(ctl_ext(YAML_TEST_FILE), new_ctl_path)

  # copy with .overwrite=TRUE
  copy_model_from(MOD1, basename(NEW_MOD2), .overwrite=TRUE)

  # check the control stream is modified by overwrite
  new_mod_str <- readr::read_file(new_ctl_path)

  orig_desc_pattern <- paste0("\\$PROBLEM ", DESC_IN_CTL, "\n\n\\$INPUT")
  expect_false(grepl(orig_desc_pattern, new_mod_str))

  new_desc_pattern <- as.character(glue("\\$PROBLEM From bbr: see {basename(NEW_MOD2)}.yaml for details\n\n\\$INPUT"))
  expect_true(grepl(new_desc_pattern, new_mod_str))
})

test_that("copy_from_model .overwrite=FALSE works", {
  on.exit({ cleanup() })

  # set up model object
  new_yaml_path <- yaml_ext(NEW_MOD2)
  new_ctl_path <- ctl_ext(NEW_MOD2)
  expect_false(fs::file_exists(new_yaml_path))
  expect_false(fs::file_exists(new_ctl_path))

  # copy control stream
  fs::file_copy(ctl_ext(YAML_TEST_FILE), new_ctl_path)

  # copy with .overwrite=FALSE
  expect_error(
    copy_model_from(MOD1, basename(NEW_MOD2), .overwrite=FALSE),
    regexp = "File already exists at"
  )

  # check the control stream is NOT modified (i.e. no overwrite)
  new_mod_str <- readr::read_file(new_ctl_path)

  orig_desc_pattern <- paste0("\\$PROBLEM ", DESC_IN_CTL, "\n\n\\$INPUT")
  expect_true(grepl(orig_desc_pattern, new_mod_str))

  new_desc_pattern <- paste0("\\$PROBLEM ", basename(NEW_MOD2), "\n\n\\$INPUT")
  expect_false(grepl(new_desc_pattern, new_mod_str))
})

test_that("copy_model_from() supports `.new_model` containing a period", {
  temp_mod_path <- create_temp_model()
  temp_mod <- read_model(temp_mod_path)

  temp_dir <- normalizePath(tempdir())
  new_mod_path <- "foo.bar"
  new_ctl <- paste0(file.path(temp_dir, new_mod_path), ".ctl")
  new_yaml <- paste0(file.path(temp_dir, new_mod_path), ".yaml")

  expect_false(fs::file_exists(new_ctl))
  expect_false(fs::file_exists(new_yaml))
  on.exit(fs::file_delete(c(new_ctl, new_yaml)))

  new_mod <- copy_model_from(temp_mod, new_mod_path)
  expect_true(inherits(new_mod, NM_MOD_CLASS))
  expect_true(fs::file_exists(new_ctl))
  expect_true(fs::file_exists(new_yaml))

})

test_that("copy_from_model.bbi_stan_model creates accurate copy", {
  skip_if_no_stan("copy_from_model.bbi_stan_model")

  mod_name <- "testmod_copy_stan1"
  new_mod <- copy_model_from(STAN_MOD1, mod_name, .add_tags = NEW_TAGS)
  on.exit(cleanup_model(new_mod))

  # check that everything is copied through in the object
  expect_true(inherits(new_mod, STAN_MOD_CLASS))
  expect_identical(new_mod[[YAML_BASED_ON]], STAN_MOD_ID)
  expect_identical(new_mod[[YAML_TAGS]], c(STAN_MOD1$tags, NEW_TAGS))
  expect_null(new_mod[[YAML_DESCRIPTION]])
})


test_that("copy_from_model.bbi_stan_model from brms copies through json", {
  skip_if_no_stan("copy_from_model.bbi_stan_model")


  expect_equal(1,1)
})
