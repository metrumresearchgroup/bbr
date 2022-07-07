context("Copying model objects")

#########################
# copy_model_from tests
#########################

cleanup()

test_that("copy_from_model creates accurate copy [BBR-CMF-001]", {
  on.exit({ cleanup() })

  # run copy_model_from
  new_mod <- copy_model_from(MOD1, basename(NEW_MOD2), .add_tags = NEW_TAGS)

  # check that everything is copied through in the object
  expect_identical(class(new_mod), NM_MOD_CLASS_LIST)
  expect_identical(new_mod[[YAML_BASED_ON]], "1")
  expect_identical(new_mod[[YAML_TAGS]], NEW_TAGS)
  expect_equal(new_mod[[YAML_BBI_ARGS]], list(overwrite = TRUE, threads = 4L, parallel = TRUE))
  expect_null(new_mod[[YAML_DESCRIPTION]])

  # check that everything is copied through in the YAML
  new_yaml <- yaml::read_yaml(yaml_ext(NEW_MOD2))

  expect_identical(new_yaml[[YAML_BASED_ON]], "1")
  expect_identical(new_yaml[[YAML_TAGS]], NEW_TAGS)
  expect_equal(new_yaml[[YAML_BBI_ARGS]], list(overwrite = TRUE, threads = 4L, parallel = TRUE))

  # check the control stream is modified
  new_mod_str <- ctl_ext(NEW_MOD2) %>% readr::read_file()
  new_desc_pattern <- as.character(glue("\\$PROBLEM From bbr: see {basename(NEW_MOD2)}.yaml for details\n\n\\$INPUT"))
  expect_true(grepl(new_desc_pattern, new_mod_str))
})


test_that("copy_from_model options work [BBR-CMF-002]", {
  on.exit({ cleanup() })

  # run copy_model_from
  fs::file_copy(YAML_TEST_FILE, paste0(NEW_MOD2, '.yaml'))
  copy_model_from(MOD1,
                  basename(NEW_MOD3),
                  NEW_DESC,
                  .based_on_additional = get_model_id(NEW_MOD2),
                  .inherit_tags = TRUE,
                  .update_model_file = FALSE)

  # check that everything is copied through
  new_yaml <- yaml::read_yaml(yaml_ext(NEW_MOD3))

  expect_identical(new_yaml[[YAML_DESCRIPTION]], NEW_DESC)
  expect_identical(new_yaml[[YAML_BASED_ON]], c("1", get_model_id(NEW_MOD2)))
  expect_identical(new_yaml[[YAML_TAGS]], ORIG_TAGS)
  expect_equal(new_yaml[[YAML_BBI_ARGS]], list(overwrite = TRUE, threads = 4L, parallel = TRUE))

  # check the control stream is not modified
  prob_pattern <- "\\$PROB(.|\n)*?\\$"
  orig_mod_str <- ctl_ext(YAML_TEST_FILE) %>% readr::read_file()
  new_mod_str <- ctl_ext(NEW_MOD3) %>% readr::read_file()
  expect_identical(
    stringr::str_extract(orig_mod_str, prob_pattern),
    stringr::str_extract(new_mod_str, prob_pattern)
  )


  #Editing YAML to add star status is true from clean model
  temp_mod_path <- create_temp_model()
  new_mod <- read_model(temp_mod_path)
  expect_null(new_mod$star)
  new_mod <- add_star(new_mod)

  #Checking that star is written out to the yaml
  model_yml <- paste0(temp_mod_path, ".yaml") %>% read_yaml()
  yaml_fields <- model_yml %>% names()
  expect_true("star" %in% yaml_fields)

  #Checking the yaml is correctly specified with true
  expect_true(model_yml$star)

})

test_that("copy_from_model.bbi_nonmem_model works with numeric input [BBR-CMF-003]", {
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


test_that("copy_from_model .overwrite=TRUE works [BBR-CMF-004]", {
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

test_that("copy_from_model .overwrite=FALSE works [BBR-CMF-005]", {
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

test_that("copy_model_from() supports `.new_model` containing a period [BBR-CMF-006]", {
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


test_that("copy_model_from(.new_model=NULL) increments to next integer by default [BBR-CMF-007]", {
  withr::defer(cleanup())
  new_id <- "002"
  new_mod1 <- copy_model_from(MOD1, file.path(basename(LEVEL2_DIR), new_id))
  new_mod2 <- copy_model_from(new_mod1)
  expect_equal(
    as.integer(get_model_id(new_mod1)) + 1,
    as.integer(get_model_id(new_mod2))
  )
  expect_equal(!!nchar(get_model_id(new_mod2)), !!nchar(new_id))
})


test_that("copy_model_from(.new_model=NULL) increments to next highest integer [BBR-CMF-007]", {
  withr::defer(cleanup())
  new_id <- "002"
  high_id <- "0100"
  bad_id <- "aaaaa10000aaaaa"
  new_mod1 <- copy_model_from(MOD1, file.path(basename(LEVEL2_DIR), new_id))
  # write a fake ctl with a higher number
  readr::write_file("naw", file.path(LEVEL2_DIR, paste0(high_id, ".ctl")))
  readr::write_file("naw", file.path(LEVEL2_DIR, paste0(bad_id, ".ctl")))

  # copy and check it increments over the higher number
  new_mod2 <- copy_model_from(new_mod1)
  expect_equal(
    as.integer(high_id) + 1,
    as.integer(get_model_id(new_mod2))
  )
  expect_equal(!!nchar(high_id), !!nchar(get_model_id(new_mod2)))
})

test_that("copy_model_from(.new_model=NULL) errors when no models are valid integer [BBR-CMF-007]", {
  withr::defer(cleanup())
  new_id <- "AAA"
  new_mod1 <- copy_model_from(MOD1, file.path(basename(LEVEL2_DIR), new_id))

  expect_error({
    new_mod2 <- copy_model_from(new_mod1)
  }, regexp = "no models.+integer")
})
