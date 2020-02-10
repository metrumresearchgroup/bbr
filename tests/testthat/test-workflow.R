context("Testing workflow file manipulation")

# delete modtest2 stuff, if exists?

test_that("copy_from_model creates accurate copy", {
  # run copy_model_from
  new_mod <- "data/modtest2"
  new_desc <- "new description"
  copy_model_from("data/modtest", new_mod, new_desc)

  # check that everything is copied through
  new_yaml <- yaml::read_yaml(paste0(new_mod, ".yaml"))

  expect_identical(new_yaml[[YAML_MOD_PATH]], paste0(new_mod, ".mod"))
  expect_identical(new_yaml[[YAML_DESCRIPTION]], new_desc)
  expect_identical(new_yaml[[YAML_BASED_ON]], "acop")
  expect_identical(new_yaml[[YAML_TAGS]], c("acop tag", "other tag"))
  expect_identical(new_yaml[[YAML_BBI_ARGS]], list(overwrite = TRUE, threads = 4L, nm_version = "nm74gf"))

  # check the control stream is modified
  new_mod_str <- readr::read_file(paste0(new_mod, ".mod"))
  new_desc_pattern <- paste0("\\$PROBLEM ", get_mod_id(new_mod), " ", new_desc, "\n\n\\$INPUT")
  expect_true(grepl(new_desc_pattern, new_mod_str))

})

# cleanup modtest2 stuff at the end
fs::file_delete(paste0(new_mod, ".yaml")) # if exists?
fs::file_delete(paste0(new_mod, ".mod")) # if exists?
