context("Workflow file manipulation")

# define new mod vars
orig_mod <- "data/acop"
new_mod2 <- "data/modtest2"
new_mod3 <- "data/modtest3"
new_desc <- "new description"
new_tags <- c("new tag 1", "new tag 2")

# delete tmp files if they are leftover from previous test
for (m in c(new_mod2, new_mod3)) {
  if (fs::file_exists(paste0(m, ".yaml"))) fs::file_delete(paste0(m, ".yaml"))
  if (fs::file_exists(paste0(m, ".mod"))) fs::file_delete(paste0(m, ".mod"))
}

test_that("copy_from_model creates accurate copy", {
  # run copy_model_from
  copy_model_from(orig_mod, new_mod2, new_desc)

  # check that everything is copied through
  new_yaml <- yaml::read_yaml(paste0(new_mod2, ".yaml"))

  expect_identical(new_yaml[[YAML_MOD_PATH]], paste0(new_mod2, ".mod"))
  expect_identical(new_yaml[[YAML_DESCRIPTION]], new_desc)
  expect_identical(new_yaml[[YAML_BASED_ON]], "acop")
  expect_identical(new_yaml[[YAML_TAGS]], c("acop tag", "other tag"))
  expect_identical(new_yaml[[YAML_BBI_ARGS]], list(overwrite = TRUE, threads = 4L, nm_version = "nm74gf"))

  # check the control stream is modified
  new_mod_str <- readr::read_file(paste0(new_mod2, ".mod"))
  new_desc_pattern <- paste0("\\$PROBLEM ", get_mod_id(new_mod2), " ", new_desc, "\n\n\\$INPUT")
  expect_true(grepl(new_desc_pattern, new_mod_str))

})


test_that("copy_from_model options work", {
  # run copy_model_from
  copy_model_from(orig_mod,
                  new_mod3,
                  new_desc,
                  .based_on_additional = get_mod_id(new_mod2),
                  .inherit_tags = FALSE,
                  .add_tags = new_tags,
                  .update_mod_file = FALSE)

  # check that everything is copied through
  new_yaml <- yaml::read_yaml(paste0(new_mod3, ".yaml"))

  expect_identical(new_yaml[[YAML_MOD_PATH]], paste0(new_mod3, ".mod"))
  expect_identical(new_yaml[[YAML_DESCRIPTION]], new_desc)
  expect_identical(new_yaml[[YAML_BASED_ON]], c("acop", get_mod_id(new_mod2)))
  expect_identical(new_yaml[[YAML_TAGS]], new_tags)
  expect_identical(new_yaml[[YAML_BBI_ARGS]], list(overwrite = TRUE, threads = 4L, nm_version = "nm74gf"))

  # check the control stream is not modified
  prob_pattern <- "\\$PROB(.|\n)*?\\$"
  orig_mod_str <- readr::read_file(paste0(orig_mod, ".mod"))
  new_mod_str <- readr::read_file(paste0(new_mod3, ".mod"))
  expect_identical(
    stringr::str_extract(orig_mod_str, prob_pattern),
    stringr::str_extract(new_mod_str, prob_pattern)
  )
})


test_that("create_run_log matches reference tibble", {
  df <- suppressWarnings(create_run_log("data"))
  ref_df <- readRDS("data/run_log_basic_200211.rds")
  expect_identical(df, ref_df)
})


# cleanup temp files at the end
for (m in c(new_mod2, new_mod3)) {
  if (fs::file_exists(paste0(m, ".yaml"))) fs::file_delete(paste0(m, ".yaml"))
  if (fs::file_exists(paste0(m, ".mod"))) fs::file_delete(paste0(m, ".mod"))
}
