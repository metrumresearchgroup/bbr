context("Workflow file manipulation")

# define new mod vars
orig_mod_yaml_path <- "model-examples/1.yaml"
new_mod2 <- "model-examples/2"
new_mod3 <- "model-examples/3"
new_desc <- "new description"
new_tags <- c("new tag 1", "new tag 2")

# delete tmp files if they are leftover from previous test
for (m in c(new_mod2, new_mod3)) {
  if (fs::file_exists(yaml_ext(m))) fs::file_delete(yaml_ext(m))
  if (fs::file_exists(ctl_ext(m))) fs::file_delete(ctl_ext(m))
}

test_that("copy_from_model creates accurate copy", {
  # run copy_model_from
  copy_model_from(orig_mod_yaml_path, new_mod2, new_desc, .add_tags = new_tags)

  # check that everything is copied through
  new_yaml <- yaml::read_yaml(yaml_ext(new_mod2))

  expect_identical(new_yaml[[YAML_MOD_PATH]], basename(ctl_ext(new_mod2)))
  expect_identical(new_yaml[[YAML_DESCRIPTION]], new_desc)
  expect_identical(new_yaml[[YAML_BASED_ON]], "1")
  expect_identical(new_yaml[[YAML_TAGS]], new_tags)
  expect_equal(new_yaml[[YAML_BBI_ARGS]], list(overwrite = TRUE, threads = 4L))

  # check the control stream is modified
  new_mod_str <- ctl_ext(new_mod2) %>% readr::read_file()
  new_desc_pattern <- paste0("\\$PROBLEM ", get_mod_id(new_mod2), " ", new_desc, "\n\n\\$INPUT")
  expect_true(grepl(new_desc_pattern, new_mod_str))

})


test_that("copy_from_model options work", {
  # run copy_model_from
  copy_model_from(orig_mod_yaml_path,
                  new_mod3,
                  new_desc,
                  .based_on_additional = get_mod_id(new_mod2),
                  .inherit_tags = TRUE,
                  .update_mod_file = FALSE)

  # check that everything is copied through
  new_yaml <- yaml::read_yaml(yaml_ext(new_mod3))

  expect_identical(new_yaml[[YAML_MOD_PATH]], basename(ctl_ext(new_mod3)))
  expect_identical(new_yaml[[YAML_DESCRIPTION]], new_desc)
  expect_identical(new_yaml[[YAML_BASED_ON]], c("1", get_mod_id(new_mod2)))
  expect_identical(new_yaml[[YAML_TAGS]], c("acop tag", "other tag"))
  expect_equal(new_yaml[[YAML_BBI_ARGS]], list(overwrite = TRUE, threads = 4L))

  # check the control stream is not modified
  prob_pattern <- "\\$PROB(.|\n)*?\\$"
  orig_mod_str <- ctl_ext(orig_mod_yaml_path) %>% readr::read_file()
  new_mod_str <- ctl_ext(new_mod3) %>% readr::read_file()
  expect_identical(
    stringr::str_extract(orig_mod_str, prob_pattern),
    stringr::str_extract(new_mod_str, prob_pattern)
  )
})


test_that("run_log matches reference tibble", {
  df <- suppressWarnings(run_log("model-examples"))
  ref_df <- readRDS("data/run_log_basic_200224.rds")
  expect_identical(df, ref_df)
})


# cleanup temp files at the end
for (m in c(new_mod2, new_mod3)) {
  if (fs::file_exists(yaml_ext(m))) fs::file_delete(yaml_ext(m))
  if (fs::file_exists(ctl_ext(m))) fs::file_delete(ctl_ext(m))
}



test_that("parse_mod_yaml() returns expected list", {
  ref_list <- list(
    description = "original acop model",
    model_type = "nonmem",
    model_path = "1.ctl",
    tags = c("acop tag", "other tag"),
    bbi_args = list(
      overwrite = TRUE,
      threads = 4L),
    orig_working_dir = file.path(getwd(), "model-examples"),
    orig_yaml_file ="1.yaml"
  )
  class(ref_list) <- c("bbi_nonmem_spec", "list")
  expect_equal(parse_mod_yaml("model-examples/1.yaml"), ref_list)

})


test_that("yaml with no model type will fail", {
  expect_error(parse_mod_yaml("test-yaml/zz_fail_no_modtype.yaml"))
})

test_that("yaml with bad model path will fail", {
  expect_error(parse_mod_yaml("test-yaml/zz_fail_bad_modpath.yaml"))
})

test_that("yaml with no model path will return ctl", {
  .test_path <- "test-yaml/zz_pass_no_modpath"
  .spec <- parse_mod_yaml(yaml_ext(.test_path))
  expect_identical(.spec[[YAML_MOD_PATH]], basename(ctl_ext(.test_path)))
})
