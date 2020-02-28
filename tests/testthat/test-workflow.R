context("Workflow file manipulation")

# define new mod vars
YAML_TEST_FILE <- "model-examples/1.yaml"
NEW_MOD2 <- "model-examples/2"
NEW_MOD3 <- "model-examples/3"

ORIG_DESC <- "original acop model"
NEW_DESC <- "new description"

ORIG_TAGS <- c("acop tag", "other tag")
NEW_TAGS <- c("new tag 1", "new tag 2")

NEW_TEXT1 <- c("naw", "paw")
NEW_TEXT2 <- c("all", "done")

#########################
# copy_model_from tests
#########################

# delete tmp files if they are leftover from previous test
for (m in c(NEW_MOD2, NEW_MOD3)) {
  if (fs::file_exists(yaml_ext(m))) fs::file_delete(yaml_ext(m))
  if (fs::file_exists(ctl_ext(m))) fs::file_delete(ctl_ext(m))
}

test_that("copy_from_model creates accurate copy", {
  # run copy_model_from
  copy_model_from(YAML_TEST_FILE, NEW_MOD2, NEW_DESC, .add_tags = NEW_TAGS)

  # check that everything is copied through
  new_yaml <- yaml::read_yaml(yaml_ext(NEW_MOD2))

  expect_identical(new_yaml[[YAML_MOD_PATH]], basename(ctl_ext(NEW_MOD2)))
  expect_identical(new_yaml[[YAML_DESCRIPTION]], NEW_DESC)
  expect_identical(new_yaml[[YAML_BASED_ON]], "1")
  expect_identical(new_yaml[[YAML_TAGS]], NEW_TAGS)
  expect_equal(new_yaml[[YAML_BBI_ARGS]], list(overwrite = TRUE, threads = 4L))

  # check the control stream is modified
  new_mod_str <- ctl_ext(NEW_MOD2) %>% readr::read_file()
  new_desc_pattern <- paste0("\\$PROBLEM ", get_mod_id(NEW_MOD2), " ", NEW_DESC, "\n\n\\$INPUT")
  expect_true(grepl(new_desc_pattern, new_mod_str))

})


test_that("copy_from_model options work", {
  # run copy_model_from
  copy_model_from(YAML_TEST_FILE,
                  NEW_MOD3,
                  NEW_DESC,
                  .based_on_additional = get_mod_id(NEW_MOD2),
                  .inherit_tags = TRUE,
                  .update_mod_file = FALSE)

  # check that everything is copied through
  new_yaml <- yaml::read_yaml(yaml_ext(NEW_MOD3))

  expect_identical(new_yaml[[YAML_MOD_PATH]], basename(ctl_ext(NEW_MOD3)))
  expect_identical(new_yaml[[YAML_DESCRIPTION]], NEW_DESC)
  expect_identical(new_yaml[[YAML_BASED_ON]], c("1", get_mod_id(NEW_MOD2)))
  expect_identical(new_yaml[[YAML_TAGS]], ORIG_TAGS)
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

test_that("run_log matches reference tibble", {
  df <- suppressWarnings(run_log("model-examples"))
  ref_df <- readRDS("data/run_log_basic_200224.rds")
  expect_identical(df, ref_df)
})

# cleanup temp files
for (m in c(NEW_MOD2, NEW_MOD3)) {
  if (fs::file_exists(yaml_ext(m))) fs::file_delete(yaml_ext(m))
  if (fs::file_exists(ctl_ext(m))) fs::file_delete(ctl_ext(m))
}

test_that("copy_from_model bbi_nonmem_spec", {
  # run copy_model_from on a spec object
  spec1 <- create_model_from_yaml(YAML_TEST_FILE)
  new_yaml_path <- yaml_ext(NEW_MOD2)
  new_ctl_path <- ctl_ext(NEW_MOD2)
  expect_false(fs::file_exists(new_yaml_path))
  expect_false(fs::file_exists(new_ctl_path))
  copy_model_from(spec1, NEW_MOD2, NEW_DESC, .add_tags = NEW_TAGS)

  # check that everything is copied through
  new_yaml <- yaml::read_yaml(new_yaml_path)

  expect_identical(new_yaml[[YAML_MOD_PATH]], basename(new_ctl_path))
  expect_identical(new_yaml[[YAML_DESCRIPTION]], NEW_DESC)
  expect_identical(new_yaml[[YAML_BASED_ON]], "1")
  expect_identical(new_yaml[[YAML_TAGS]], NEW_TAGS)
  expect_equal(new_yaml[[YAML_BBI_ARGS]], list(overwrite = TRUE, threads = 4L))

  # check the control stream is modified
  new_mod_str <- readr::read_file(new_ctl_path)
  new_desc_pattern <- paste0("\\$PROBLEM ", get_mod_id(NEW_MOD2), " ", NEW_DESC, "\n\n\\$INPUT")
  expect_true(grepl(new_desc_pattern, new_mod_str))

  # cleanup
  fs::file_delete(new_yaml_path)
  fs::file_delete(new_ctl_path)
})


#########################
# interacting with YAML
#########################

test_that("parse_mod_yaml() returns expected list", {
  ref_list <- list(
    description = ORIG_DESC,
    model_type = "nonmem",
    model_path = "1.ctl",
    tags = ORIG_TAGS,
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

test_that("save_mod_yaml() saves to correct default path", {
  # make a new yaml
  new_yaml <- yaml_ext(NEW_MOD2)
  fs::file_copy(YAML_TEST_FILE, new_yaml)

  # make a spec from it
  new_spec <- create_model_from_yaml(new_yaml)

  # delete the underlying yaml
  fs::file_delete(new_yaml)
  expect_false(fs::file_exists(new_yaml))

  # re-save yaml
  save_mod_yaml(new_spec)

  # look for it
  expect_true(fs::file_exists(new_yaml))

  # cleanup
  fs::file_delete(new_yaml)
})

test_that("save_mod_yaml() saves to user supplied path", {
  # give fake path
  fake_path <- "model-examples/fake.yaml"
  expect_false(fs::file_exists(fake_path))

  # make a spec
  new_spec <- create_model_from_yaml(YAML_TEST_FILE)

  # re-save yaml
  save_mod_yaml(new_spec, fake_path)

  # look for it
  expect_true(fs::file_exists(fake_path))

  # cleanup
  fs::file_delete(fake_path)
})

test_that("save_mod_yaml() deletes the right keys", {
  # give fake path
  fake_path <- "model-examples/fake.yaml"
  expect_false(fs::file_exists(fake_path))

  # make a spec
  new_spec <- create_model_from_yaml(YAML_TEST_FILE)

  # re-save yaml
  save_mod_yaml(new_spec, fake_path)

  # read it back in and check the keys
  loaded_yaml <- yaml::read_yaml(fake_path)
  expect_false(any(names(loaded_yaml) %in% YAML_ERASE_OUT_KEYS))

  # cleanup
  fs::file_delete(fake_path)
})


test_that("reconcile_mod_yaml() pulls in new tags", {
  # make a new yaml
  new_yaml <- yaml_ext(NEW_MOD2)
  fs::file_copy(YAML_TEST_FILE, new_yaml)

  # make a spec from it
  new_spec <- create_model_from_yaml(new_yaml)
  expect_identical(new_spec[[YAML_TAGS]], ORIG_TAGS)

  # add the tags to the yaml manually
  rogue_spec <- yaml::read_yaml(new_yaml)
  rogue_spec[[YAML_TAGS]] <- c(rogue_spec[[YAML_TAGS]], NEW_TAGS)
  yaml::write_yaml(rogue_spec, new_yaml)

  # check the reconcile add the new tags
  new_spec <- reconcile_mod_yaml(new_spec, new_yaml)
  expect_identical(new_spec[[YAML_TAGS]], c(ORIG_TAGS, NEW_TAGS))

  # cleanup
  fs::file_delete(new_yaml)
})



######################################
# modify_spec_field and its wrappers
######################################

test_that("modify_spec_field() works correctly", {
  # make a new yaml
  new_yaml <- yaml_ext(NEW_MOD2)
  fs::file_copy(YAML_TEST_FILE, new_yaml)

  # make a spec from it
  new_spec <- create_model_from_yaml(new_yaml)
  expect_identical(new_spec[[YAML_TAGS]], ORIG_TAGS)

  # modify the tags field
  new_spec <- modify_spec_field(new_spec, .field=YAML_TAGS, .value=NEW_TAGS)
  expect_identical(new_spec[[YAML_TAGS]], c(ORIG_TAGS, NEW_TAGS))

  # check that the yaml was also modified
  rogue_spec <- yaml::read_yaml(new_yaml)
  expect_identical(rogue_spec[[YAML_TAGS]], c(ORIG_TAGS, NEW_TAGS))

  # cleanup
  fs::file_delete(new_yaml)
})

test_that("modify_spec_field() de-duplication works", {
  dupe_tags <- c("ha", "hey", "hey")
  uniq_tags <- c("ha", "hey")

  # make a new yaml
  new_yaml <- yaml_ext(NEW_MOD2)
  fs::file_copy(YAML_TEST_FILE, new_yaml)

  # make a spec from it
  new_spec <- create_model_from_yaml(new_yaml)
  expect_identical(new_spec[[YAML_TAGS]], ORIG_TAGS)

  # check that .unique = FALSE turns off de-duping
  new_spec <- modify_spec_field(new_spec, .field=YAML_TAGS, .value=dupe_tags, .unique = FALSE)
  expect_identical(new_spec[[YAML_TAGS]], c(ORIG_TAGS, dupe_tags))

  # check that .unique = TRUE (default) correctly de-dupes
  new_spec <- modify_spec_field(new_spec, .field=YAML_TAGS, .value=dupe_tags)
  expect_identical(new_spec[[YAML_TAGS]], c(ORIG_TAGS, uniq_tags))

  # cleanup
  fs::file_delete(new_yaml)
})

test_that("add_tags() and replace_tags() work correctly", {
  # make a new yaml
  new_yaml <- yaml_ext(NEW_MOD2)
  fs::file_copy(YAML_TEST_FILE, new_yaml)

  # make a spec from it
  new_spec <- create_model_from_yaml(new_yaml)
  expect_identical(new_spec[[YAML_TAGS]], ORIG_TAGS)

  # test adding
  new_spec <- add_tags(new_spec, NEW_TAGS)
  expect_identical(new_spec[[YAML_TAGS]], c(ORIG_TAGS, NEW_TAGS))

  # test_replacing
  new_spec <- replace_tags(new_spec, NEW_TAGS)
  expect_identical(new_spec[[YAML_TAGS]], NEW_TAGS)

  # cleanup
  fs::file_delete(new_yaml)
})

test_that("add_decisions() and replace_decisions() work correctly", {
  # make a new yaml
  new_yaml <- yaml_ext(NEW_MOD2)
  fs::file_copy(YAML_TEST_FILE, new_yaml)

  # make a spec from it
  new_spec <- create_model_from_yaml(new_yaml)
  expect_null(new_spec[[YAML_DECISIONS]])

  # test adding
  new_spec <- add_decisions(new_spec, NEW_TEXT1)
  expect_identical(new_spec[[YAML_DECISIONS]], NEW_TEXT1)
  new_spec <- add_decisions(new_spec, NEW_TEXT2)
  expect_identical(new_spec[[YAML_DECISIONS]], c(NEW_TEXT1, NEW_TEXT2))

  # test_replacing
  new_spec <- replace_decisions(new_spec, NEW_TEXT2)
  expect_identical(new_spec[[YAML_DECISIONS]], NEW_TEXT2)

  # cleanup
  fs::file_delete(new_yaml)
})

test_that("replace_description() works correctly", {
  # make a new yaml
  new_yaml <- yaml_ext(NEW_MOD2)
  fs::file_copy(YAML_TEST_FILE, new_yaml)

  # make a spec from it
  new_spec <- create_model_from_yaml(new_yaml)
  expect_identical(new_spec[[YAML_DESCRIPTION]], ORIG_DESC)

  # test_replacing
  new_spec <- replace_description(new_spec, NEW_DESC)
  expect_identical(new_spec[[YAML_DESCRIPTION]], NEW_DESC)

  # cleanup
  fs::file_delete(new_yaml)
})
