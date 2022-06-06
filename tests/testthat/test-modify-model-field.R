context("Modify attributes of model object")

######################################
# modify_model_field and its wrappers
######################################

test_that("modify_model_field() works correctly [BBR-MMF-001]", {
  temp_mod_path <- create_temp_model()

  # make a spec from it
  new_mod <- read_model(temp_mod_path)
  expect_identical(new_mod[[YAML_TAGS]], ORIG_TAGS)

  # modify the tags field
  new_mod <- modify_model_field(new_mod, .field = YAML_TAGS, .value = NEW_TAGS)
  expect_identical(new_mod[[YAML_TAGS]], c(ORIG_TAGS, NEW_TAGS))

  # check that the yaml was also modified
  rogue_spec <- yaml::read_yaml(fs::path_ext_set(temp_mod_path, "yaml"))
  expect_identical(rogue_spec[[YAML_TAGS]], c(ORIG_TAGS, NEW_TAGS))
})

test_that("modify_model_field() de-duplication works [BBR-MMF-002]", {
  dupe_tags <- c("ha", "hey", "ha")
  uniq_tags <- c("ha", "hey")

  temp_mod_path <- create_temp_model()

  new_mod <- read_model(temp_mod_path)
  expect_identical(new_mod[[YAML_TAGS]], ORIG_TAGS)

  # check that .unique = FALSE turns off de-duping
  new_mod <- modify_model_field(new_mod, .field = YAML_TAGS, .value = dupe_tags, .unique = FALSE)
  expect_identical(new_mod[[YAML_TAGS]], c(ORIG_TAGS, dupe_tags))

  # check that .unique = TRUE (default) correctly de-dupes
  new_mod <- modify_model_field(new_mod, .field=YAML_TAGS, .value=dupe_tags)
  expect_identical(new_mod[[YAML_TAGS]], c(ORIG_TAGS, uniq_tags))
})

test_that("modify_model_field() errors with .append=T and .remove=T [BBR-MMF-003]", {
  expect_error(
    modify_model_field(MOD1, YAML_TAGS, ORIG_TAGS, .append = TRUE, .remove = TRUE),
    regexp = "cannot have both"
  )
})

test_that("add_tags() and replace_all_tags() work correctly [BBR-MMF-004]", {
  temp_mod_path <- create_temp_model()

  # make a spec from it
  new_mod <- read_model(temp_mod_path)
  expect_identical(new_mod[[YAML_TAGS]], ORIG_TAGS)

  # test adding
  new_mod <- add_tags(new_mod, NEW_TAGS)
  expect_identical(new_mod[[YAML_TAGS]], c(ORIG_TAGS, NEW_TAGS))

  # test_replacing
  new_mod <- replace_all_tags(new_mod, NEW_TAGS)
  expect_identical(new_mod[[YAML_TAGS]], NEW_TAGS)
})

test_that("replace_model_field() works correctly [BBR-MMF-005]", {
  temp_mod_path <- create_temp_model()

  # make a spec from it
  new_mod <- read_model(temp_mod_path)
  expect_identical(new_mod[[YAML_TAGS]], ORIG_TAGS)

  # test_replacing
  new_mod <- replace_model_field(new_mod, YAML_TAGS, ORIG_TAGS[1], NEW_TAGS[1])
  expect_identical(new_mod[[YAML_TAGS]], c(NEW_TAGS[1], ORIG_TAGS[2:length(ORIG_TAGS)]))

  # check that YAML is modified
  mod_yaml <- new_mod %>% get_yaml_path() %>% yaml::read_yaml()
  expect_identical(new_mod[[YAML_TAGS]], mod_yaml[[YAML_TAGS]])
})


test_that("add_notes() and replace_all_notes() work correctly [BBR-MMF-006]", {
  temp_mod_path <- create_temp_model()
  new_mod <- read_model(temp_mod_path)
  expect_null(new_mod[[YAML_NOTES]])

  # test adding
  new_mod <- add_notes(new_mod, c(NEW_NOTES, EXTRA_NOTE))
  expect_identical(new_mod[[YAML_NOTES]], c(NEW_NOTES, EXTRA_NOTE))

  # test_replacing
  new_mod <- replace_all_notes(new_mod, NEW_NOTES)
  expect_identical(new_mod[[YAML_NOTES]], NEW_NOTES)
})

test_that("remove_notes() works correctly [BBR-MMF-007]", {
  temp_mod_path <- create_temp_model()

  # make a model object and replace the notes
  new_mod <- read_model(temp_mod_path)
  new_mod <- replace_all_notes(new_mod, c(NEW_NOTES, EXTRA_NOTE))
  expect_identical(new_mod[[YAML_NOTES]], c(NEW_NOTES, EXTRA_NOTE))

  # test removing
  new_mod <- expect_warning(
    remove_notes(new_mod, c(EXTRA_NOTE, "bad note")),
    regexp = "does not contain any of the following.+bad"
  )
  expect_identical(new_mod[[YAML_NOTES]], NEW_NOTES)
})

test_that("replace_description() works correctly [BBR-MMF-008]", {
  temp_mod_path <- create_temp_model()

  # make a spec from it
  new_mod <- read_model(temp_mod_path)
  expect_identical(new_mod[[YAML_DESCRIPTION]], ORIG_DESC)

  # test_replacing
  new_mod <- replace_description(new_mod, NEW_DESC)
  expect_identical(new_mod[[YAML_DESCRIPTION]], NEW_DESC)
})

test_that("replace_description() can use NULL [BBR-MMF-009]", {
  temp_mod_path <- create_temp_model()

  # make a spec from it
  new_mod <- read_model(temp_mod_path)
  expect_identical(new_mod[[YAML_DESCRIPTION]], ORIG_DESC)

  # test_replacing
  new_mod <- replace_description(new_mod, NULL)
  expect_null(new_mod[[YAML_DESCRIPTION]])
})

test_that("replace_description() can use NA [BBR-MMF-010]", {
  temp_mod_path <- create_temp_model()

  # make a spec from it
  new_mod <- read_model(temp_mod_path)
  expect_identical(new_mod[[YAML_DESCRIPTION]], ORIG_DESC)

  # test_replacing
  new_mod <- replace_description(new_mod, NA)
  expect_null(new_mod[[YAML_DESCRIPTION]])
})

test_that("add_bbi_args() and replace_all_bbi_args() work correctly [BBR-MMF-011]", {
  temp_mod_path <- create_temp_model()

  # make a spec from it
  new_mod <- read_model(temp_mod_path)
  expect_null(new_mod[[YAML_BBI_ARGS]][["clean_lvl"]])

  # test adding
  new_mod <- add_bbi_args(new_mod, list(clean_lvl = 1))
  expect_identical(new_mod[[YAML_BBI_ARGS]][["threads"]], 4)
  expect_identical(new_mod[[YAML_BBI_ARGS]][["clean_lvl"]], 1)

  # test_replacing
  new_mod <- replace_all_bbi_args(new_mod, list(clean_lvl = 1))
  expect_null(new_mod[[YAML_BBI_ARGS]][["threads"]])
  expect_identical(new_mod[[YAML_BBI_ARGS]][["clean_lvl"]], 1)
})


test_that("add_tags etc. can be chained [BBR-MMF-012]", {
  temp_mod_path <- create_temp_model()

  # make a spec from it
  new_mod <- read_model(temp_mod_path)
  expect_identical(new_mod[[YAML_DESCRIPTION]], ORIG_DESC)

  # test adding and replacing
  new_mod <- new_mod %>%
    add_tags(NEW_TAGS) %>%
    add_notes(NEW_NOTES) %>%
    replace_description(NEW_DESC)

  expect_identical(new_mod[[YAML_TAGS]], c(ORIG_TAGS, NEW_TAGS))
  expect_identical(new_mod[[YAML_NOTES]], NEW_NOTES)
  expect_identical(new_mod[[YAML_DESCRIPTION]], NEW_DESC)
})

test_that("add_based_on() and replace_all_based_on() work correctly [BBR-MMF-013]", {
  temp_mod_path <- create_temp_model()
  parent_model_id <- get_model_id(create_temp_model())
  child_model_id <- get_model_id(temp_mod_path)

  # make a spec from it
  new_mod <- read_model(temp_mod_path)
  expect_null(new_mod[[YAML_BASED_ON]])

  # test adding
  new_mod <- add_based_on(new_mod, parent_model_id)
  expect_identical(new_mod[[YAML_BASED_ON]], parent_model_id)

  # add itself to check adding an absolute path with get_yaml_path
  new_mod <- add_based_on(new_mod, get_yaml_path(new_mod))
  expect_identical(
    new_mod[[YAML_BASED_ON]],
    c(parent_model_id, child_model_id)
  )

  # test_replacing
  new_mod <- replace_all_based_on(new_mod, child_model_id)
  expect_identical(new_mod[[YAML_BASED_ON]], child_model_id)
})


####################################################
# check_yaml_in_sync gets triggered when it should
####################################################

test_that("reconcile_yaml() pulls in new tags [BBR-MMF-014]", {
  temp_mod_path <- create_temp_model()
  temp_yaml <- fs::path_ext_set(temp_mod_path, "yaml")

  # make a spec from it
  new_mod <- read_model(temp_mod_path)
  expect_identical(new_mod[[YAML_TAGS]], ORIG_TAGS)

  # add the tags to the yaml manually
  rogue_spec <- yaml::read_yaml(temp_yaml)
  rogue_spec[[YAML_TAGS]] <- c(rogue_spec[[YAML_TAGS]], NEW_TAGS)
  yaml::write_yaml(rogue_spec, temp_yaml)

  # check that reconcile adds the new tags
  new_mod <- reconcile_yaml(new_mod)
  expect_identical(new_mod[[YAML_TAGS]], c(ORIG_TAGS, NEW_TAGS))
})


test_that("check_yaml_in_sync() passes when nothing has changed [BBR-MMF-015]", {
  expect_invisible(check_yaml_in_sync(MOD1))
})

test_that("check_yaml_in_sync() fails when YAML has changed and passes after reconciled [BBR-MMF-016]", {
  temp_mod_path <- create_temp_model()
  temp_yaml <- fs::path_ext_set(temp_mod_path, "yaml")

  # make a spec from it
  new_mod <- read_model(temp_mod_path)
  expect_identical(new_mod[[YAML_TAGS]], ORIG_TAGS)

  # add the tags to the yaml manually
  rogue_spec <- yaml::read_yaml(temp_yaml)
  rogue_spec[[YAML_TAGS]] <- c(rogue_spec[[YAML_TAGS]], NEW_TAGS)
  yaml::write_yaml(rogue_spec, temp_yaml)

  # check against the YAML and expect error
  expect_error(check_yaml_in_sync(new_mod), .regexpr = "Model NOT in sync with corresponding YAML file")

  # reconcile and check again
  new_mod <- reconcile_yaml(new_mod)
  check_yaml_in_sync(new_mod) # this should pass this time
  expect_identical(new_mod[[YAML_TAGS]], c(ORIG_TAGS, NEW_TAGS))
})

test_that("add_tags fails if it wasn't re-assigned previously (testing check_yaml_in_sync) [BBR-MMF-017]", {
  temp_mod_path <- create_temp_model()

  # make a spec from it
  new_mod <- read_model(temp_mod_path)

  # test adding with assignment
  new_mod <- new_mod %>% add_tags(NEW_TAGS)
  expect_identical(new_mod[[YAML_TAGS]], c(ORIG_TAGS, NEW_TAGS))

  # add without assignment (so YAML gets out of sync)
  new_mod %>% add_notes(NEW_NOTES)
  expect_null(new_mod[[YAML_NOTES]])

  # try to add more and get error
  expect_error(new_mod %>% replace_description(NEW_DESC), regexp = "Model NOT in sync with corresponding YAML file")
})

test_that("submit_model() fails YAML out of sync (testing check_yaml_in_sync) [BBR-MMF-018]", {
  temp_mod_path <- create_temp_model()
  temp_yaml <- fs::path_ext_set(temp_mod_path, "yaml")

  # make a spec from it
  new_mod <- read_model(temp_mod_path)

  # add some garbage (so YAML gets out of sync)
  readr::write_lines("naw: dawg", temp_yaml, append = TRUE)

  # try to submit_model and get error
  expect_error(submit_model(new_mod, .dry_run = T), regexp = "Model NOT in sync with corresponding YAML file")
})

test_that("model_summary() fails YAML out of sync (testing check_yaml_in_sync) [BBR-MMF-019]", {
  temp_mod_path <- create_temp_model()
  temp_yaml <- fs::path_ext_set(temp_mod_path, "yaml")

  # make a spec from it
  new_mod <- read_model(temp_mod_path)

  # add some garbage (so YAML gets out of sync)
  readr::write_lines("naw: dawg", temp_yaml, append = TRUE)

  # try to submit_model and get error
  expect_error(model_summary(new_mod, .dry_run = T), regexp = "Model NOT in sync with corresponding YAML file")
})

test_that("copy_model_from() fails YAML out of sync (testing check_yaml_in_sync) [BBR-MMF-020]", {
  temp_mod_path <- create_temp_model()
  temp_yaml <- fs::path_ext_set(temp_mod_path, "yaml")

  # make a spec from it
  new_mod <- read_model(temp_mod_path)

  # add some garbage (so YAML gets out of sync)
  readr::write_lines("naw: dawg", temp_yaml, append = TRUE)

  # try to submit_model and get error
  expect_error(copy_model_from(new_mod, "foo"), regexp = "Model NOT in sync with corresponding YAML file")
})

test_that("add_tags(), add_notes() and friends check for character vector [BBR-MMF-021]", {
  temp_mod_path <- create_temp_model()
  new_mod <- read_model(temp_mod_path)

  # Accept a list value if it can be converted to a character vector.
  new_mod <- new_mod %>%
    add_tags(as.list(NEW_TAGS)) %>%
    add_notes(as.list(NEW_NOTES))
  expect_identical(new_mod[[YAML_TAGS]], c(ORIG_TAGS, NEW_TAGS))
  expect_identical(new_mod[[YAML_NOTES]], c(NEW_NOTES))

  # Otherwise passing a type other than a character vector leads to an error.
  fns <- c(add_tags, replace_all_tags, remove_tags,
           add_notes, replace_all_notes, remove_notes)
  for (fn in fns){
    expect_error(new_mod %>% fn(1:3))
    expect_error(new_mod %>% fn(list(1:3)))
  }
})

test_that("remove_tags() works correctly [BBR-MMF-022]", {
  temp_mod_path <- create_temp_model()
  test_tags <- c("one", "two", "three", "four", "five")
  rem_tags <- c("two", "four", "bad")
  ref_tags <- c("one", "three", "five")

  # make a model object and replace the tags
  new_mod <- read_model(temp_mod_path)
  new_mod <- replace_all_tags(new_mod, test_tags)
  expect_identical(new_mod[[YAML_TAGS]], test_tags)

  # test removing
  new_mod <- expect_warning(
    remove_tags(new_mod, rem_tags),
    regexp = "does not contain any of the following.+bad"
  )
  expect_identical(new_mod[[YAML_TAGS]], ref_tags)
})

test_that("Checking that add_star is adding star to model object [BBR-MMF-023]", {
  temp_mod_path <- create_temp_model()

  # make a model object and check YAML
  new_mod <- read_model(temp_mod_path)

  expect_null(new_mod$star)
  new_mod <- add_star(new_mod)

  expect_true(new_mod$star)


})

test_that("Checking that remove_star is removing star from model object [BBR-MMF-024]", {
  temp_mod_path <- create_temp_model()

  # make a model object and check YAML
  new_mod <- read_model(temp_mod_path)

  expect_null(new_mod$star)
  new_mod <- remove_star(new_mod)
  expect_null(new_mod$star)


})
