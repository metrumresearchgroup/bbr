context("Modify attributes of model object")

######################################
# modify_model_field and its wrappers
######################################

test_that("modify_model_field() works correctly", {
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

test_that("modify_model_field() de-duplication works", {
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

test_that("modify_model_field() errors with .append=T and .remove=T", {
  expect_error(
    modify_model_field(MOD1, YAML_TAGS, ORIG_TAGS, .append = TRUE, .remove = TRUE),
    regexp = "cannot have both"
  )
})

test_that("add_tags() and replace_all_tags() work correctly", {
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

test_that("replace_tag() works correctly", {
  temp_mod_path <- create_temp_model()

  # make a spec from it
  new_mod <- read_model(temp_mod_path)
  expect_identical(new_mod[[YAML_TAGS]], ORIG_TAGS)

  # test_replacing
  new_mod <- replace_tag(new_mod, ORIG_TAGS[1], NEW_TAGS[1])
  expect_identical(new_mod[[YAML_TAGS]], c(NEW_TAGS[1], ORIG_TAGS[2:length(ORIG_TAGS)]))
})


test_that("remove_tags() works correctly", {
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

test_that("add_notes() and replace_all_notes() work correctly", {
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

test_that("remove_notes() works correctly", {
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

test_that("add_decisions() and replace_decisions() work correctly", {
  temp_mod_path <- create_temp_model()

  # make a spec from it
  new_mod <- read_model(temp_mod_path)
  expect_null(new_mod[[YAML_DECISIONS]])

  # test adding
  new_mod <- expect_warning(
    add_decisions(new_mod, NEW_TEXT1),
    regexp = "has been replaced by"
  )
  expect_identical(new_mod[[YAML_DECISIONS]], NEW_TEXT1)

  new_mod <- expect_warning(
    add_decisions(new_mod, NEW_TEXT2),
    regexp = "has been replaced by"
  )
  expect_identical(new_mod[[YAML_DECISIONS]], c(NEW_TEXT1, NEW_TEXT2))

  # test_replacing
  new_mod <- expect_warning(
    replace_decisions(new_mod, NEW_TEXT2),
    regexp = "has been replaced by"
  )
  expect_identical(new_mod[[YAML_DECISIONS]], NEW_TEXT2)
})

test_that("replace_description() works correctly", {
  temp_mod_path <- create_temp_model()

  # make a spec from it
  new_mod <- read_model(temp_mod_path)
  expect_identical(new_mod[[YAML_DESCRIPTION]], ORIG_DESC)

  # test_replacing
  new_mod <- replace_description(new_mod, NEW_DESC)
  expect_identical(new_mod[[YAML_DESCRIPTION]], NEW_DESC)
})

test_that("replace_description() can use NULL", {
  temp_mod_path <- create_temp_model()

  # make a spec from it
  new_mod <- read_model(temp_mod_path)
  expect_identical(new_mod[[YAML_DESCRIPTION]], ORIG_DESC)

  # test_replacing
  new_mod <- replace_description(new_mod, NULL)
  expect_null(new_mod[[YAML_DESCRIPTION]])
})

test_that("replace_description() can use NA", {
  temp_mod_path <- create_temp_model()

  # make a spec from it
  new_mod <- read_model(temp_mod_path)
  expect_identical(new_mod[[YAML_DESCRIPTION]], ORIG_DESC)

  # test_replacing
  new_mod <- replace_description(new_mod, NA)
  expect_equal(new_mod[[YAML_DESCRIPTION]], NA)
})

test_that("add_bbi_args() and replace_all_bbi_args() work correctly", {
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


test_that("add_tags etc. can be chained", {
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

test_that("add_based_on() and replace_all_based_on() work correctly", {
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

test_that("reconcile_yaml() pulls in new tags", {
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


test_that("check_yaml_in_sync() passes when nothing has changed", {
  expect_invisible(check_yaml_in_sync(MOD1))
})

test_that("check_yaml_in_sync() fails when YAML has changed and passes after reconciled", {
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

test_that("add_tags fails if it wasn't re-assigned previously (testing check_yaml_in_sync)", {
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

test_that("submit_model() fails YAML out of sync (testing check_yaml_in_sync)", {
  temp_mod_path <- create_temp_model()
  temp_yaml <- fs::path_ext_set(temp_mod_path, "yaml")

  # make a spec from it
  new_mod <- read_model(temp_mod_path)

  # add some garbage (so YAML gets out of sync)
  readr::write_lines("naw: dawg", temp_yaml, append = TRUE)

  # try to submit_model and get error
  expect_error(submit_model(new_mod, .dry_run = T), regexp = "Model NOT in sync with corresponding YAML file")
})

test_that("model_summary() fails YAML out of sync (testing check_yaml_in_sync)", {
  temp_mod_path <- create_temp_model()
  temp_yaml <- fs::path_ext_set(temp_mod_path, "yaml")

  # make a spec from it
  new_mod <- read_model(temp_mod_path)

  # add some garbage (so YAML gets out of sync)
  readr::write_lines("naw: dawg", temp_yaml, append = TRUE)

  # try to submit_model and get error
  expect_error(model_summary(new_mod, .dry_run = T), regexp = "Model NOT in sync with corresponding YAML file")
})

test_that("copy_model_from() fails YAML out of sync (testing check_yaml_in_sync)", {
  temp_mod_path <- create_temp_model()
  temp_yaml <- fs::path_ext_set(temp_mod_path, "yaml")

  # make a spec from it
  new_mod <- read_model(temp_mod_path)

  # add some garbage (so YAML gets out of sync)
  readr::write_lines("naw: dawg", temp_yaml, append = TRUE)

  # try to submit_model and get error
  expect_error(copy_model_from(new_mod, "foo"), regexp = "Model NOT in sync with corresponding YAML file")
})
