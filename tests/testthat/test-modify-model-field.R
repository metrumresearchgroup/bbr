context("Modify attributes of model object")

source("data/test-workflow-ref.R")

withr::with_options(list(rbabylon.model_directory = NULL), {

  ######################################
  # modify_model_field and its wrappers
  ######################################

  test_that("modify_model_field() works correctly", {
    on.exit({ cleanup() })

    # make a new yaml
    new_yaml <- yaml_ext(NEW_MOD2)
    fs::file_copy(YAML_TEST_FILE, new_yaml)

    # make a spec from it
    suppressSpecificWarning({
      new_mod <- read_model(new_yaml)
    }, .regexpr = "No model file found at.+\\.ctl")
    expect_identical(new_mod[[YAML_TAGS]], ORIG_TAGS)

    # modify the tags field
    suppressSpecificWarning({
      new_mod <- modify_model_field(new_mod, .field=YAML_TAGS, .value=NEW_TAGS)
    }, .regexpr = "No model file found at.+\\.ctl")
    expect_identical(new_mod[[YAML_TAGS]], c(ORIG_TAGS, NEW_TAGS))

    # check that the yaml was also modified
    rogue_spec <- yaml::read_yaml(new_yaml)
    expect_identical(rogue_spec[[YAML_TAGS]], c(ORIG_TAGS, NEW_TAGS))
  })

  test_that("modify_model_field() de-duplication works", {
    on.exit({ cleanup() })

    dupe_tags <- c("ha", "hey", "ha")
    uniq_tags <- c("ha", "hey")

    # make a new yaml
    new_yaml <- yaml_ext(NEW_MOD2)
    fs::file_copy(YAML_TEST_FILE, new_yaml)

    # make a spec from it
    suppressSpecificWarning({
      new_mod <- read_model(new_yaml)
    }, .regexpr = "No model file found at.+\\.ctl")
    expect_identical(new_mod[[YAML_TAGS]], ORIG_TAGS)

    # check that .unique = FALSE turns off de-duping
    suppressSpecificWarning({
      new_mod <- modify_model_field(new_mod, .field=YAML_TAGS, .value=dupe_tags, .unique = FALSE)
    }, .regexpr = "No model file found at.+\\.ctl")
    expect_identical(new_mod[[YAML_TAGS]], c(ORIG_TAGS, dupe_tags))

    # check that .unique = TRUE (default) correctly de-dupes
    new_mod <- modify_model_field(new_mod, .field=YAML_TAGS, .value=dupe_tags)
    expect_identical(new_mod[[YAML_TAGS]], c(ORIG_TAGS, uniq_tags))
  })

  test_that("add_tags() and replace_tags() work correctly", {
    on.exit({ cleanup() })

    # make a new yaml
    new_yaml <- yaml_ext(NEW_MOD2)
    fs::file_copy(YAML_TEST_FILE, new_yaml)

    # make a spec from it
    suppressSpecificWarning({
      new_mod <- read_model(new_yaml)
    }, .regexpr = "No model file found at.+\\.ctl")
    expect_identical(new_mod[[YAML_TAGS]], ORIG_TAGS)

    # test adding
    suppressSpecificWarning({
      new_mod <- add_tags(new_mod, NEW_TAGS)
    }, .regexpr = "No model file found at.+\\.ctl")
    expect_identical(new_mod[[YAML_TAGS]], c(ORIG_TAGS, NEW_TAGS))

    # test_replacing
    new_mod <- replace_tags(new_mod, NEW_TAGS)
    expect_identical(new_mod[[YAML_TAGS]], NEW_TAGS)
  })

  test_that("add_decisions() and replace_decisions() work correctly", {
    on.exit({ cleanup() })

    # make a new yaml
    new_yaml <- yaml_ext(NEW_MOD2)
    fs::file_copy(YAML_TEST_FILE, new_yaml)

    # make a spec from it
    suppressSpecificWarning({
      new_mod <- read_model(new_yaml)
    }, .regexpr = "No model file found at.+\\.ctl")
    expect_null(new_mod[[YAML_DECISIONS]])

    # test adding
    suppressSpecificWarning({
      new_mod <- add_decisions(new_mod, NEW_TEXT1)
    }, .regexpr = "No model file found at.+\\.ctl")
    expect_identical(new_mod[[YAML_DECISIONS]], NEW_TEXT1)
    new_mod <- add_decisions(new_mod, NEW_TEXT2)
    expect_identical(new_mod[[YAML_DECISIONS]], c(NEW_TEXT1, NEW_TEXT2))

    # test_replacing
    new_mod <- replace_decisions(new_mod, NEW_TEXT2)
    expect_identical(new_mod[[YAML_DECISIONS]], NEW_TEXT2)
  })

  test_that("replace_description() works correctly", {
    on.exit({ cleanup() })

    # make a new yaml
    new_yaml <- yaml_ext(NEW_MOD2)
    fs::file_copy(YAML_TEST_FILE, new_yaml)

    # make a spec from it
    suppressSpecificWarning({
      new_mod <- read_model(new_yaml)
    }, .regexpr = "No model file found at.+\\.ctl")
    expect_identical(new_mod[[YAML_DESCRIPTION]], ORIG_DESC)

    # test_replacing
    suppressSpecificWarning({
      new_mod <- replace_description(new_mod, NEW_DESC)
    }, .regexpr = "No model file found at.+\\.ctl")
    expect_identical(new_mod[[YAML_DESCRIPTION]], NEW_DESC)
  })


  test_that("add_bbi_args() and replace_bbi_args() work correctly", {
    on.exit({ cleanup() })

    # make a new yaml
    new_yaml <- yaml_ext(NEW_MOD2)
    fs::file_copy(YAML_TEST_FILE, new_yaml)

    # make a spec from it
    suppressSpecificWarning({
      new_mod <- read_model(new_yaml)
    }, .regexpr = "No model file found at.+\\.ctl")
    expect_null(new_mod[[YAML_BBI_ARGS]][["clean_lvl"]])

    # test adding
    suppressSpecificWarning({
      new_mod <- add_bbi_args(new_mod, list(clean_lvl = 1))
    }, .regexpr = "No model file found at.+\\.ctl")
    expect_identical(new_mod[[YAML_BBI_ARGS]][["threads"]], 4)
    expect_identical(new_mod[[YAML_BBI_ARGS]][["clean_lvl"]], 1)

    # test_replacing
    new_mod <- replace_bbi_args(new_mod, list(clean_lvl = 1))
    expect_null(new_mod[[YAML_BBI_ARGS]][["threads"]])
    expect_identical(new_mod[[YAML_BBI_ARGS]][["clean_lvl"]], 1)
  })


  test_that("add_tags etc. can be chained", {
    on.exit({ cleanup() })

    # make a new yaml
    new_yaml <- yaml_ext(NEW_MOD2)
    fs::file_copy(YAML_TEST_FILE, new_yaml)

    # make a spec from it
    suppressSpecificWarning({
      new_mod <- read_model(new_yaml)
    }, .regexpr = "No model file found at.+\\.ctl")
    expect_identical(new_mod[[YAML_DESCRIPTION]], ORIG_DESC)

    # test adding and replacing
    suppressSpecificWarning({
      new_mod <- new_mod %>%
        add_tags(NEW_TAGS) %>%
        add_decisions(NEW_TEXT1) %>%
        replace_description(NEW_DESC)
    }, .regexpr = "No model file found at.+\\.ctl")

    expect_identical(new_mod[[YAML_TAGS]], c(ORIG_TAGS, NEW_TAGS))
    expect_identical(new_mod[[YAML_DECISIONS]], NEW_TEXT1)
    expect_identical(new_mod[[YAML_DESCRIPTION]], NEW_DESC)
  })

  test_that("add_based_on() and replace_based_on() work correctly", {
    on.exit({ cleanup() })

    # make a new yaml
    new_yaml <- yaml_ext(NEW_MOD2)
    #new_yaml <- paste0(NEW_MOD2, '.yml') # change back to this once I fix yaml_ext i.e. line 86 of new-model.R
    fs::file_copy(YAML_TEST_FILE, new_yaml)

    # make a spec from it
    suppressSpecificWarning({
      new_mod <- read_model(new_yaml)
    }, .regexpr = "No model file found at.+\\.ctl")
    expect_null(new_mod[[YAML_BASED_ON]])

    # test adding
    new_mod <- add_based_on(new_mod, "1")
    expect_identical(new_mod[[YAML_BASED_ON]], "1")

    # add itself to check adding an absolute path with get_yaml_path
    new_mod <- add_based_on(new_mod, get_yaml_path(new_mod))
    expect_identical(new_mod[[YAML_BASED_ON]], c("1", "2"))

    # test_replacing
    new_mod <- replace_based_on(new_mod, "2")
    expect_identical(new_mod[[YAML_BASED_ON]], "2")
  })


  ####################################
  # helper functions for model fields
  ####################################

  test_that("safe_based_on works on happy path", {
    on.exit({ cleanup() })
    # test on 1.yaml
    expect_equal(safe_based_on(.start = ".", .based_on = YAML_TEST_FILE), tools::file_path_sans_ext(YAML_TEST_FILE))
    expect_equal(safe_based_on(.start = MODEL_DIR, .based_on = "1.yaml"), "1")
  })

  test_that("safe_based_on works on happy path with vector", {
    on.exit({ cleanup() })
    fs::file_copy(YAML_TEST_FILE, paste0(NEW_MOD2, '.yml'))
    expect_equal(safe_based_on(.start = ".", .based_on = c(YAML_TEST_FILE, NEW_MOD2)), c(tools::file_path_sans_ext(YAML_TEST_FILE), NEW_MOD2))
    expect_equal(safe_based_on(.start = MODEL_DIR, .based_on = c("1", "2")), c("1", "2"))
  })

  test_that("safe_based_on works on nested dirs", {
    on.exit({ cleanup() })

    # copy model 1 to level deeper
    fs::dir_create(LEVEL2_DIR)
    fs::file_copy(YAML_TEST_FILE, LEVEL2_DIR)
    fs::file_copy(YAML_TEST_FILE, paste0(NEW_MOD2, '.yml'))

    expect_equal(
      safe_based_on(.start = ".", .based_on = c(YAML_TEST_FILE, NEW_MOD2, file.path(LEVEL2_DIR, "1"))),
      c(tools::file_path_sans_ext(YAML_TEST_FILE), NEW_MOD2, file.path(LEVEL2_DIR, "1"))
    )

    expect_equal(safe_based_on(.start = MODEL_DIR, .based_on = c("1", "2", "level2/1")), c("1", "2", "level2/1"))
  })

  test_that("safe_based_on works with absolute path" , {
    on.exit({ cleanup() })

    # copy model 1 to level deeper
    create_all_models()

    new_model_path <- get_model_path(mod2)
    expect_true(fs::is_absolute_path(new_model_path))

    expect_identical(safe_based_on(mod2[[WORKING_DIR]], c("1", new_model_path)), c("1", "2"))
    expect_identical(safe_based_on(mod4[[WORKING_DIR]], c("1", new_model_path)), c("1", "../2"))
  })

  test_that("safe_based_on fails when yaml can't be found", {
    expect_error(safe_based_on(.start = ".", .based_on = c(YAML_TEST_FILE, NEW_MOD2)), regexp = "cannot find .yaml or .yml files")
    expect_error(safe_based_on(.start = MODEL_DIR, .based_on = c("1", "2")), regexp = "cannot find .yaml or .yml files")
  })


  ####################################################
  # check_yaml_in_sync gets triggered when it should
  ####################################################

  test_that("reconcile_yaml() pulls in new tags", {
    on.exit({ cleanup() })

    # make a new yaml
    new_yaml <- yaml_ext(NEW_MOD2)
    fs::file_copy(YAML_TEST_FILE, new_yaml)

    # make a spec from it
    suppressSpecificWarning({
      new_mod <- read_model(new_yaml)
    }, .regexpr = "No model file found at.+\\.ctl")
    expect_identical(new_mod[[YAML_TAGS]], ORIG_TAGS)

    # add the tags to the yaml manually
    rogue_spec <- yaml::read_yaml(new_yaml)
    rogue_spec[[YAML_TAGS]] <- c(rogue_spec[[YAML_TAGS]], NEW_TAGS)
    rogue_spec[[YAML_YAML_NAME]] <- new_yaml
    yaml::write_yaml(rogue_spec, new_yaml)

    # check that reconcile adds the new tags
    suppressSpecificWarning({
      new_mod <- reconcile_yaml(new_mod)
    }, .regexpr = "No model file found at.+\\.ctl")
    expect_identical(new_mod[[YAML_TAGS]], c(ORIG_TAGS, NEW_TAGS))
  })


  test_that("check_yaml_in_sync() passes when nothing has changed", {
    new_mod <- read_model(YAML_TEST_FILE)
    expect_invisible(check_yaml_in_sync(new_mod))
  })

  test_that("check_yaml_in_sync() fails when YAML has changed and passes after reconciled", {
    on.exit({ cleanup() })

    # make a new yaml
    new_yaml <- yaml_ext(NEW_MOD2)
    fs::file_copy(YAML_TEST_FILE, new_yaml)

    # make a spec from it
    suppressSpecificWarning({
      new_mod <- read_model(new_yaml)
    }, .regexpr = "No model file found at.+\\.ctl")
    expect_identical(new_mod[[YAML_TAGS]], ORIG_TAGS)

    # add the tags to the yaml manually
    rogue_spec <- yaml::read_yaml(new_yaml)
    rogue_spec[[YAML_TAGS]] <- c(rogue_spec[[YAML_TAGS]], NEW_TAGS)
    rogue_spec[[YAML_YAML_NAME]] <- new_yaml
    yaml::write_yaml(rogue_spec, new_yaml)

    # check against the YAML and expect error
    expect_error(check_yaml_in_sync(new_mod), .regexpr = "Model NOT in sync with corresponding YAML file")

    # reconcile and check again
    suppressSpecificWarning({
      new_mod <- reconcile_yaml(new_mod)
    }, .regexpr = "No model file found at.+\\.ctl")
    check_yaml_in_sync(new_mod) # this should pass this time
    expect_identical(new_mod[[YAML_TAGS]], c(ORIG_TAGS, NEW_TAGS))
  })

  test_that("add_tags fails if it wasn't re-assigned previously (testing check_yaml_in_sync)", {
    on.exit({ cleanup() })

    # make a new yaml
    new_yaml <- yaml_ext(NEW_MOD2)
    fs::file_copy(YAML_TEST_FILE, new_yaml)

    # make a spec from it
    suppressSpecificWarning({
      new_mod <- read_model(new_yaml)
    }, .regexpr = "No model file found at.+\\.ctl")

    # test adding with assignment
    suppressSpecificWarning({
      new_mod <- new_mod %>% add_tags(NEW_TAGS)
    }, .regexpr = "No model file found at.+\\.ctl")
    expect_identical(new_mod[[YAML_TAGS]], c(ORIG_TAGS, NEW_TAGS))

    # add without assignment (so YAML gets out of sync)
    new_mod %>% add_decisions(NEW_TEXT1)
    expect_null(new_mod[[YAML_DECISIONS]])

    # try to add more and get error
    expect_error(new_mod %>% replace_description(NEW_DESC), regexp = "Model NOT in sync with corresponding YAML file")
  })

  test_that("submit_model() fails YAML out of sync (testing check_yaml_in_sync)", {
    on.exit({ cleanup() })

    # make a new yaml
    new_yaml <- yaml_ext(NEW_MOD2)
    fs::file_copy(YAML_TEST_FILE, new_yaml)

    # make a spec from it
    suppressSpecificWarning({
      new_mod <- read_model(new_yaml)
    }, .regexpr = "No model file found at.+\\.ctl")

    # add some garbage (so YAML gets out of sync)
    readr::write_lines("naw: dawg", new_yaml, append = TRUE)

    # try to submit_model and get error
    expect_error(submit_model(new_mod, .dry_run = T), regexp = "Model NOT in sync with corresponding YAML file")
  })

  test_that("model_summary() fails YAML out of sync (testing check_yaml_in_sync)", {
    on.exit({ cleanup() })

    # make a new yaml
    new_yaml <- yaml_ext(NEW_MOD2)
    fs::file_copy(YAML_TEST_FILE, new_yaml)

    # make a spec from it
    suppressSpecificWarning({
      new_mod <- read_model(new_yaml)
    }, .regexpr = "No model file found at.+\\.ctl")

    # add some garbage (so YAML gets out of sync)
    readr::write_lines("naw: dawg", new_yaml, append = TRUE)

    # try to submit_model and get error
    expect_error(model_summary(new_mod, .dry_run = T), regexp = "Model NOT in sync with corresponding YAML file")
  })

  test_that("copy_model_from() fails YAML out of sync (testing check_yaml_in_sync)", {
    on.exit({ cleanup() })

    # make a new yaml
    new_yaml <- yaml_ext(NEW_MOD2)
    fs::file_copy(YAML_TEST_FILE, new_yaml)

    # make a spec from it
    suppressSpecificWarning({
      new_mod <- read_model(new_yaml)
    }, .regexpr = "No model file found at.+\\.ctl")

    # add some garbage (so YAML gets out of sync)
    readr::write_lines("naw: dawg", new_yaml, append = TRUE)

    # try to submit_model and get error
    expect_error(copy_model_from(new_mod, "naw", "dawg"), regexp = "Model NOT in sync with corresponding YAML file")
  })


}) # closing withr::with_options
