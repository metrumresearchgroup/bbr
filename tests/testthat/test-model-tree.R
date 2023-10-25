context("Constructing model tree")

test_that("model_tree() works", {
  skip_if_tree_missing_deps()
  clean_test_enviroment(create_tree_models)

  pl_tree <- model_tree(MODEL_DIR)

  withr::deferred_run()
})
