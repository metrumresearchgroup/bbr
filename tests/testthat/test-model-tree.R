context("Constructing model tree")

test_that("model_tree() default behavior", {
  skip_if_tree_missing_deps()
  clean_test_enviroment(create_tree_models)

  run_df <- run_log(MODEL_DIR)
  pl_tree <- model_tree(run_df)

  # Confirm number of expected nodes - doesnt seem to work
  # expect_equal(
  #   length(pl_tree$x$options$hierarchy) + 1,
  #   nrow(run_log(MODEL_DIR))
  # )

  # Confirm hierarchy - doesnt seem reliable
  # expect_equal(pl_tree$x$options$hierarchy, seq(1,7))

  # Confirm order of nodes (hierarchy doesnt track where child nodes are)
  # (requires dealing with a nested list)


  # withr::deferred_run()
})


test_that("make_tree_tooltip()", {
  skip_if_tree_missing_deps()
  clean_test_enviroment(create_tree_models)
  run_df <- run_log(MODEL_DIR)

})
