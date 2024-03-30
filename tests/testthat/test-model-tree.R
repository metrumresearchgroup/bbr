context("Model tree diagram")

count_nodes <- function(tree_list) {
  if(length(tree_list) == 0) return(0)
  # Iterate through each element in the list
  total_nodes <- 0
  for(i in seq_along(tree_list)){
    # Increment the count for the current node
    total_nodes <- total_nodes + 1
    # If the current node has children, recursively count nodes in children
    if(length(tree_list[[i]]$children) > 0){
      total_nodes <- total_nodes + count_nodes(tree_list[[i]]$children)
    }
  }
  return(total_nodes)
}

describe("model_tree() integration", {

  it("default behavior", {
    skip_if_tree_missing_deps()
    clean_test_enviroment(create_tree_models)

    run_df <- run_log(MODEL_DIR)
    pl_tree <- model_tree(run_df)

    # Confirm number of expected nodes
    expect_equal(count_nodes(pl_tree$x$data$children), nrow(run_df))
    # Confirm number of origins
    expect_equal(length(pl_tree$x$data$children), 1)
  })

  it("additional based on", {
    # Includes models that have multiple based_on attributes
    skip_if_tree_missing_deps()
    clean_test_enviroment(create_tree_models(addl_based_on = TRUE))
    run_df <- run_log(MODEL_DIR)
    pl_tree <- model_tree(run_df)

    # Confirm number of expected nodes
    expect_equal(count_nodes(pl_tree$x$data$children), nrow(run_df))
    # Confirm number of origins
    expect_equal(length(pl_tree$x$data$children), 1)

  })

  it("multiple origins", {
    # Multiple starting models (models without a `based_on` attribute)
    skip_if_tree_missing_deps()
    clean_test_enviroment(create_tree_models(multiple_origins = TRUE))
    run_df <- run_log(MODEL_DIR)
    pl_tree <- model_tree(run_df)

    # Confirm number of expected nodes
    expect_equal(count_nodes(pl_tree$x$data$children), nrow(run_df))
    # Confirm number of origins
    expect_equal(length(pl_tree$x$data$children), 2)
  })

  it("Broken links", {
    # A based_on referenced model no longer exists at the expected location
    skip_if_tree_missing_deps()
    clean_test_enviroment(create_tree_models(broken_link = TRUE))
    run_df <- run_log(MODEL_DIR)
    expect_warning(
      pl_tree <- model_tree(run_df),
      "The following models could not be linked properly"
    )

    # Confirm number of expected nodes
    # Here an extra node is made for mod 1000. mod 1000 is not present in the
    # run log (was deleted), but is referenced as based_on in mod 1001.
    expect_equal(count_nodes(pl_tree$x$data$children), nrow(run_df) + 1)
    # Confirm number of origins
    expect_equal(length(pl_tree$x$data$children), 2)
  })
})

describe("model_tree() data setup",{

  it("make_tree_data()", {

  })

  it("make_model_network()", {

  })
})

describe("model_tree() formatting",{

  it("make_tree_tooltip()", {
    skip_if_tree_missing_deps()
    clean_test_enviroment(create_tree_models(broken_link = TRUE))
    run_df <- run_log(MODEL_DIR)
    model_tree(run_df)
  })

  it("color_tree_by()", {

  })
})
