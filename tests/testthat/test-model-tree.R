context("Model tree diagram")
skip_if_tree_missing_deps()

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
    clean_test_enviroment(create_tree_models)
    # With summary (default)
    tree_data <- make_tree_data(run_log(MODEL_DIR), add_summary = TRUE)
    tree_data <- make_tree_tooltip(tree_data)
    # Starting node
    expect_true(grepl(MODEL_DIR, tree_data$tooltip[1]))
    # Spot check some rendered tooltips
    expect_true(grepl(paste0(MOD1$tags, collapse = ", "), tree_data$tooltip[2]))
    expect_true(grepl(MOD1$description, tree_data$tooltip[2]))
    # only MOD1 can be summarized (second node)
    expect_equal(grep("OFV", tree_data$tooltip), 2)
    expect_equal(grep("--Heuristics Found--", tree_data$tooltip), 2)
    # Without summary
    tree_data <- make_tree_data(run_log(MODEL_DIR), add_summary = FALSE)
    tree_data <- make_tree_tooltip(tree_data)
    expect_false(any(grepl("OFV", tree_data$tooltip)))
  })

  it("color_tree_by()", {
    clean_test_enviroment(create_tree_models)
    pl_tree <- model_tree(MODEL_DIR)
    expect_equal(pl_tree$x$options$attribute, "run")
    pl_tree <- model_tree(MODEL_DIR, color_by = "star")
    expect_equal(pl_tree$x$options$attribute, "star")

    # Check generated color column
    tree_data <- make_tree_data(run_log(MODEL_DIR))
    tree_data <- color_tree_by(tree_data, color_by = "star")
    expect_equal(
      as.numeric(as.factor(tree_data$star)),
      as.numeric(as.factor(tree_data$col))
    )
  })
})
