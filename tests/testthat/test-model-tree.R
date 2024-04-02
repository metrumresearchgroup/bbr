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
    tree_data <- make_tree_data(run_df)

    # Confirm one origin in data
    expect_equal(sum(grepl("Start", tree_data$from)), 1)

    pl_tree <- model_tree(run_df)
    # Confirm number of expected nodes
    expect_equal(count_nodes(pl_tree$x$data$children), nrow(run_df))
    # Confirm number of origins plotted
    expect_equal(length(pl_tree$x$data$children), 1)
  })

  it("additional based on", {
    # Includes models that have multiple based_on attributes
    clean_test_enviroment(create_tree_models(addl_based_on = TRUE))
    run_df <- run_log(MODEL_DIR)
    tree_data <- make_tree_data(run_df)
    addl_based_on <- tree_data$addl_based_on

    # Additional based on attributes are stored as a separate column and
    # included in the tooltip. The first one found is used to create the link
    expect_equal(addl_based_on[!is.na(addl_based_on)], c("1, 3", "2"))
    expect_equal(sum(is.na(addl_based_on)), 6)

    pl_tree <- model_tree(run_df)
    # Confirm number of expected nodes
    expect_equal(count_nodes(pl_tree$x$data$children), nrow(run_df))
    # Confirm number of origins plotted
    expect_equal(length(pl_tree$x$data$children), 1)
  })

  it("multiple origins", {
    # Multiple starting models (models without a `based_on` attribute)
    clean_test_enviroment(create_tree_models(multiple_origins = TRUE))
    run_df <- run_log(MODEL_DIR)
    tree_data <- make_tree_data(run_df)

    # Confirm two origins in data
    expect_equal(sum(grepl("Start", tree_data$from)), 2)

    pl_tree <- model_tree(run_df)
    # Confirm number of expected nodes
    expect_equal(count_nodes(pl_tree$x$data$children), nrow(run_df))
    # Confirm number of origins plotted
    expect_equal(length(pl_tree$x$data$children), 2)
  })

  it("Broken links", {
    # A based_on referenced model no longer exists at the expected location
    # This will introduce another origin node for each missing model.
    clean_test_enviroment(create_tree_models(broken_link = TRUE))
    run_df <- run_log(MODEL_DIR)

    expect_warning(
      tree_data <- make_tree_data(run_df),
      "The following models could not be linked properly"
    )
    # Confirm two origins in data
    expect_equal(sum(grepl("Start", tree_data$from)), 2)

    pl_tree <- model_tree(run_df) %>% suppressWarnings()
    # Confirm number of expected nodes
    # Here an extra node is made for mod 1000. mod 1000 is not present in the
    # run log (was deleted), but is referenced as based_on in mod 1001.
    expect_equal(count_nodes(pl_tree$x$data$children), nrow(run_df) + 1)
    # Confirm number of origins plotted
    expect_equal(length(pl_tree$x$data$children), 2)
  })

  it("recursive run log", {
    clean_test_enviroment(create_tree_models)
    fs::dir_create(LEVEL2_DIR)
    mod_nest <- copy_model_from(
      MOD1, file.path(LEVEL2_SUBDIR, MOD_ID), "level 2 copy of 1.yaml",
      .inherit_tags = TRUE
    )
    fs::dir_copy(MOD1_PATH, LEVEL2_MOD)
    run_df <- run_log(MODEL_DIR, .recurse = TRUE)

    # Check data prep
    tree_data <- make_tree_data(run_df)
    # Confirm based_on subdirectories are replaced with just the model id & run
    # ids now include the subdirectory
    expect_false(any(grepl("\\Q..\\E", tree_data$based_on)))
    expect_true(any(grepl("level2/1", tree_data$run)))

    pl_tree <- model_tree(run_df)
    # Confirm number of expected nodes
    expect_equal(count_nodes(pl_tree$x$data$children), nrow(run_df))
    # Confirm number of origins plotted
    expect_equal(length(pl_tree$x$data$children), 1)
    # Conirm number of models based on MOD1 (one is nested in LEVEL2_DIR)
    expect_equal(length(pl_tree$x$data$children[[1]]$children), 2)
  })

  it("combine cases", {
    # This combines all the above cases and is meant to ensure there is no
    # interaction between these cases. It duplicates some of the expectations
    # in those individual tests just to make sure they are still true.
    clean_test_enviroment(
      create_tree_models(
        addl_based_on = TRUE, multiple_origins = TRUE, broken_link = TRUE
      )
    )
    fs::dir_create(LEVEL2_DIR)
    mod_nest <- copy_model_from(
      MOD1, file.path(LEVEL2_SUBDIR, MOD_ID), "level 2 copy of 1.yaml",
      .inherit_tags = TRUE
    )
    fs::dir_copy(MOD1_PATH, LEVEL2_MOD)

    run_df <- run_log(MODEL_DIR, .recurse = TRUE)

    # Check broken links case
    expect_warning(
      tree_data <- make_tree_data(run_df),
      "The following models could not be linked properly"
    )
    # Check multiple origins case
    expect_equal(sum(grepl("Start", tree_data$from)), 2)
    # Check additional based on case
    addl_based_on <- tree_data$addl_based_on
    expect_equal(addl_based_on[!is.na(addl_based_on)], c("1, 3", "2"))
    expect_equal(sum(is.na(addl_based_on)), 10)
    # Check recursive case
    expect_false(any(grepl("\\Q..\\E", tree_data$based_on)))
    expect_true(any(grepl("level2/1", tree_data$run)))

    # Check plot
    pl_tree <- model_tree(run_df) %>% suppressWarnings()
    # Confirm total number of expected nodes (not including start node)
    # (see broken link test for why 1 is added to the number of rows)
    expect_equal(count_nodes(pl_tree$x$data$children), nrow(run_df) + 1)
    # Confirm number of origins plotted
    expect_equal(length(pl_tree$x$data$children), 2)
    # Confirm split-off cases (multiple children per node) & number of child nodes
    expect_equal(length(pl_tree$x$data$children), 2)
    expect_equal(length(pl_tree$x$data$children[[1]]$children), 2)
    expect_equal(count_nodes(pl_tree$x$data$children[[1]]$children), 7)
    expect_equal(length(pl_tree$x$data$children[[1]]$children[[1]]$children), 2)
    expect_equal(count_nodes(pl_tree$x$data$children[[1]]$children[[1]]$children), 5)
  })
})

describe("model_tree() data setup",{
  it("make_tree_data()", {
    # This function is tested for more unique cases in other tests
    clean_test_enviroment(create_tree_models)
    run_df <- run_log(MODEL_DIR)
    tree_data <- make_tree_data(run_df)

    # Check other model_tree expectations
    expect_true(inherits(tree_data$based_on, "character"))

    # Tags are unlisted and formatted when included as part of the tooltip
    expect_true(inherits(tree_data$tags, "character"))

    # Check columns for various configurations
    tree_data <- make_tree_data(run_df, include_info = "star")
    # Tags remain unchanged when not part of the tooltip
    expect_true(inherits(tree_data$tags, "list"))

    # Summary columns are not included if add_summary = FALSE, unless you pass
    # it in via `include_info`
    tree_data <- run_df %>% add_summary() %>%
      make_tree_data(include_info = "ofv", add_summary = FALSE)
    expect_true("ofv" %in% names(tree_data))
    expect_false("any_heuristics" %in% names(tree_data))
  })

  it("make_model_network()", {
    clean_test_enviroment(create_tree_models)
    run_df <- run_log(MODEL_DIR) %>% add_model_status()
    # Replace NULL based_on elements with empty string to preserve rows when unnesting
    run_df <- run_df %>% dplyr::mutate(
      based_on = purrr::map(.data$based_on, function(.x){if(is.null(.x)) "" else .x}),
    ) %>% tidyr::unnest("based_on")
    # run log classes are removed when unnesting columns
    class(run_df) <- c("bbi_run_log_df", "bbi_log_df", class(run_df))
    network_df <- make_model_network(run_df)

    # Check expected collapsibleTree attributes
    expect_true(is.na(network_df$from[1]))
    expect_equal(network_df$to[1], "Start")
    expect_equal(network_df$from[2],"Start")
    expect_equal(network_df$to[2], "1")
    expect_equal(network_df$status[1], paste0("Model Directory:<br>", MODEL_DIR))
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
      as.character(tree_data$col),
      # green (start node), white/FALSE, red/TRUE (starred), white/FALSE x3
      c("#007319", "#FFFFFF", "#EB003D", rep("#FFFFFF", 3))
    )
  })

  it("static plot", {
    skip_if_tree_missing_deps(static = TRUE)
    clean_test_enviroment(create_tree_models)
    pl_tree <- model_tree(MODEL_DIR, static = TRUE)
    # Just check that the class was assigned and the right
    # data is returned
    expect_true(inherits(pl_tree, "model_tree_static"))
    expect_true(inherits(pl_tree$png_array, "array"))
  })
})
