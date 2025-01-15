context("Model tree diagram")
skip_if_not_ci_or_metworx("test-model-tree")
skip_if_tree_missing_deps()

# These two functions ignore the 'start' node, as we are only comparing
# to the run_log

# Count how many nodes appear in the model tree for each model
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

# Get node attribute for each model
get_node_attribute <- function(tree_list, attr = 'SizeOfNode') {
  if(length(tree_list) == 0) return(numeric(0))
  # Iterate through each element in the list
  attribute_values <- numeric(0)
  for(i in seq_along(tree_list)) {
    # Check if the specified attribute exists in the current node
    if(!is.null(tree_list[[i]][[attr]])){
      attr_value <- tree_list[[i]][[attr]]
      if(is.factor(attr_value)) attr_value <- as.character(attr_value)
      attribute_values <- c(attribute_values, attr_value)
    }
    # If the current node has children, recursively get the attribute from children
    if(length(tree_list[[i]]$children) > 0){
      attribute_values <- c(attribute_values, get_node_attribute(tree_list[[i]]$children, attr))
    }
  }
  return(attribute_values)
}

withr::with_options(list(bbr.bbi_exe_path = read_bbi_path()), {


  describe("model_tree() integration", {
    it("default behavior", {
      clean_test_enviroment(create_tree_models)
      run_df <- run_log(MODEL_DIR)
      tree_data <- make_tree_data(run_df, add_summary = FALSE)

      # Confirm one origin in data
      expect_equal(sum(grepl("Start", tree_data$from)), 1)

      pl_tree <- model_tree(run_df, add_summary = FALSE)
      # Confirm number of expected nodes
      expect_equal(count_nodes(pl_tree$x$data$children), nrow(run_df))
      # Confirm number of origins plotted
      expect_equal(length(pl_tree$x$data$children), 1)
    })

    it("additional based on", {
      # Includes models that have multiple based_on attributes
      clean_test_enviroment(create_tree_models(addl_based_on = TRUE))
      run_df <- run_log(MODEL_DIR)
      tree_data <- make_tree_data(run_df, add_summary = FALSE)
      addl_based_on <- tree_data$addl_based_on

      # Additional based on attributes are stored as a separate column and
      # included in the tooltip. The first one found is used to create the link
      expect_equal(addl_based_on[!is.na(addl_based_on)], c("1, 3", "2"))
      expect_equal(sum(is.na(addl_based_on)), 6)

      pl_tree <- model_tree(run_df, add_summary = FALSE)
      # Confirm number of expected nodes
      expect_equal(count_nodes(pl_tree$x$data$children), nrow(run_df))
      # Confirm number of origins plotted
      expect_equal(length(pl_tree$x$data$children), 1)
    })

    it("multiple origins", {
      # Multiple starting models (models without a `based_on` attribute)
      clean_test_enviroment(create_tree_models(multiple_origins = TRUE))
      run_df <- run_log(MODEL_DIR)
      tree_data <- make_tree_data(run_df, add_summary = FALSE)

      # Confirm two origins in data
      expect_equal(sum(grepl("Start", tree_data$from)), 2)

      pl_tree <- model_tree(run_df, add_summary = FALSE)
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
        tree_data <- make_tree_data(run_df, add_summary = FALSE),
        "The following models could not be linked properly"
      )
      # Confirm two origins in data
      expect_equal(sum(grepl("Start", tree_data$from)), 2)

      pl_tree <- model_tree(run_df, add_summary = FALSE) %>% suppressWarnings()
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
      tree_data <- make_tree_data(run_df, add_summary = FALSE)
      # Confirm based_on subdirectories are replaced with just the model id & run
      # ids now include the subdirectory
      expect_false(any(grepl("\\Q..\\E", tree_data$based_on)))
      expect_true(any(grepl("level2/1", tree_data$run)))

      pl_tree <- model_tree(run_df, add_summary = FALSE)
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
        tree_data <- make_tree_data(run_df, add_summary = FALSE),
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
      pl_tree <- model_tree(run_df, add_summary = FALSE) %>% suppressWarnings()
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

    it("Include bootstrap model", {
      skip_if_old_bbi("3.2.0") # calls model_summary()
      clean_test_enviroment(create_tree_models)
      boot_run <- make_fake_boot(MOD1, n = 3)
      on.exit(delete_models(boot_run, .tags = NULL, .force = TRUE), add = TRUE)
      run_df <- run_log(MODEL_DIR)
      tree_data <- make_tree_data(run_df, add_summary = TRUE)

      # Confirm one origin in data
      expect_equal(sum(grepl("Start", tree_data$from)), 1)

      pl_tree <- model_tree(run_df, add_summary = TRUE)
      # Confirm number of expected nodes
      expect_equal(count_nodes(pl_tree$x$data$children), nrow(run_df))
      # Confirm number of origins plotted
      expect_equal(length(pl_tree$x$data$children), 1)

      # confirm bootstrap formatting
      tree_data <- make_tree_tooltip(tree_data)
      expect_true(grepl("Bootstrap Run", tree_data$tooltip[3]))
    })

    it("Include a simulation", {
      skip_if_old_bbi("3.2.0") # calls model_summary()
      # Set up fake simulation
      mod_sim <- make_fake_sim(MOD1)
      sim_inc <- get_simulation(mod_sim)
      on.exit(
        delete_models(list(mod_sim, sim_inc), .tags = NULL, .force = TRUE),
        add = TRUE
      )

      run_df <- run_log(MODEL_DIR)
      tree_data <- make_tree_data(run_df, add_summary = TRUE)

      # Confirm one origin in data
      expect_equal(sum(grepl("Start", tree_data$from)), 1)

      pl_tree <- model_tree(run_df, add_summary = TRUE)
      # Confirm number of expected nodes
      expect_equal(count_nodes(pl_tree$x$data$children), nrow(run_df))
      # Confirm number of origins plotted
      expect_equal(length(pl_tree$x$data$children), 1)

      # confirm bootstrap formatting
      tree_data <- make_tree_tooltip(tree_data)
      expect_true(grepl("Simulation attached", tree_data$tooltip[3]))
    })
  })

  describe("model_tree() data setup",{
    it("make_tree_data()", {
      # This function is tested for more unique cases in other tests
      clean_test_enviroment(create_tree_models)
      run_df <- run_log(MODEL_DIR)
      tree_data <- make_tree_data(run_df, add_summary = FALSE)

      # Check other model_tree expectations
      expect_true(inherits(tree_data$based_on, "character"))

      # Tags are unlisted and formatted when included as part of the tooltip
      expect_true(inherits(tree_data$tags, "character"))

      # Check columns for various configurations
      tree_data <- make_tree_data(run_df, add_summary = FALSE, include_info = "star")
      # Tags remain unchanged when not part of the tooltip
      expect_true(inherits(tree_data$tags, "list"))
    })

    it("summary column inclusion", {
      skip_if_old_bbi("3.2.0") # calls model_summary()
      clean_test_enviroment(create_tree_models)
      run_df <- run_log(MODEL_DIR)

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
      skip_if_old_bbi("3.2.0") # calls model_summary()
      clean_test_enviroment(create_tree_models)
      # With summary (default)
      tree_data <- make_tree_data(run_log(MODEL_DIR), add_summary = TRUE)
      tree_data <- make_tree_tooltip(tree_data)
      # Starting node
      expect_true(grepl(MODEL_DIR, tree_data$tooltip[1]))
      # Spot check some rendered tooltips
      expect_true(grepl(paste0(MOD1$tags, collapse = ", "), tree_data$tooltip[2]))
      expect_true(grepl(MOD1$description, tree_data$tooltip[2]))
      expect_true(grepl("NONMEM Model", tree_data$tooltip[2]))
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

      # Attribute checks
      pl_tree <- model_tree(MODEL_DIR, add_summary = FALSE)
      expect_equal(pl_tree$x$options$attribute, "run")
      pl_tree <- model_tree(MODEL_DIR, add_summary = FALSE, color_by = "star")
      expect_equal(pl_tree$x$options$attribute, "star")

      ### Data checks ###
      # Test logical color_by
      tree_data <- make_tree_data(run_log(MODEL_DIR), add_summary = FALSE)
      tree_data_star <- color_tree_by(tree_data, color_by = "star")
      expect_equal(
        as.character(tree_data_star$col),
        # green (start node), white/FALSE, red/TRUE (starred), white/FALSE x3
        c("#007319", "#FFFFFF", "#EB003D", rep("#FFFFFF", 3))
      )

      # Check if only FALSE
      tree_data$star[2:nrow(tree_data)] <- FALSE
      tree_data_star <- color_tree_by(tree_data, color_by = "star")
      expect_equal(
        as.character(tree_data_star$col),
        # green (start node), white/FALSE x5
        c("#007319", rep("#FFFFFF", 5))
      )

      # Check if only TRUE
      tree_data$star[2:nrow(tree_data)] <- TRUE
      tree_data_star <- color_tree_by(tree_data, color_by = "star")
      expect_equal(
        as.character(tree_data_star$col),
        # green (start node), red/TRUE (starred) x5
        c("#007319", rep("#EB003D", 5))
      )

      # Check NA values
      tree_data$star[nrow(tree_data)] <- NA
      tree_data_star <- color_tree_by(tree_data, color_by = "star")
      expect_equal(
        as.character(tree_data_star$col),
        # green (start node), red/TRUE (starred) x4, grey/NA
        c("#007319", rep("#EB003D", 4), "#C0C0C0")
      )

      # Test character color_by (gradient coloring)
      tree_data <- make_tree_data(run_log(MODEL_DIR), add_summary = FALSE)
      tree_data_run <- color_tree_by(tree_data, color_by = "run")
      expect_equal(
        as.character(tree_data_run$col),
        # green (start node), gradient coloring between white and red
        # Note: all gradient colors will shift if number of models change
        c("#007319", "#FFFFFF", "#F4DBD3", "#ED9D84", "#E35B44", "#EB003D")
      )

      # Regression test: numeric color_by is sorted appropriately
      log_df <- run_log(MODEL_DIR)
      log_df$numeric_vals <- c(1534, 3892, 731, 2653, 3574)
      pl_tree <- model_tree(
        log_df, add_summary = FALSE, color_by = "numeric_vals",
        include_info = "numeric_vals"
      )
      node_colors <- get_node_attribute(pl_tree$x$data$children, attr = "fill")
      expected_colors <- c("#F4DBD3", "#EB003D", "#FFFFFF", "#ED9D84", "#E35B44")
      # Can inspect with `scales::show_col(node_colors)`
      expect_equal(node_colors, expected_colors)
    })

    it("size_tree_by()", {
      clean_test_enviroment(create_tree_models)

      log_df <- run_log(MODEL_DIR) %>% dplyr::mutate(
        size_col = as.integer(run)
      )

      # Checks that the size increases with each node (like size_col, i.e. run number)
      pl_tree <- model_tree(log_df, add_summary = FALSE, size_by = "size_col")
      node_sizes <- get_node_attribute(pl_tree$x$data$children, attr = "SizeOfNode")
      expect_true(all(diff(node_sizes) > 0))

      ### Data checks ###

      # Test numeric size_by (gradient sizing) - mimics objective function
      set.seed(1234)
      log_df <- log_df %>% dplyr::mutate(
        size_col = abs(rnorm(nrow(log_df), mean = 1500, sd = 800))
      )
      size_col_vals <- log_df$size_col
      pl_tree <- model_tree(log_df, add_summary = FALSE, size_by = "size_col")
      node_sizes <- get_node_attribute(pl_tree$x$data$children, attr = "SizeOfNode")

      tree_data <- make_tree_data(log_df, add_summary = FALSE, size_by = "size_col")
      tree_data_size <- size_tree_by(tree_data, size_by = "size_col")
      data_sizes <- tree_data_size$node_size[-1]

      # Checks that the ordering is consistent
      # - Checks the underlying data, and rendered node size
      expect_equal(order(size_col_vals), order(node_sizes))
      expect_equal(order(size_col_vals), order(data_sizes))

      # Check if all the same value
      log_df2 <- log_df
      log_df2$size_col <- 1
      size_col_vals <- log_df2$size_col
      pl_tree <- model_tree(log_df2, add_summary = FALSE, size_by = "size_col")
      node_sizes <- get_node_attribute(pl_tree$x$data$children, attr = "SizeOfNode")

      tree_data <- make_tree_data(log_df2, add_summary = FALSE, size_by = "size_col")
      tree_data_size <- size_tree_by(tree_data, size_by = "size_col")
      data_sizes <- tree_data_size$node_size[-1]

      # Checks that all values are the same size
      # - Checks the underlying data, and rendered node size
      expect_true(dplyr::n_distinct(node_sizes) == 1)
      expect_true(dplyr::n_distinct(data_sizes) == 1)

      ## Warns if non-numeric (or non-integer) column ##
      log_df2 <- log_df2 %>% dplyr::mutate(run = as.character(run))
      # Check logical
      expect_warning(
        pl_tree <- model_tree(log_df2, add_summary = FALSE, size_by = "star"),
        'Only numeric columns are supported'
      )
      node_sizes <- get_node_attribute(pl_tree$x$data$children, attr = "SizeOfNode")
      expect_true(dplyr::n_distinct(node_sizes) == 2) # leafCount sizing
      # Check character
      expect_warning(
        pl_tree <- model_tree(log_df2, add_summary = FALSE, size_by = "star"),
        'Only numeric columns are supported'
      )
      node_sizes <- get_node_attribute(pl_tree$x$data$children, attr = "SizeOfNode")
      expect_true(dplyr::n_distinct(node_sizes) == 2) # leafCount sizing
    })

    it("static plot", {
      skip_if_tree_missing_deps(static = TRUE)
      clean_test_enviroment(create_tree_models)
      pl_tree <- model_tree(MODEL_DIR, add_summary = FALSE, static = TRUE)
      # Just check that the class was assigned and the right
      # data is returned
      expect_true(inherits(pl_tree, "model_tree_static"))
      expect_true(inherits(pl_tree$png_array, "array"))
    })

    it("Check for missing columns", {
      clean_test_enviroment(create_tree_models)
      # Required columns are missing
      log_df <- run_log(MODEL_DIR) %>% dplyr::select(-c("run", "based_on", "model_type"))
      expect_error(
        model_tree(log_df, add_summary = FALSE),
        "columns are missing"
      )
      # Specified columns are missing
      expect_error(
        model_tree(MODEL_DIR, add_summary = FALSE, include_info = c("oops_I", "did_it_again")),
        "columns are missing"
      )
    })
  })
}) # closing withr::with_options
