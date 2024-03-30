#' Create a tree diagram of a modeling directory
#'
#' @param log_df a `bbr` run log or a base directory to look in for models.
#' @param include_info vector of [bbr::run_log()] columns to include in the tooltip or
#'        appended table.
#' @param color_by a run log column to color the nodes by. Can be helpful for
#'        identifying which models are starred, have heuristics, etc. Defaults
#'        to `"run"`.
#' @param add_summary Logical (`TRUE`/`FALSE`). If `TRUE`, include key columns
#'        from [model_summary()] output.
#' @param static Logical (`TRUE`/`FALSE`). If `TRUE`, render the plot as a
#'        static image. This takes a little longer, as the interactive plot must
#'        be saved as a PNG and loaded into the viewer.
#'
#' @details
#' Uses the `based_on` attribute of each model to determine the tree network.
#'  - Additional `based_on` flags will be shown in the tooltip, using the first one
#' to create the tree network
#'
#'
#' @export
model_tree <- function(
    log_df,
    include_info = c("description","star", "tags"),
    color_by = "run",
    add_summary = TRUE,
    static = FALSE
){
  UseMethod("model_tree")
}

#' @rdname model_tree
#' @export
model_tree.character <- function(
    log_df,
    include_info = c("description","star", "tags"),
    color_by = "run",
    add_summary = TRUE,
    static = FALSE
){
  checkmate::assert_directory_exists(log_df)
  log_df <- run_log(log_df) %>% suppressWarnings()
  model_tree(log_df, include_info, color_by, add_summary, static)
}

#' @rdname model_tree
#' @export
model_tree.bbi_log_df <- function(
    log_df,
    include_info = c("description","star", "tags"),
    color_by = "run",
    add_summary = TRUE,
    static = FALSE
){
  # Make sure required dependencies are installed
  stop_if_tree_missing_deps(static = static)

  # Make tree data
  tree_data <- make_tree_data(log_df, include_info, add_summary)

  # Format coloring
  tree_data <- color_tree_by(tree_data, color_by = color_by)

  # Compile attributes into tooltip
  tree_data <- make_tree_tooltip(tree_data)

  # Create model tree
  pl_tree <- collapsibleTree::collapsibleTreeNetwork(
    tree_data, zoomable = FALSE, attribute = color_by,
    fill="col", collapsed = FALSE, nodeSize = "leafCount",
    tooltipHtml = "tooltip")

  if(isTRUE(static)){
    model_tree_png(pl_tree)
  }else{
    return(pl_tree)
  }
}


#' Construct dataset for use in `model_tree`
#' @param log_df a `bbr` run log
#' @inheritParams model_tree
#'
#' @details
#' This function does the following things:
#'  - Performs checks on required columns
#'  - Unnests nested columns (such as `based_on` and `tags`)
#'    - Both character (`'Two CMT'`) and list (`AMT: nmol`) tags are supported
#'  - Formats columns and performs prep work for creating a tooltip
#'  - Creates a model network using the `based_on` column. If additional
#'  based_on models were used, the first one will be used to create the network;
#'  the rest will be stored in a separate `addl_based_on` column.
#'    - This is represented via the `from` and `to` columns.
#'
#' @returns a dataframe
#' @keywords internal
make_tree_data <- function(
    log_df,
    include_info = c("description","star", "tags"),
    add_summary = TRUE
){
  checkmate::assert_true(all(include_info %in% names(log_df)))

  # Check for required columns and starting format
  req_cols <- c(ABS_MOD_PATH, "run", "based_on")
  if(!(all(req_cols %in% names(log_df)))){
    cols_missing <- req_cols[!(req_cols %in% names(log_df))]
    cols_missing <- paste(cols_missing, collapse = ", ")
    rlang::abort(
      glue::glue("The following required columns are missing: {cols_missing}")
    )
  }
  checkmate::assert_true(inherits(log_df$based_on, "list"))

  # Starting run log
  log_df <- log_df %>% dplyr::select(all_of(c(req_cols, include_info)))

  # Tooltip columns (may be appended later)
  attr_cols <- include_info

  # Replace NULL based_on elements with NA to preserve rows when unnesting
  full_log <- log_df %>% dplyr::mutate(
    based_on = purrr::map(.data$based_on, function(.x){if(is.null(.x)) "" else .x}),
  ) %>% tidyr::unnest("based_on")

  if("tags" %in% include_info){
    full_log <- full_log %>% dplyr::mutate(
      tags = purrr::map(.data$tags, function(m_tags){
        if(is.null(m_tags)){
          ""
        }else{
          # Check if any individual tags were defined as a list. Some users make
          # tags like `AMT: nmol`, which would show up as `list(AMT = "nmol")`
          # without this logic.
          has_lst_tags <- purrr::map_lgl(m_tags, ~ inherits(.x, "list"))
          if(any(has_lst_tags)){
            tags_lst <- unlist(m_tags[has_lst_tags])
            tags_lst <- paste(names(tags_lst), tags_lst, sep = ": ")
            m_tags[has_lst_tags] <- tags_lst
          }
          paste(m_tags, collapse = ", ")
        }
      })
    ) %>% tidyr::unnest("tags")

    # Truncate tags
    full_log <- full_log %>% dplyr::mutate(tags = stringr::str_trunc(tags, 30))
  }

  # Handling for multiple based_on flags
  full_log$addl_based_on <- NA_character_
  dup_rows <- duplicated(full_log[[ABS_MOD_PATH]])
  if(any(dup_rows)){
    based_on_addl_df <- full_log[dup_rows,] %>%
      dplyr::select(all_of(c(ABS_MOD_PATH, "based_on"))) %>%
      tidyr::nest("based_on" = "based_on") %>%
      dplyr::mutate(
        based_on = purrr::map(.data$based_on, function(.x){ paste(.x$based_on, collapse = ", ")})
      ) %>% tidyr::unnest("based_on")

    # Remove duplicate rows
    full_log <- full_log[!dup_rows,]

    # Add additional based_on flags to new column
    has_addl_based_on <- full_log[[ABS_MOD_PATH]] %in% based_on_addl_df[[ABS_MOD_PATH]]
    full_log$addl_based_on[has_addl_based_on] <- based_on_addl_df$based_on
    # Add column to tooltip (we have to display these since we cant draw them)
    attr_cols <- c(attr_cols, "addl_based_on")
  }


  # Optionally append model summary information
  # This must be done after filtering out any duplicate rows
  if(isTRUE(add_summary)){
    sum_cols <- c("status", "number_of_subjects", "number_of_obs", "ofv", "any_heuristics")
    # Add run log classes back to full_log to use add_summary().
    # - run log classes are removed when unnesting columns
    class(full_log) <- c("bbi_run_log_df", "bbi_log_df", class(full_log))
    full_log <- full_log %>% add_summary() %>% add_model_status() %>%
      dplyr::select(all_of(c(names(full_log), sum_cols)))

    # Add summary columns to tooltip
    attr_cols <- c(attr_cols, sum_cols)
  }

  # Create model network and append to full run log
  tree_data <- make_model_network(full_log)

  # Store columns to be used in tooltip
  attr(tree_data, "tooltip_columns") <- attr_cols

  return(tree_data)
}


#' Make model network for use in `model_tree`
#' @param full_log full log including model run and summary information
#' @noRd
make_model_network <- function(full_log){
  model_dir <- unique(dirname(full_log[[ABS_MOD_PATH]]))

  # Create Network
  start <- "Start" # TODO: maybe replace directory with dirname?
  noref <- data.frame(from = NA, to = start)
  parent_mods <- data.frame(from = start,
                            to = full_log$run[full_log$based_on==""])
  child_mods <- data.frame(from = full_log$based_on[full_log$based_on!=""],
                           to = full_log$run[full_log$based_on!=""])
  network_df <- rbind(noref, parent_mods, child_mods)

  # Check network links - set unlinked models as starting points
  network_df <- check_model_tree(network_df)

  # Join network to run log
  tree_data <- dplyr::left_join(network_df, full_log, by = c("to" = "run")) %>%
    dplyr::mutate(run = ifelse(.data$to == "Start", NA, .data$to)) %>%
    dplyr::relocate(all_of(c("from", "to", "run")))

  # Adjust status for any unlinked (missing) models
  tree_data <- tree_data %>% dplyr::mutate(
    status = ifelse(is.na(status) & to != "Start", "Not Found", status)
  )

  # Set status to be modeling directory for start node
  tree_data$status[tree_data$to=="Start"] <-
    paste0("Model Directory:<br>", fs::path_rel(model_dir))

  return(tree_data)
}



#' Perform checks regarding whether a tree can be made. Will set models as
#' base models if referenced `based_on` model cannot be found
#'
#' @param network_df dataframe indicating how models are related
#' @noRd
check_model_tree <- function(network_df){

  find_roots <- function(network_df){
    children <- network_df[ , 2]
    parents <- network_df[ , 1]
    root_name <- unique(parents[!(parents %in% children)])
    if (length(root_name) != 1){
      # Remove expected NA root
      unlinked_models <- root_name[!is.na(root_name)]
      return(unlinked_models)
    }
    return(NULL)
  }

  # Check network links
  if(!is.null(find_roots(network_df))){
    # Connect other roots to directory. Point all un-networked models to base directory
    start <- network_df$to[is.na(network_df$from)]
    unlinked_roots <- find_roots(network_df)
    missing_roots <- purrr::map_dfr(unlinked_roots, function(.x){
      data.frame(from = start, to = .x)
    })
    network_df <- rbind(network_df, missing_roots) %>% tibble::as_tibble()

    unlinked_model_txt <- paste0("`", unlinked_roots, "`", collapse = ", ")
    msg <- glue::glue("The following models could not be linked properly: {unlinked_model_txt}")
    rlang::warn(c(
      "!" = msg,
      "i" = "Setting these as starting models in tree diagram.",
      "i" = paste("Check the yaml files or run `bbr::run_log()` to make sure",
                  "all `based_on` models still exist")
    ))
  }

  return(network_df)
}

#' Create tooltip for interactive [model_tree()]
#' @param tree_data a combined dataframe that includes run log columns and
#'  defines the model network.
#' @noRd
make_tree_tooltip <- function(tree_data){

  # Columns to include in tooltip or table
  attr_cols <- attr(tree_data, "tooltip_columns")

  # Helper for coloring text and applying other styles
  style_html <- function(txt, color = "black", ..., br_before = FALSE, br_after = FALSE){
    txt <- paste0(glue::glue("<span style='color:{color};"), ...,"'>" ,txt,"</span>")
    if(isTRUE(br_before)) txt <- paste0("<br>", txt)
    if(isTRUE(br_after)) txt <- paste0(txt, "<br>")
    return(txt)
  }

  # Discern whether or not to display cell text
  can_include <- function(txt) !is.na(txt) && txt != ""

  # Tooltip from run log
  tooltip <- purrr::imap_chr(tree_data$to, function(.x, .y){
    mod_name <- ifelse(.x == "Start", .x, paste("Run", .x))
    mod_html <- style_html(
      mod_name, color = "#538b01", "font-size:14px; font-weight:bold", br_after = TRUE
    )

    # Additional based_on flags: NA for models with only one based_on flag
    based_on_addl_html <- ifelse(
      can_include(tree_data$addl_based_on[.y]),
      style_html(
        paste("Additional Based on:", tree_data$addl_based_on[.y]),
        "font-weight:bold", br_after = TRUE
      ),
      ""
    )
    desc_html <- ifelse(
      "description" %in% attr_cols && can_include(tree_data$description[.y]),
      style_html(tree_data$description[.y], "font-style:italic", br_after = TRUE),
      ""
    )
    tags_html <- ifelse(
      "tags" %in% attr_cols && can_include(tree_data$tags[.y]),
      paste0("Tags: ", style_html(tree_data$tags[.y], color = "#297f9c", br_after = TRUE)),
      ""
    )
    star_html <- ifelse(
      "star" %in% attr_cols && can_include(tree_data$star[.y]) && isTRUE(tree_data$star[.y]),
      style_html("Starred", color = "#ffa502", "font-weight:bold", br_after = TRUE),
      ""
    )

    paste0(mod_html, based_on_addl_html, desc_html, star_html, tags_html)
  })

  # Tooltip from model summary
  add_summary <- "status" %in% names(tree_data)
  if(isTRUE(add_summary)){
    sum_tooltip <- purrr::imap_chr(tree_data$status, function(mod_status, .y){
      if(grepl("Finished", mod_status)){
        # Conditional heuristics text
        any_heuristics <- tree_data$any_heuristics[.y]
        heuristics_txt <- if(!is.na(any_heuristics) && isTRUE(any_heuristics)){
          paste0("<br><br>", style_html("--Heuristics Found--", color = "#A30000", "font-weight:bold"))
        }else{
          ""
        }
        # Combined tooltip
        paste0(
          style_html(mod_status, color = "#538b01", "font-weight:bold", br_before = TRUE, br_after = TRUE),
          "OFV: ", style_html(tree_data$ofv[.y], color = "#A30000", br_after = TRUE),
          "N Subjects: ", style_html(tree_data$number_of_subjects[.y], color = "#A30000", br_after = TRUE),
          "N Obs: ", style_html(tree_data$number_of_obs[.y], color = "#A30000"),
          heuristics_txt
        )
      }else{
        # If not run, just show the status
        style_html(mod_status, color = "#A30000", "font-weight:bold", br_before = TRUE)
      }
    })
    tooltip <- paste0(tooltip, sum_tooltip)
  }

  tree_data$tooltip <- tooltip
  return(tree_data)
}

#' Create a color column based on the unique values of another column
#' @inheritParams make_tree_tooltip
#' @inheritParams model_tree
#' @noRd
color_tree_by <- function(tree_data, color_by = "run"){
  checkmate::assert_true(color_by %in% names(tree_data))
  # white and colors in bbr's logo (red and orange)
  # #f0cfc5 is a gradient color between bbr orange and white
  bbr_cols <- c("#ffffff","#f0cfc5", "#e06a46", "#eb003d")

  # Get colors for unique values (Dont assign one for NA values)
  # To preview colors: scales::show_col(pal_bbr)
  vals <- tree_data[[color_by]][!is.na(tree_data[[color_by]])]
  n_levels <- dplyr::n_distinct(vals)
  pal_bbr <- scales::pal_gradient_n(bbr_cols)(seq(0, 1, length.out = n_levels))

  # Assign colors to new column
  tree_data$col <- factor(tree_data[[color_by]])
  levels(tree_data$col) <- pal_bbr

  return(tree_data)
}

#' Save interactive [model_tree()] as PNG and load in Rstudio viewer
#'
#' This function first saves and renders widget as HTML. It then screenshots
#' the webpage and saves the file to a temporary PNG.
#'
#' @keywords internal
model_tree_png <- function(widget) {
  temp_html <- tempfile(pattern = "model-tree-", fileext = ".html")
  temp_png <- tempfile(pattern = "model-tree-", fileext = ".png")
  # Save widget as HTML
  htmlwidgets::saveWidget(widget, temp_html)
  # Screenshot HTML as PNG
  webshot::webshot(
    temp_html, temp_png,
    selector = "#htmlwidget_container",
    zoom = 2,
    delay = 0.75
  )
  # Load and plot PNG
  png_file <- png::readPNG(temp_png)
  grid::grid.newpage()
  grid::grid.raster(png_file)
}


#' Required packages for running [model_tree()]
#'
#' @param static Logical (`TRUE`/`FALSE`). If `TRUE`, check for additional
#'  required packages for rendering the plot as a PNG.
#' @details
#' The packages below are required for both interactive and static viewing:
#'  - **`collapsibleTree`** for the core plot
#'
#' The packages below are all used for rendering the model tree as a **static** image:
#'  - **`htmlwidgets`** is a dependency of `collapsibleTree`, so it's a 'free'
#'  import. It's used to save the tree diagram out as an `HTML` file.
#'  - **`grid`** is a built-in R package and is used to display the PNG in the
#'  Rstudio viewer.
#'  - **`webshot`** is needed for taking a screenshot of the rendered HTML,
#'  saved out via `htmlwidgets::saveWidget()`.
#'  - **`png`** is needed for reading in a PNG file in order to plot it in the
#'  Rstudio viewer.
#'
#' @keywords internal
req_tree_pkgs <- function(static = FALSE){
  if(isTRUE(static)){
    c("collapsibleTree", "htmlwidgets", "webshot", "png", "grid")
  }else{
    "collapsibleTree"
  }
}

#' @describeIn req_tree_pkgs Checks if all packages needed for [model_tree()] are present
#' @return Returns a vector with the missing packages, or returns NULL if all are
#' present.
check_for_model_tree_pkgs <- function(static = FALSE) {
  REQUIRED_TREE_PKGS <- req_tree_pkgs(static = static)
  pkgs_present <- purrr::map_lgl(REQUIRED_TREE_PKGS, function(.pkg) {
    requireNamespace(.pkg, quietly = TRUE)
  })

  if (any(!pkgs_present)) {
    return(REQUIRED_TREE_PKGS[!pkgs_present])
  } else {
    return(NULL)
  }
}

#' @describeIn req_tree_pkgs Skip tests if missing [model_tree()] dependencies
skip_if_tree_missing_deps <- function(static = FALSE) {
  missing_pkgs <- check_for_model_tree_pkgs(static = static)
  testthat::skip_if(
    !is.null(missing_pkgs),
    glue::glue("Skipped because the following packages are needed for this test: {paste(missing_pkgs, collapse = ', ')}")
  )
}

#' @describeIn req_tree_pkgs Error if missing [model_tree()] dependencies. Called internally
#'  within `model_tree()`
stop_if_tree_missing_deps <- function(static = FALSE) {
  missing_pkgs <- check_for_model_tree_pkgs(static = static)
  if (!is.null(missing_pkgs)) {
    msg <- paste(
      glue::glue("The following packages needed to run `model_tree()` are not installed: {paste(missing_pkgs, collapse = ', ')}"),
      "Consider running `install.packages('bbr', dependencies = TRUE)`",
      "\nIf using pkgr, add the following to your pkgr.yml and re-run `pkgr install`",
      "\nCustomizations:\n  Packages:\n    - bbr:\n        Suggests: true",
      sep = "\n"
    )
    # use rlang::abort to get the pkgr formatting to print correctly
    rlang::abort(msg, call. = FALSE)
  }
}

