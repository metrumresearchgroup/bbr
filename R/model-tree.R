#' Create a tree diagram of a modeling directory
#'
#' @param log_df a `bbr` run log.
#' @param include_info vector of [bbr::run_log()] columns to include in the tooltip or
#'        appended table.
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
#' @keywords internal
model_tree <- function(log_df,
                       include_info = c("description","star", "tags"),
                       add_summary = TRUE,
                       static = FALSE
){
  # Make sure required dependencies are installed
  stop_if_tree_missing_deps(static = static)

  # Make tree data
  tree_data <- make_tree_data(log_df, include_info, add_summary)
  network_df <- tree_data$network_df
  attr_cols <- tree_data$attr_cols

  # Create model tree
  pl_tree <- collapsibleTree::collapsibleTreeNetwork(
    network_df, zoomable = FALSE, attribute = c("star"),
    fill="col", collapsed = FALSE, nodeSize = "leafCount",
    tooltipHtml = "tooltip")

  if(isTRUE(static)){
    model_tree_png(pl_tree)
  }else{
    return(pl_tree)
  }
}


#' @describeIn model_tree Construct dataset for use in `model_tree`
make_tree_data <- function(
    log_df,
    include_info = c("description","star", "tags"),
    add_summary = TRUE
){

  # Default tooltip columns
  checkmate::assert_true(all(include_info %in% colnames(log_df)))
  attr_cols <- include_info

  # Replace NULL based_on elements with NA to preserve rows when unnesting
  full_log <- log_df %>% dplyr::mutate(
    based_on = purrr::map(.data$based_on, \(.x){if(is.null(.x)) "" else .x}),
    tags = purrr::map(tags, \(.x){if(is.null(.x)) "" else paste(.x, collapse = ", ")})
  ) %>% tidyr::unnest(c("based_on", "tags"))

  # Handling for multiple based_on flags
  dup_rows <- duplicated(full_log[[ABS_MOD_PATH]])
  if(any(dup_rows)){
    addl_based_on <- full_log[dup_rows,] %>%
      dplyr::select(all_of(c(ABS_MOD_PATH, "based_on"))) %>%
      tidyr::nest("based_on" = "based_on") %>%
      dplyr::mutate(
        based_on = purrr::map(.data$based_on, \(.x){ paste(.x$based_on, collapse = ", ")})
      ) %>% tidyr::unnest("based_on")

    # Remove duplicate rows
    full_log <- full_log[!dup_rows,]

    # Add additional based_on flags to new column
    full_log$addl_based_on <- NA_character_
    full_log$addl_based_on[full_log[[ABS_MOD_PATH]] %in% addl_based_on[[ABS_MOD_PATH]]] <- addl_based_on$based_on
    # Add column to tooltip
    attr_cols <- c(attr_cols, "addl_based_on")
  }

  # Truncate tags
  full_log <- full_log %>% dplyr::mutate(
    tags = stringr::str_trunc(tags, 30)
  )


  # Optionally append model summary information
  if(isTRUE(add_summary)){
    sum_cols <- c("status", "number_of_subjects", "number_of_obs", "ofv")
    mod_list <- purrr::map(unique(full_log[[ABS_MOD_PATH]]), bbr::read_model) %>%
      bbr::suppressSpecificWarning("incomplete final line")
    mods_run <- purrr::map(mod_list, bbi_nonmem_model_status)

    sums_df <- bbr::add_summary(log_df) %>% suppressWarnings() %>%
      dplyr::mutate(status = purrr::map_chr(mod_list, bbi_nonmem_model_status)) %>%
      dplyr::select(all_of(c(ABS_MOD_PATH, sum_cols)))

    full_log <- dplyr::left_join(full_log, sums_df, by = ABS_MOD_PATH)
    # Add summary columns to tooltip
    attr_cols <- c(attr_cols, sum_cols)
  }

  # Create model network and add tooltips
  network_df <- make_model_network(full_log, attr_cols)

  return(
    list(
      network_df = network_df,
      attr_cols = attr_cols
    )
  )
}


#' Make model network for use in `model_tree`
#' @param full_log full log including model run and summary information
#' @param attr_cols attribute columns to include in tooltip or table
#' @noRd
make_model_network <- function(full_log, attr_cols){

  # Create Network
  start <- "Start" # TODO: maybe replace directory with dirname?
  noref <- data.frame(from = NA, to = start)
  parent_mods <- data.frame(from = start,
                            to = full_log$run[full_log$based_on==""])
  child_mods <- data.frame(from = full_log$based_on[full_log$based_on!=""],
                           to = full_log$run[full_log$based_on!=""])
  network_df <- rbind(noref, parent_mods, child_mods)

  # Check network links - remove any unlinked models
  network_df <- check_model_tree(network_df)

  # Add attributes
  network_df <- dplyr::left_join(network_df, full_log, by = c("to" = "run")) %>%
    dplyr::select(all_of(c("from", "to", attr_cols)))

  # Color by star (potentially user-specified variable)
  network_df$col <- factor(network_df$star)
  levels(network_df$col) <- c("#edf8fb","#810f7c")


  # Compile attributes into tooltip
  network_df$tooltip <- make_tree_tooltip(network_df)

  return(network_df)
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
    missing_roots <- purrr::map_dfr(unlinked_roots, \(.x){
      data.frame(from = start, to = .x)
    })
    network_df <- rbind(network_df, missing_roots)

    unlinked_model_txt <- paste0("{.code ", unlinked_roots, "}", collapse = ", ")
    msg <- glue::glue("The following models could not be linked properly: {{unlinked_model_txt}}",
                      .open = "{{", .close = "}}")
    cli::cli_warn(c(
      "!" = msg,
      "i" = "Setting these as base models in tree diagram.",
      "i" = "Check the yaml files or run {.code bbr::run_log} to make sure all
      `based_on` models still exist"
    ))
  }

  return(network_df)
}

#' Create tooltip for interactive [model_tree()]
#' @inheritParams check_model_tree
#' @noRd
make_tree_tooltip <- function(network_df){

  style_html <- function(txt, color, ...){
    paste0(glue::glue("<span style='color:{color};"), ...,"'>" ,txt,"</span>")
  }

  # Tooltip from run log
  tooltip <- purrr::imap_chr(network_df$to, \(.x, .y){
    mod_html <- paste0(
      "<span style='font-size:14px; color:#538b01; font-weight:bold;'>", .x,
      "</span><br>")

    # Additional based_on flags: NULL if column doesnt exist, or NA for specific
    # models with only one based_on flag
    based_on_addl_html <- ifelse(
      is.null(network_df$addl_based_on) || is.na(network_df$addl_based_on[.y]), "",
      paste0("<span style='font-weight:bold;'>", "Additional Based on: ",
             network_df$addl_based_on[.y], "</span><br>")
    )
    desc_html <- ifelse(
      is.na(network_df$description[.y]), "",
      paste0("<span style='font-style:italic;'>", network_df$description[.y],
             "</span><br>")
    )
    tags_html <- ifelse(
      network_df$tags[.y] =="", "",
      paste0("Tags: ", network_df$tags[.y], "<br>")
    )
    star_html <- ifelse(
      isTRUE(network_df$star[.y]),
      paste0("<span style='color: #87CEEB'>Starred</span><br>"), ""
    )

    paste0(mod_html, based_on_addl_html, desc_html, star_html, tags_html)
  })

  # Tooltip from model summary
  add_summary <- "status" %in% names(network_df)
  if(isTRUE(add_summary)){
    sum_tooltip <- purrr::imap_chr(network_df$status, \(mod_status, .y){
      if(grepl("Finished", mod_status)){
        paste0(
          "<br>",
          "<span style='color:#538b01; font-weight:bold'>", mod_status,"</span><br>",
          "OFV: ", style_html(network_df$ofv[.y],color = "#A30000"), "<br>",
          "N Subjects: ", style_html(network_df$number_of_subjects[.y], color = "#A30000"), "<br>",
          "N Obs: ", style_html(network_df$number_of_obs[.y], color = "#A30000")
        )
      }else{
        paste0(
          "<br>",
          "<span style='color:#A30000; font-weight:bold'>", mod_status,"</span>"
        )
      }
    })
    tooltip <- paste0(tooltip, sum_tooltip)
  }
  return(tooltip)
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
#' @params static Logical (`TRUE`/`FALSE`). If `TRUE`, check for additional
#'  required packages for rendering the plot as a PNG.
#' @details
#' The packages below are required for both interactive and static viewing:
#'  - `collapsibleTree` for the core plot
#'
#' The packages below are all used for rendering the model tree as a static image:
#'  - `htmlwidgets` is a dependency of `collapsibleTree`, so it's a 'free' import
#'  - `grid` is a built-in R package
#'  - `webshot` is needed for taking a screenshot of the rendered HTML
#'  - `png` is needed for reading in a PNG file in order to plot it in Rstudio
#'
#' @keywords internal
req_tree_pkgs <- function(static = FALSE){
  if(isTRUE(static)){
    c("collapsibleTree", "htmlwidgets", "webshot", "png", "grid")
  }else{
    "collapsibleTree"
  }
}

#' Checks if all packages needed for [model_tree()] are present
#'
#' Returns a vector with the missing packages, or returns NULL if all are
#' present.
#' @inheritParams req_tree_pkgs
#' @noRd
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

#' Skip tests if missing [model_tree()] dependencies
#' @inheritParams req_tree_pkgs
#' @noRd
skip_if_tree_missing_deps <- function(static = FALSE) {
  missing_pkgs <- check_for_model_tree_pkgs(static = static)
  testthat::skip_if(
    !is.null(missing_pkgs),
    glue::glue("Skipped because the following packages are needed for this test: {paste(missing_pkgs, collapse = ', ')}")
  )
}

#' Error if missing [model_tree()] dependencies
#' @inheritParams req_tree_pkgs
#' @noRd
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

