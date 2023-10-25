#' Required packages for running [model_tree()]
#'
#' @details
#' The packages below are required for both interactive and static viewing:
#'  - `collapsibleTree` for the core plot
#'  - `kableExtra` and `manipulateWidget` for creating and adding a table
#'     respectively
#'
#' The packages below are all used for rendering the model tree as a static image:
#'  - `htmlwidgets` is a dependency of `collapsibleTree`, so it's a 'free' import
#'  - `grid` is a built-in R package
#'  - `webshot` is needed for taking a screenshot
#'  - `png` is needed for reading in a PNG file in order to plot it in Rstudio
#'
#' @keywords internal
REQUIRED_TREE_PKGS <- c("collapsibleTree", "manipulateWidget","kableExtra",
                        "htmlwidgets", "webshot", "png", "grid")


#' Create a tree diagram of a modeling directory
#'
#' @inheritParams run_log
#'
#' @details
#' Uses the `based_on` attribute of each model to determine the tree network
#'
#' Note: This will not (currently) work if `.based_on_additional` was set when
#'  `copy_model_from()` was used (i.e. it can only be based on one model)
#'
#' @export
model_tree <- function(.base_dir,
                       .recurse = FALSE,
                       .include = NULL,
                       .add_summary = TRUE,
                       .static_plot = FALSE,
                       .append_table = FALSE
){
  log_df <- run_log(.base_dir, .recurse, .include) %>%
    suppressSpecificWarning("incomplete final line")

  # Replace NULL based_on elements with NA to preserve rows when unnesting
  full_log <- log_df %>% dplyr::mutate(
    based_on = purrr::map(based_on, \(.x){if(is.null(.x)) "" else .x}),
    tags = purrr::map(tags, \(.x){if(is.null(.x)) "" else paste(.x, collapse = ", ")})
  ) %>% tidyr::unnest(c("based_on", "tags"))

  # Truncate tags
  full_log <- full_log %>% dplyr::mutate(
    tags = stringr::str_trunc(tags, 30)
  )

  attr_cols <- c("description","star", "tags")
  # Optionally append model summary information
  if(isTRUE(.add_summary)){
    sum_cols <- c("status", "number_of_subjects",
                  "number_of_obs", "ofv")
    mod_list <- purrr::map(unique(full_log[[ABS_MOD_PATH]]), read_model) %>%
      suppressSpecificWarning("incomplete final line")
    mods_run <- purrr::map(mod_list, bbi_nonmem_model_status)
    sums_df <- summary_log_impl(mod_list) %>% dplyr::mutate(
      status = purrr::map_chr(mod_list, bbi_nonmem_model_status)
    ) %>% dplyr::select("absolute_model_path", all_of(sum_cols))

    full_log <- left_join(full_log, sums_df, by = ABS_MOD_PATH)
    attr_cols <- c(attr_cols, sum_cols)
  }

  # Create Network
  noref <- data.frame(from = NA,
                      to = full_log$run[full_log$based_on==""])
  refs <- data.frame(from = full_log$based_on[full_log$based_on!=""],
                     to = full_log$run[full_log$based_on!=""])
  hierarchy_df <- rbind(noref, refs)

  # Add attributes
  # TODO: add support for multiple based_on
  hierarchy_df <- left_join(hierarchy_df, full_log, by = c("to" = "run"),
                            relationship = "many-to-many") %>%
    dplyr::select(all_of(c("from", "to", "absolute_model_path", attr_cols))) %>%
    dplyr::distinct()

  # Color by star (potentially user-specified variable)
  hierarchy_df$col <- factor(hierarchy_df$star)
  levels(hierarchy_df$col) <- c("#edf8fb","#810f7c")

  # Compile attributes into tooltip
  hierarchy_df$tooltip <- make_tree_tooltip(hierarchy_df)

  # TODO: perform checks regarding whether a tree can be made. If models
  # were deleted, or links otherwise cannot be determined, a tree cannot be made
  # If check fails --> do we filter those out and warn?

  pl_tree <- collapsibleTree::collapsibleTreeNetwork(
    hierarchy_df, zoomable = FALSE, attribute = c("star"),
    fill="col", collapsed = FALSE, nodeSize = "leafCount",
    tooltipHtml = "tooltip")

  if(isTRUE(.append_table)){
    # Format attributes as standalone table if static
    tooltip_df <- hierarchy_df %>% dplyr::select("Model" = "to", all_of(attr_cols))
    html_table <- kableExtra::kbl(tooltip_df, format = "html") %>%
      kableExtra::kable_styling(font_size = 10)
    pl_tree <- manipulateWidget::combineWidgets(pl_tree, html_table)
  }

  if(isTRUE(.static_plot)){
    model_tree_png(pl_tree)
  }else{
    return(pl_tree)
  }
}

#' Create tooltip for interactive [model_tree()]
#'
#' @keywords internal
make_tree_tooltip <- function(.hierarchy_df){

  style_html <- function(txt, color, ...){
    paste0(glue::glue("<span style='color:{color};"), ...,"'>" ,txt,"</span>")
  }

  # Tooltip from run log
  tooltip <- purrr::imap_chr(.hierarchy_df$to, \(.x, .y){
    mod_html <- paste0(
      "<span style='font-size:14px; color:#538b01; font-weight:bold'>", .x,
      "</span><br>")
    desc_html <- ifelse(
      is.na(.hierarchy_df$description[.y]), "",
      paste0("<span style='font-style:italic;'>", .hierarchy_df$description[.y],
             "</span><br>")
    )
    tags_html <- ifelse(
      .hierarchy_df$tags[.y] =="", "",
      paste0("Tags: ", .hierarchy_df$tags[.y], "<br>")
    )
    star_html <- ifelse(
      isTRUE(.hierarchy_df$star[.y]),
      paste0("<span style='color: #87CEEB'>Starred</span><br>"), ""
    )

    paste0(mod_html, desc_html, star_html, tags_html)
  })

  # Tooltip from model summary
  add_summary <- "status" %in% names(.hierarchy_df)
  if(isTRUE(add_summary)){
    sum_tooltip <- purrr::imap_chr(.hierarchy_df$status, \(mod_status, .y){
      if(grepl("Finished", mod_status)){
        paste0(
          "<br>",
          "<span style='color:#538b01; font-weight:bold'>", mod_status,"</span><br>",
          "OFV: ", style_html(.hierarchy_df$ofv[.y],color = "#A30000"), "<br>",
          "N Subjects: ", style_html(.hierarchy_df$number_of_subjects[.y], color = "#A30000"), "<br>",
          "N Obs: ", style_html(.hierarchy_df$number_of_obs[.y], color = "#A30000")
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
  grid::grid.raster(png_file)
}


#' Checks if all packages needed for [model_tree()] are present
#'
#' Returns a vector with the missing packages, or returns NULL if all are
#' present.
#' @keywords internal
check_for_model_tree_pkgs <- function() {
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
#' @keywords internal
skip_if_tree_missing_deps <- function() {
  missing_pkgs <- check_for_model_tree_pkgs()
  testthat::skip_if(
    !is.null(missing_pkgs),
    glue::glue("Skipped because the following packages are needed for this test: {paste(missing_pkgs, collapse = ', ')}")
  )
}

#' Error if missing [model_tree()] dependencies
#' @keywords internal
stop_if_tree_missing_deps <- function() {
  missing_pkgs <- check_for_model_tree_pkgs()
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
