#' Create a tree diagram of a modeling directory
#'
#' @inheritParams run_log
#'
#' @export
model_tree <- function(.base_dir,
                       .recurse = FALSE,
                       .include = NULL,
                       .add_summary = TRUE
){
  log_df <- run_log(.base_dir, .recurse, .include) %>%
    suppressSpecificWarning("incomplete final line")

  # Replace NULL based_on elements with NA to preserve rows when unnesting
  full_log <- log_df %>% dplyr::mutate(
    based_on = purrr::map(based_on, \(.x){if(is.null(.x)) "" else .x}),
    tags = purrr::map(tags, \(.x){if(is.null(.x)) "" else paste(.x, collapse = ", ")})
  ) %>% tidyr::unnest(c("based_on", "tags"))


  attr_cols <- c("description","star", "tags")
  # Optionally append model summary information
  if(isTRUE(.add_summary)){
    sum_cols <- c("absolute_model_path", "status", "number_of_subjects",
                  "number_of_obs", "ofv")
    mod_list <- purrr::map(full_log[[ABS_MOD_PATH]], read_model) %>%
      suppressSpecificWarning("incomplete final line")
    mods_run <- purrr::map(mod_list, bbi_nonmem_model_status)
    sums_df <- summary_log_impl(mod_list) %>% dplyr::mutate(
      status = purrr::map_chr(mod_list, bbi_nonmem_model_status)
    ) %>% dplyr::select(all_of(sum_cols))

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
  hierarchy_df <- left_join(hierarchy_df, full_log, by = c("to" = "run")) %>%
    dplyr::select(all_of(c("from", "to", attr_cols)))
  # Color by star (potentially user-specified variable)
  hierarchy_df$col <- factor(hierarchy_df$star)
  levels(hierarchy_df$col) <- c("#edf8fb","#810f7c")
  # Add tooltip
  hierarchy_df$tooltip <- make_tree_tooltip(hierarchy_df)

  # TODO: perform checks regarding whether a tree can be made. If models
  # were deleted, or links otherwise cannot be determined, a tree cannot be made
  # If check fails --> do we filter those out and warn?

  # pl_tree <-
  collapsibleTree::collapsibleTreeNetwork(
    hierarchy_df, zoomable = FALSE, attribute = c("star"),
    fill="col", collapsed = FALSE, nodeSize = "leafCount",
    tooltipHtml = "tooltip")

}

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
      paste0("Tags: ", stringr::str_trunc(.hierarchy_df$tags[.y], 30), "<br>")
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
