#' Create a tree diagram of a modeling directory
#'
#' @param .log_df A `bbi_run_log_df` tibble (the output of `run_log()`) ***or***
#'        a base directory to look in for models. See details for more options.
#' @param include_info A vector of columns present in `.log_df` to include in the
#'        tooltip.
#' @param color_by A run log column to color the nodes by. Can be helpful for
#'        identifying which models are starred, have heuristics, etc. See details
#'        for more information.
#' @param size_by A **numeric** (or integer) run log column to size the nodes by.
#'        If not specified, the default is to size the nodes based on how many
#'        models are based on it.
#' @param add_summary Logical (`TRUE`/`FALSE`). If `TRUE`, include key columns
#'        from [model_summary()] output.
#' @param digits Number of digits to round decimal places to for display in
#'        the tooltip.
#' @param zoomable Logical (`TRUE`/`FALSE`). If `TRUE`, allow pan and zoom by
#'        dragging and scrolling.
#' @param static Logical (`TRUE`/`FALSE`). If `TRUE`, render the plot as a
#'        static image. This takes a little longer, as the interactive plot must
#'        be saved as a PNG and loaded into the viewer.
#' @param width Width in pixels (optional, defaults to automatic sizing)
#' @param height Height in pixels (optional, defaults to automatic sizing)
#' @param font_size Font size of the label text in pixels
#' @param ... Additional arguments passed to [run_log()]. Only used if `.log_df`
#'        is a modeling directory.
#'
#' @section Required Columns:
#'
#' `.log_df` must contain **`absolute_model_path`**, **`run`**, and **`based_on`**
#' columns in order to properly link and label each of the models, where the `based_on`
#' attribute is used to determine the tree network.
#'  - *Additional* `based_on` flags will be shown in the tooltip, using the first
#'  one to create the tree network
#'
#' Any dataframe with the `bbi_run_log_df` class and required columns can be used.
#' In other words, users can add/modify columns of their `run_log()`, and pass these
#' additional columns as tooltips. This is illustrated in the examples via
#' `add_summary()` and `add_config()`.
#'
#' @section Tooltip formatting, coloring, and sizing:
#' **Tooltip formatting**
#'
#' Any column in `.log_df` can be chosen to include in the tooltip. However,
#' certain columns will be formatted *specially* if specified via `include_info`.
#' Any other column will be displayed as verbatim text (no special handling), though
#' column *names* will be formatted slightly.
#'
#' Specially formatted columns (*if specified* via `include_info`):
#'  - `'description'`, `'tags'`, and `'star'`
#'  - When `add_summary = TRUE` these specific summary columns are also
#'  formatted specially:
#'     - `'number_of_subjects'`, `'number_of_obs'`, `'ofv'`, and `'any_heuristics'`.
#'     - Note that the above summary columns will only receive the special
#'     formatting if added via `add_summary = TRUE`.
#'     - i.e. if `.log_df = run_log() %>% add_summary()` and
#'     `include_info = 'ofv'`, the `'OFV'` parameter will display the same as if
#'     it was _not_ passed to `include_info`.
#'
#' **Coloring**
#'
#' Logical columns are handled differently from numeric or character columns.
#' Nodes will be colored `'white'` for `FALSE` and `'red'` for `TRUE`. All other
#' column types will be colored via a gradient between `'white'` and `'red'`,
#' where earlier runs are whiter, and later runs appear to be more red. You can
#' pass `color_by = NULL` to make all model nodes `'red'`.
#'
#'
#' @examples
#' \dontrun{
#'
#' # Basic
#' MODEL_DIR %>% model_tree()
#' run_log(MODEL_DIR) %>% model_tree()
#'
#' # Color by a column
#' model_tree(MODEL_DIR, color_by = "star")
#'
#'
#' ## Run `add_config()`, `add_summary()`, and/or `mutate()` calls beforehand
#'
#'
#' # Size nodes by objective function value
#' run_log(MODEL_DIR) %>% add_summary() %>%
#'   model_tree(size_by = "ofv", color_by = "ofv")
#'
#' # Determine if certain models need to be re-run
#' run_log(MODEL_DIR) %>% add_config() %>%
#'   dplyr::mutate(
#'     out_of_date = model_has_changed | data_has_changed
#'   ) %>%
#'   model_tree(
#'     include_info = c("model_has_changed", "data_has_changed", "nm_version"),
#'     color_by = "out_of_date"
#'   )
#'
#' # Highlight models with any heuristics
#' run_log(MODEL_DIR) %>% add_summary() %>%
#'   model_tree(
#'     include_info = c("param_count", "eta_pval_significant"),
#'     color_by = "any_heuristics"
#'   )
#'
#' }
#'
#' @export
model_tree <- function(
    .log_df,
    include_info = c("description","star", "tags"),
    color_by = "run",
    size_by = NULL,
    add_summary = TRUE,
    digits = 3,
    zoomable = FALSE,
    static = FALSE,
    width = NULL,
    height = NULL,
    font_size = 10,
    ...
){
  UseMethod("model_tree")
}


#' @export
model_tree.character <- function(
    .log_df,
    include_info = c("description","star", "tags"),
    color_by = "run",
    size_by = NULL,
    add_summary = TRUE,
    digits = 3,
    zoomable = FALSE,
    static = FALSE,
    width = NULL,
    height = NULL,
    font_size = 10,
    ...
){
  checkmate::assert_directory_exists(.log_df)
  .log_df <- run_log(.log_df, ...)
  model_tree(
    .log_df,
    include_info = include_info,
    color_by = color_by, size_by = size_by,
    add_summary = add_summary, digits = digits,
    zoomable = zoomable, static = static,
    width = width, height = height,
    font_size = font_size
  )
}

#' @export
model_tree.bbi_log_df <- function(
    .log_df,
    include_info = c("description", "star", "tags"),
    color_by = "run",
    size_by = NULL,
    add_summary = TRUE,
    digits = 3,
    zoomable = FALSE,
    static = FALSE,
    width = NULL,
    height = NULL,
    font_size = 10,
    ...
){
  check_bbi_run_log_df_object(.log_df)
  # Make sure required dependencies are installed
  stop_if_tree_missing_deps(static = static)

  # Make tree data
  tree_data <- make_tree_data(.log_df, include_info, color_by, size_by, add_summary)

  # Format coloring
  tree_data <- color_tree_by(tree_data, color_by = color_by)
  tree_attr <- attributes(tree_data)$color_by

  # Format sizing
  tree_data <- size_tree_by(tree_data, size_by = size_by)
  node_size <- attributes(tree_data)$size_by

  # Compile attributes into tooltip
  tree_data <- make_tree_tooltip(tree_data, digits = digits, font_size = font_size)

  # Create model tree
  # - Notes about aggFun:
  #  - identity is not the same as base::identity. collapsibleTree has specific
  #  handling for `aggFun = identity`, and it's the only way for sizing to work
  #  based on a column without aggregating values.
  #  - Note: The node sizing logic has a known quirk where it appears to scale
  #  sizes relative to the first "parent" node (i.e., the first row where `from`
  #  is not NA). This can cause inconsistencies in relative node sizes depending
  #  on the value of the first parent node.
  pl_tree <- collapsibleTree::collapsibleTreeNetwork(
    tree_data, zoomable = zoomable, collapsed = FALSE,
    # Coloring and sizing
    attribute = tree_attr, fill="col", nodeSize = node_size, aggFun = identity,
    # Tooltip and display
    tooltipHtml = "tooltip", fontSize = font_size,
    width = width, height = height
  )

  if(isTRUE(static)){
    pl_tree <- model_tree_png(pl_tree)
  }

  return(pl_tree)
}


#' Construct dataset for use in `model_tree`
#' @param .log_df a `bbr` run log
#' @inheritParams model_tree
#' @importFrom tidyselect any_of
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
    .log_df,
    include_info = c("description","star", "tags"),
    color_by = "run",
    size_by = NULL,
    add_summary = TRUE
){
  # Check for required columns and starting format
  req_cols <- c(ABS_MOD_PATH, "run", "based_on", "model_type")
  if(!(all(req_cols %in% names(.log_df)))){
    cols_missing <- setdiff(req_cols, names(.log_df))
    cli::cli_abort(
      "The following {.emph required} columns are missing from `.log_df`: {.val {cols_missing}}"
    )
  }
  checkmate::assert_true(inherits(.log_df$based_on, "list"))

  cols_keep <- unique(c(include_info, color_by, size_by))
  if(!all(cols_keep %in% names(.log_df))){
    cols_missing <- setdiff(cols_keep, names(.log_df))
    cli::cli_abort(
      "The following {.emph specified} columns are missing from `.log_df`: {.val {cols_missing}}"
    )
  }

  # These columns have special handling either here or in the tooltip
  base_log_cols <- c(req_cols, "description", "star", "tags")

  # Starting run log
  log_cols <- unique(c(base_log_cols, cols_keep))
  .log_df <- .log_df %>% dplyr::select(any_of(log_cols))
  # unnest based_on column
  full_log <- unnest_based_on(.log_df)

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
    full_log <- full_log %>% dplyr::mutate(tags = stringr::str_trunc(.data$tags, 30))
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
    base_log_cols <- c(base_log_cols, "addl_based_on")
  }

  # Add run log classes back to full_log to use add_summary().
  # - run log classes are removed when unnesting columns
  class(full_log) <- c("bbi_run_log_df", "bbi_log_df", class(full_log))

  # Add model status for tooltip display
  full_log <- full_log %>% add_model_status()

  # Optionally append model summary information
  # This must be done after filtering out any duplicate rows
  if(isTRUE(add_summary)){
    sum_cols <- c("number_of_subjects", "number_of_obs", "ofv", "any_heuristics")
    # We shouldn't just apply add_summary() to full_log, as this could introduce
    # column naming issues if users passed a *summary* column to `include_info`
    # i.e. model_tree(run_log(model_dir) %>% add_summary(), include_info = c("problem_text", "ofv"))
    #  - Remove any specially handled columns prior to running
    #  - Only join the _new_ select summary columns back
    sum_log <- full_log %>% dplyr::select(-any_of(sum_cols)) %>% add_summary() %>%
      dplyr::select(all_of(c(req_cols, sum_cols)))
    if(any(sum_cols %in% names(full_log))){
      existing_sum_cols <- sum_cols[sum_cols %in% names(full_log)]
      sum_log <- sum_log %>% dplyr::select(-all_of(existing_sum_cols))
    }
    full_log <- dplyr::left_join(full_log, sum_log, by = req_cols)
  }

  # Create model network and append to full run log
  tree_data <- make_model_network(full_log)

  # Store columns to be used in tooltip as attributes
  attr(tree_data, "base_tt_cols") <- base_log_cols[base_log_cols %in% include_info]
  if(isTRUE(add_summary)){
    attr(tree_data, "sum_tt_cols") <- sum_cols
    attr(tree_data, "other_tt_cols") <- setdiff(include_info, c(base_log_cols, sum_cols))
  }else{
    attr(tree_data, "other_tt_cols") <- setdiff(include_info, base_log_cols)
  }

  return(tree_data)
}

#' Unnest the based_on column from a run_log
#' @param .log_df a `bbr` run log
#' @noRd
unnest_based_on <- function(.log_df){
  .log_df %>% dplyr::mutate(
    # Replace NULL based_on elements with empty string to preserve rows when unnesting
    based_on = purrr::map(.data$based_on, function(.x){if(is.null(.x)) "" else .x}),
  ) %>% tidyr::unnest("based_on")
}

#' Make model network for use in `model_tree`
#' @param full_log full log including model run and summary information. Assumes
#'  the `based_on` column is unnested.
#' @noRd
make_model_network <- function(full_log){
  req_cols <- c(ABS_MOD_PATH, "run", "based_on", "status")
  checkmate::assert_true(all(req_cols %in% names(full_log)))
  checkmate::assert_character(full_log$based_on)

  # Check if log was recursive, and append relevant columns
  full_log <- add_log_recurse_dirs(full_log)
  if(any(full_log$is_sub_dir)){
    full_log2 <- full_log
    sub_dir_rows <- which(full_log2$is_sub_dir)
    # Handle recursive run logs
    for(i in 1:seq_along(sub_dir_rows)){
      rn <- sub_dir_rows[i]
      # Make `based_on` path just the model name, and add the relative subdirectory
      # to the run name. This is necessary for linking nested models.
      full_log2$run[rn] <- file.path(full_log$sub_dir_name[rn], basename(full_log$based_on[rn]))
      full_log2$based_on[rn] <- basename(full_log$based_on[rn])
    }
    full_log <- full_log2
  }

  # Truncate model directory(ies) for tooltip display
  model_dirs <- unique(full_log$model_dir) %>%
    stringr::str_trunc(40, side = "left")

  # Create Network
  start <- "Start"
  noref_df <- data.frame(from = NA, to = start)

  child_mods_df <- data.frame(
    from = full_log$based_on[full_log$based_on!=""],
    to = full_log$run[full_log$based_on!=""]
  )

  parent_mods <- full_log$run[full_log$based_on==""]
  if(!rlang::is_empty(parent_mods)){
    parent_mods_df <- data.frame(from = start, to = parent_mods)
    network_df <- rbind(noref_df, parent_mods_df, child_mods_df)
  }else{
    # This happens when there is no "starting" or origin model
    # - i.e. the first referenced based_on model no longer exists
    network_df <- rbind(noref_df, child_mods_df)
  }

  # Check network links
  # - set unlinked models as starting points and warn that they're missing
  network_df <- check_model_tree(network_df)

  # Join network to run log
  recurse_cols <- c("par_dir", "is_sub_dir", "sub_dir_name")
  tree_data <- dplyr::left_join(network_df, full_log, by = c("to" = "run")) %>%
    dplyr::mutate(run = ifelse(.data$to == "Start", NA, .data$to)) %>%
    dplyr::relocate(all_of(c("from", "to", "run"))) %>%
    dplyr::select(-all_of(recurse_cols))

  # Adjust status for any unlinked (missing) models
  tree_data <- tree_data %>% dplyr::mutate(
    status = ifelse(
      is.na(.data$status) & .data$to != "Start", "Not Found", .data$status
    )
  )

  # Set status to be modeling directory for start node
  start_txt <- if(length(model_dirs) == 1){
    paste0("Model Directory:<br>", model_dirs)
  }else if(length(model_dirs) > 1){
    paste0("Model Directories:<br>", paste(model_dirs, collapse = ",<br>"))
  }
  tree_data$status[tree_data$to=="Start"] <- start_txt

  return(tibble::as_tibble(tree_data))
}


#' Determine if `.recurse` was set to `TRUE` when calling bbr::run_log(), and
#' append columns linking sub directories to their parent if so.
#' @param .log_df a `bbr` run log
#' @noRd
add_log_recurse_dirs <- function(.log_df){
  checkmate::assert_true(all(c(ABS_MOD_PATH, "based_on") %in% names(.log_df)))
  checkmate::assert_character(.log_df$based_on)

  # Add modeling directory to log for later joining
  .log_df$model_dir <- dirname(.log_df[[ABS_MOD_PATH]]) %>% fs::path_rel() %>%
    as.character()
  model_dirs <- unique(.log_df$model_dir)

  # Set whether .recurse was set when calling bbr::run_log()
  # This doesn't necessarily guarantee a path is a subdirectory of another
  # E.g., if data manipulation/filtering was done ahead of time
  .recurse <- any(grepl("\\Q..\\E", .log_df$based_on))

  is_sub_path <- function(x, dir, n = nchar(dir)){
    (substr(x, 1, n) == dir) && (x != dir)
  }

  if(length(model_dirs) > 1){
    # Check all combinations for subdirectories
    dir_combs <- expand.grid(dir = model_dirs, test = model_dirs, stringsAsFactors = FALSE)
    dir_combs$is_sub_dir <- purrr::map2_lgl(dir_combs$dir, dir_combs$test, is_sub_path)

    if(any(dir_combs$is_sub_dir) && .recurse){
      sub_dir_combs <- dir_combs[dir_combs$is_sub_dir,] %>%
        dplyr::rename("sub_dir" = "dir", "par_dir" = "test")
      sub_dir_combs$sub_dir_name <- purrr::map2_chr(
        sub_dir_combs$sub_dir, sub_dir_combs$par_dir, fs::path_rel
      ) %>% unique()

      .log_df <- .log_df %>% dplyr::left_join(
        sub_dir_combs, by = c("model_dir" = "sub_dir")
      ) %>% dplyr::mutate(
        is_sub_dir = ifelse(is.na(.data$is_sub_dir), FALSE, .data$is_sub_dir)
      )
    }else{
      .log_df <- .log_df %>% dplyr::mutate(
        is_sub_dir = FALSE, par_dir = NA_character_, sub_dir_name = NA_character_
      )
    }
  }else{
    .log_df <- .log_df %>% dplyr::mutate(
      is_sub_dir = FALSE, par_dir = NA_character_, sub_dir_name = NA_character_
    )
  }

  return(.log_df)
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

    cli::cli_warn(c(
      "The following models could not be linked properly: {.val {unlinked_roots}}",
      "i" = "Setting these as starting models in tree diagram.",
      "i" = paste("Check the yaml files or run {.func bbr::run_log()} to make sure",
                  "all {.var based_on} models still exist")
    ))
  }

  return(network_df)
}

#' Create tooltip for interactive [model_tree()]
#' @param tree_data a combined dataframe that includes run log columns and
#'  defines the model network.
#' @inheritParams model_tree
#' @noRd
make_tree_tooltip <- function(tree_data, digits = 3, font_size = 10){

  round_numeric <- function(x, digits){
    # Round instead of signif - this can matter for objective functions
    if(inherits(x, "numeric")) round(x, digits) else x
  }


  # Discern whether or not to display cell text for some columns
  #  - This is only used for run_log columns that we may not want to display
  #.    (such as an empty description or no tags)
  #  - The key summary columns will be displayed as long as the model has been
  #     executed (i.e. NA values will still be displayed for these columns)
  can_include <- function(txt) !is.na(txt) && txt != ""

  bold_css <- "font-weight:bold;"
  italics_css <- "font-style:italic;"
  run_font_size <- font_size + 4
  run_css <- glue::glue("font-size:{run_font_size}px; {bold_css}")

  # Tooltip from run log
  base_tt_cols <- attr(tree_data, "base_tt_cols")
  tooltip <- purrr::imap_chr(tree_data$to, function(.x, .y){
    mod_name <- ifelse(.x == "Start", .x, paste("Run", .x))
    mod_html <- style_html(
      mod_name, color = "#538b01", run_css, br_after = TRUE
    )

    # Model type
    mod_type <- ifelse(
      .x == "Start", .x,
      format_model_type(tree_data$model_type[.y], fmt_html = TRUE, br_after = TRUE)
    )

    # Additional based_on flags: NA for models with only one based_on flag
    based_on_addl_html <- ifelse(
      can_include(tree_data$addl_based_on[.y]),
      style_html(
        paste("Additional Based on:", tree_data$addl_based_on[.y]),
        bold_css, br_after = TRUE
      ),
      ""
    )

    # Other parameters
    desc_html <- ifelse(
      "description" %in% base_tt_cols && can_include(tree_data$description[.y]),
      style_html(tree_data$description[.y], italics_css, br_after = TRUE),
      ""
    )
    tags_html <- ifelse(
      "tags" %in% base_tt_cols && can_include(tree_data$tags[.y]),
      paste0("Tags: ", style_html(tree_data$tags[.y], color = "#297f9c", br_after = TRUE)),
      ""
    )
    star_html <- ifelse(
      "star" %in% base_tt_cols && can_include(tree_data$star[.y]) && isTRUE(tree_data$star[.y]),
      style_html("Starred", color = "#ffa502", bold_css, br_after = TRUE),
      ""
    )

    paste0(mod_html, mod_type, based_on_addl_html, desc_html, star_html, tags_html)
  })

  # Tooltip from model summary
  sum_cols <- attr(tree_data, "sum_tt_cols")
  add_summary <- !is.null(sum_cols) && all(sum_cols %in% names(tree_data))
  sum_tooltip <- purrr::imap_chr(tree_data$status, function(mod_status, .y){
    status_col <- ifelse(grepl("Finished", mod_status), "#538b01", "#A30000")
    # Add key summary columns if the model has been run (even if not successfully)
    if(isTRUE(add_summary) && grepl("Finished|Incomplete", mod_status)){
      # Conditional heuristics text
      any_heuristics <- tree_data$any_heuristics[.y]
      heuristics_txt <- if(!is.na(any_heuristics) && isTRUE(any_heuristics)){
        paste0("<br><br>", style_html("--Heuristics Found--", color = "#A30000", bold_css))
      }else{
        ""
      }
      # Conditional simulation text
      has_sim_txt <- if(has_simulation(read_model(tree_data[[ABS_MOD_PATH]][.y]))){
        paste0("<br><br>", style_html("--Simulation attached--", color = "#ad7fa8", bold_css))
      }else{
        ""
      }
      # Numeric values
      ofv <- round_numeric(tree_data$ofv[.y], digits = digits)
      n_sub <- round_numeric(tree_data$number_of_subjects[.y], digits = digits)
      n_obs <- round_numeric(tree_data$number_of_obs[.y], digits = digits)
      # Format
      ofv <- ifelse(
        can_include(ofv),
        paste0("OFV: ", style_html(ofv, color = "#A30000", br_after = TRUE)),
        ""
      )
      n_sub <- ifelse(
        can_include(n_sub),
        paste0("N Subjects: ", style_html(n_sub, color = "#A30000", br_after = TRUE)),
        ""
      )
      n_obs <- ifelse(
        can_include(n_obs),
        paste0("N Obs: ", style_html(n_obs, color = "#A30000")),
        ""
      )
      # Combined tooltip
      paste0(
        style_html(mod_status, color = status_col, bold_css, br_before = TRUE, br_after = TRUE),
        ofv, n_sub, n_obs, heuristics_txt, has_sim_txt
      )
    }else{
      # If not run, just show the status
      style_html(mod_status, color = status_col, bold_css, br_before = TRUE)
    }
  })

  # The above tooltips have special formatting and conditional logic for displaying
  #  - Other columns requested will display as verbatim text between run_log
  #     and summary tooltips, and will be included regardless of the value (NA
  #     values and empty cells will still be displayed).
  other_cols <- attr(tree_data, "other_tt_cols")
  if(!is.null(other_cols)){
    other_tooltip <- purrr::imap_chr(tree_data$to, function(.x, .y){
      other_html <- purrr::map_chr(other_cols, function(col){
        col_vals <- round_numeric(tree_data[[col]][.y], digits = digits)
        col_lbl <- paste0(stringr::str_to_title(gsub("_", " ", col)), ":")
        col_html <- paste(col_lbl, style_html(col_vals, color = "#297f9c", br_after = TRUE))
      })
      paste0(other_html, collapse = "")
    })
    other_tooltip[1] <- "" # Skip start node for other tooltips
    tooltip <- paste0(tooltip, other_tooltip, sum_tooltip)
  }else{
    tooltip <- paste0(tooltip, sum_tooltip)
  }

  tree_data$tooltip <- tooltip
  return(tree_data)
}


#' Create a color column based on the unique values of another column.
#' @inheritParams make_tree_tooltip
#' @inheritParams model_tree
#' @noRd
color_tree_by <- function(tree_data, color_by = "run"){
  # white and colors in bbr's logo (red and orange)
  # #f0cfc5 is a gradient color between bbr orange and white
  bbr_cols <- c("#ffffff","#f0cfc5", "#e06a46", "#eb003d")

  # Initialize new color column
  if(!is.null(color_by)){
    checkmate::assert_true(color_by %in% names(tree_data))
    tree_data$col <- tree_data[[color_by]]
  }else{
    tree_data$col <- 1
  }

  # Set start node color to green
  tree_data$col[tree_data$to == "Start"] <- "#007319"
  # This variable is only used for gradient coloring
  node_colors <- "#007319"

  # Set NA values to grey if using color_by
  na_vals <- is.na(tree_data$col) & tree_data$to != "Start"
  if(any(na_vals)){
    tree_data$col[na_vals] <- "#C0C0C0"
    node_colors <- c(node_colors, "#C0C0C0")
  }

  if(!is.null(color_by)){
    # Get non-NA values (NA values are colored separately)
    vals <- tree_data[[color_by]][!is.na(tree_data[[color_by]])]
    n_levels <- dplyr::n_distinct(vals)

    # To preview color palette: scales::show_col(pal_bbr)
    if(inherits(vals, c("numeric", "integer"))){
      # Gradient coloring (sorted): get colors for unique values (excluding NA)
      sorted_vals <- sort(unique(vals))
      pal_bbr <- scales::pal_gradient_n(bbr_cols)(seq(0, 1, length.out = n_levels))

      # Assign colors based on sorted values
      color_mapping <- setNames(pal_bbr, sorted_vals)
      tree_data$col <- ifelse(
        tree_data$col %in% sorted_vals,
        color_mapping[as.character(tree_data$col)],
        tree_data$col
      )
    }else if(inherits(vals, "logical")){
      # Ensure both TRUE and FALSE colors are extracted, even if only one occurs
      pal_bbr <- scales::pal_gradient_n(bbr_cols)(c(0,1))
      # Explicitly set FALSE to white and TRUE to red
      # (dont overwrite start node and NA values)
      tree_data <- tree_data %>% dplyr::mutate(
        col = dplyr::case_when(
          !!rlang::sym(color_by) == FALSE ~ pal_bbr[1],
          !!rlang::sym(color_by) == TRUE ~  pal_bbr[2],
          TRUE ~ col
        )
      )
    }else{
      # Gradient coloring; doesn't need to be sorted
      pal_bbr <- scales::pal_gradient_n(bbr_cols)(seq(0, 1, length.out = n_levels))
      tree_data$col <- factor(tree_data$col)
      levels(tree_data$col) <- c(node_colors, pal_bbr)
    }
    # Set color_by attribute to color_by argument
    attr(tree_data, "color_by") <- color_by
  }else{
    # all bbr red color
    pal_bbr <- scales::pal_gradient_n(bbr_cols)(1)
    tree_data$col <- factor(tree_data$col)
    levels(tree_data$col) <- c(node_colors, pal_bbr)
    # Set color_by attribute to leafCount (default)
    attr(tree_data, "color_by") <- "leafCount"
  }

  return(tree_data)
}

#' Create a size column based on the unique _numeric_ (or integer) values of
#' another column.
#' @inheritParams make_tree_tooltip
#' @inheritParams model_tree
#' @param rescale_to A numeric vector of length 2 specifying the range to rescale
#'  `size_by` values to. Defaults to `c(1, 3)`, where `1` is the smallest node
#'  size and `3` is the largest.
#' @noRd
size_tree_by <- function(tree_data, size_by = NULL, rescale_to = c(1, 3)){
  if(!is.null(size_by)){
    checkmate::assert_true(size_by %in% names(tree_data))
    checkmate::assert_numeric(rescale_to, len = 2, lower = 1)

    tree_data$node_size <- tree_data[[size_by]]

    # Scale size with numeric value
    if(inherits(tree_data$node_size, c("numeric", "integer"))){

      # Rescale to specified range
      # - node sizes must be greater than 0 (decimals are fine)
      # - A large SD can lead to large nodes (max of rescale_to should be small)
      tree_data <- tree_data %>% dplyr::mutate(
        node_size = scales::rescale(.data$node_size, to = rescale_to)
      )

      # Set node sizes with NA values (including start node) to mean rescale_to
      tree_data$node_size[is.na(tree_data$node_size)] <- mean(rescale_to)

      # Set size_by attribute to node_size column
      attr(tree_data, "size_by") <- "node_size"
    }else{
      col_class <- class(tree_data[[size_by]])
      cli::cli_warn(
        c(
          "Only numeric columns are supported. Column {.val {size_by}} is {.val {col_class}}",
          "i" = "Setting node size to Default"
        )
      )
      attr(tree_data, "size_by") <- "leafCount"
    }
  }else{
    # Set size_by attribute to leafCount (default)
    attr(tree_data, "size_by") <- "leafCount"
  }

  return(tree_data)
}

#' Helper for coloring text and applying other styles
#' @param txt text to format
#' @param color color added to the text
#' @param ... Other `HTML` styling to add to the `span` call
#' @param br_before Logical (`T`/`F`). If `TRUE`, add a new line before the text
#' @param br_after Logical (`T`/`F`). If `TRUE`, add a new line after the text
#' @keywords internal
style_html <- function(txt, color = "black", ..., br_before = FALSE, br_after = FALSE){
  txt <- paste0(glue::glue("<span style='color:{color};"), ...,"'>" ,txt,"</span>")
  if(isTRUE(br_before)) txt <- paste0("<br>", txt)
  if(isTRUE(br_after)) txt <- paste0(txt, "<br>")
  return(txt)
}

#' Format the model type for use in [model_tree()]
#'
#' @param model_type one of the `bbr` supported model types (`nonmem`, `nboot`,
#' `nsim`). Other model types are also acceptable, but won't have custom formatting.
#' @param fmt_html Logical (`T`/`F`). If `TRUE`, apply coloring and format as `HTML`.
#' @param ... Additional arguments passed to [style_html()].
#' @keywords internal
format_model_type <- function(model_type, fmt_html = FALSE, ...){
  checkmate::assert_character(model_type, len = 1)

  mod_type_fmt <- dplyr::case_when(
    model_type == "nonmem" ~ "NONMEM Model",
    model_type == "nmboot" ~ "Bootstrap Run",
    model_type == "nmsse" ~ "SSE Run",
    model_type == "nmsim" ~ "Simulation",
    TRUE ~ paste(toupper(model_type), "Model")
  )

  if(isTRUE(fmt_html)){
    bold_css <- "font-weight:bold;"
    mod_type_fmt <- dplyr::case_when(
      model_type == "nonmem" ~
        style_html(mod_type_fmt, color = "#119a9c", bold_css, ...),
      model_type == "nmboot" ~
        style_html(mod_type_fmt, color = "#c49f02", bold_css, ...),
      model_type == "nmsse" ~
        style_html(mod_type_fmt, color = "#c49f02", bold_css, ...),
      model_type == "nmsim" ~
        style_html(mod_type_fmt, color = "#ad7fa8", bold_css, ...),
      TRUE ~
        style_html(mod_type_fmt, color = "black", bold_css, ...)
    )
  }

  return(mod_type_fmt)
}

#' Save an interactive [model_tree()] as a PNG and load in Rstudio viewer
#'
#' @details
#' This function first saves and renders widget as HTML. It then screenshots
#' the webpage and saves the file to a temporary PNG. The PNG is loaded in as
#' an array and all data is re-packaged to be print like a `ggplot` object.
#'
#' @param widget an `HTML` widget
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
  png_array <- png::readPNG(temp_png)

  p <- structure(list(
    data = widget$x$data,
    options = widget$x$options,
    png_array = png_array
  ), class = c("model_tree_static", "list"))

  return(p)
}



#' Required packages for running [model_tree()]
#'
#' @param static Logical (`TRUE`/`FALSE`). If `TRUE`, check for additional
#'  required packages for rendering the plot as a PNG.
#' @details
#' The packages below are required for both interactive and static viewing:
#'  - **`collapsibleTree`** for the core plot
#'  - **`scales`** for coloring the tree nodes. Should be installed with `ggplot2`,
#'  which has been a `bbr` suggested package.
#'
#' The packages below are all used for rendering the model tree as a **static** image:
#'  - **`htmlwidgets`** is a dependency of `collapsibleTree`, so it's a 'free'
#'  import. It's used to save the tree diagram out as an `HTML` file.
#'  - **`grid`** is a built-in R package and is used to display the PNG in the
#'  Rstudio viewer.
#'  - **`webshot`** is needed for taking a screenshot of the rendered HTML,
#'  saved out via `htmlwidgets::saveWidget()`.
#'  - **`png`** is needed for reading in a PNG file as an array, in order to plot
#'   it in the Rstudio viewer.
#'
#' @keywords internal
req_tree_pkgs <- function(static = FALSE){
  req_pkgs <- c("collapsibleTree", "1.3.0" = "scales")
  if(isTRUE(static)){
    req_pkgs <- c(req_pkgs, "htmlwidgets", "webshot", "png", "grid")
  }
  return(req_pkgs)
}

#' @describeIn req_tree_pkgs Checks if all packages needed for [model_tree()] are present
#' @return Returns a vector with the missing packages, or returns NULL if all are
#' present.
check_for_model_tree_pkgs <- function(static = FALSE) {
  REQUIRED_TREE_PKGS <- req_tree_pkgs(static = static)
  pkg_names <- paste(unname(REQUIRED_TREE_PKGS), names(REQUIRED_TREE_PKGS), sep = " ")
  pkgs_present <- purrr::imap_lgl(REQUIRED_TREE_PKGS, function(.pkg, .ver) {
    pkg_installed <- requireNamespace(.pkg, quietly = TRUE)
    # Check required versions
    ver_installed <- if(isTRUE(pkg_installed)){
      if(.ver != ""){
        utils::packageVersion(.pkg) >= package_version(.ver)
      }else{
        TRUE
      }
    }else{
      FALSE
    }
    return(pkg_installed && ver_installed)
  }) %>% stats::setNames(pkg_names)

  if (any(!pkgs_present)) {
    return(names(pkgs_present)[!pkgs_present])
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

  # phantomjs is needed for webshot
  if(isTRUE(static) && is.null(missing_pkgs)){
    testthat::skip_if(
      isFALSE(webshot::is_phantomjs_installed()),
      "Skipped because `phantomjs` is needed for this test. Run `webshot::install_phantomjs()` to execute this test"
    )
  }
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

