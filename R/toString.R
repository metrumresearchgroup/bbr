# S3 methods for toString generic on bbi objects

#' Collapse `bbi_log_df` list columns to character
#'
#' Collapses a list column in a `bbi_log_df` tibble into a column of character scalars (aka strings).
#' @details
#' Any non-list columns passed to `...` will be ignored and will trigger a warning notifying the user that only list columns can be collapsed.
#'
#' Any list columns passed to `...` which do _not_ contain either character, numeric, or logical vectors (i.e. lists of lists) will be silently ignored.
#'
#' @return
#' Returns the same tibble as `x`, but any list columns named in `...` will be collapsed to a character column, with one scalar value (or `NA`) for each row.
#' See "Details" section for caveats.
#'
#' @examples
#' df <- tibble::tibble(
#'   absolute_model_path   = c(1, 2, 3),
#'   char_list = list(c("foo", "bar"), "baz", c("naw", "dawg")),
#'   num_list  = list(c(23, 34, 45), c(-1, -2, -3), NULL)
#' )
#' class(df) <- c("bbi_log_df", class(df))
#'
#' toString(df, char_list, num_list)
#'
#' @param x Input tibble to modify
#' @param ... <[`tidy-select`][dplyr::dplyr_tidy_select]> One or more unquoted
#'   expressions separated by commas. Variable names can be used as if they
#'   were positions in the data frame, so expressions like `x:y` can
#'   be used to select a range of variables.
#' @param .sep Character scalar to use a separator when collapsing vectors. Defaults to `", "`.
#' @importFrom dplyr mutate mutate_at group_by ungroup select row_number
#' @importFrom tidyselect eval_select
#' @importFrom rlang expr
#' @importFrom purrr map_lgl
#' @importFrom checkmate assert_scalar
#' @export
toString.bbi_log_df <- function(x, ..., .sep = ", ") {
  .data <- x # rename for code clarity
  checkmate::assert_scalar(.sep)

  loc <- tidyselect::eval_select(rlang::expr(c(...)), .data)

  # do we need this? seems like a reasonable safety catch but it's internal... danger...
  loc <- dplyr:::ensure_group_vars(loc, .data, notify = TRUE)

  # warn if passed columns that are not lists
  valid_cols <- map_lgl(loc, ~ inherits(.data[[.x]], "list"))
  if (any(!valid_cols)) {
    bad_cols <- names(valid_cols)[!valid_cols]
    warning(glue("collapse_to_string() only works on list columns. The following columns are not lists and will be ignored: {paste(bad_cols, collapse = ', ')}"))
  }

  # collapse together any lists of vectors
  .data %>%
    group_by(.data[[ABS_MOD_PATH]]) %>%
    mutate_at(.vars = vars({{loc}}),
              function(.vec) {
                if (inherits(.vec, "list")) {
                  if (is.null(.vec[[1]])) {
                    .vec <- NA
                  } else if (inherits(.vec[[1]], c("character", "numeric", "logical"))) {
                    .vec <- paste(unlist(.vec), collapse = .sep)
                  }
                }
                .vec
              }) %>%
    ungroup(.data[[ABS_MOD_PATH]])
}
