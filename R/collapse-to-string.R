#' Collapse list column to a character column
#'
#' Collapses list columns in a tibble into columns of character scalars.
#'
#' @details Any non-list columns passed to `...` will be ignored and will
#' trigger a warning notifying the user that only list columns can be collapsed.
#'
#' Any list columns passed to `...` which do _not_ contain either character,
#' numeric, or logical vectors (i.e. lists of lists) will be silently ignored.
#'
#' @return Returns the same tibble as `.data`, but any list columns named in
#' `...` will be collapsed to a character column, with one scalar value (or
#' `NA`) for each row. See "Details" section for caveats.
#'
#' @examples
#' df <- tibble::tibble(
#'   row_num   = c(1, 2, 3),
#'   char_list = list(c("foo", "bar"), "baz", c("naw", "dawg")),
#'   num_list  = list(c(23, 34, 45), c(-1, -2, -3), NULL)
#' )
#'
#' collapse_to_string(df, char_list, num_list)
#'
#' @param .data Input tibble to modify
#' @param ... One or more unquoted expressions separated by commas (in the style
#'   of [dplyr::select()], etc.). Variable names can be used as if they were positions
#'   in the data frame, so expressions like `x:y` can be used to select a range
#'   of variables.
#' @param .sep Character scalar to use a separator when collapsing vectors.
#'   Defaults to `", "`.
#' @importFrom dplyr mutate mutate_at group_by ungroup select row_number
#' @importFrom tidyselect eval_select
#' @importFrom rlang expr
#' @importFrom purrr map_lgl
#' @importFrom checkmate assert_scalar
#' @export
collapse_to_string <- function(.data, ..., .sep = ", ") {
  checkmate::assert_scalar(.sep)

  cols <- tidyselect::eval_select(rlang::expr(c(...)), .data)

  # subset to only list cols and warn if passed any columns that are not lists
  valid_cols <- map_lgl(cols, ~ inherits(.data[[.x]], "list"))
  if (any(!valid_cols)) {
    bad_cols <- names(valid_cols)[!valid_cols]
    warning(glue("collapse_to_string() only works on list columns. The following columns are not lists and will be ignored: {paste(bad_cols, collapse = ', ')}"))
  }
  cols <- cols[valid_cols]

  # collapse together any lists of vectors
  .data %>%
    mutate(.collapse_key = row_number()) %>%
    group_by(.data[[".collapse_key"]]) %>%
    mutate_at(.vars = vars({{cols}}),
              function(.vec) {
                if (is.null(.vec[[1]])) {
                  .vec <- NA_character_
                } else if (inherits(.vec[[1]], c("character", "numeric", "logical"))) {
                  .vec <- paste(unlist(.vec), collapse = .sep)
                }
                .vec
              }) %>%
    ungroup(.data[[".collapse_key"]]) %>%
    select(-.data[[".collapse_key"]])
}
