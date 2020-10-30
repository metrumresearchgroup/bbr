#' Collapse list column to a character column
#'
#' Collapses list columns in a tibble into columns of character scalars.
#'
#' @details Any non-list columns passed to `...` will be ignored and will
#' trigger a warning notifying the user that only list columns can be collapsed.
#'
#' @return Returns the same tibble as `.data`, but any list columns named in
#' `...` will be collapsed to a character column, with a character scalar (or
#' `NA`) for each row.
#'
#' Cells containing either `character`, `numeric`, or `logical` vectors will
#' be collapsed to a single string with `paste0(collapse = .sep)`.
#'
#' Cells containing _anything else_ (i.e. lists, nested tibbles, etc.) will
#' be converted to a string representation of that object via [dput()].
#'
#' @examples
#' df <- tibble::tibble(
#'   row_num   = c(1, 2, 3),
#'   char_list = list(c("foo", "bar"), "baz", c("naw", "dawg")),
#'   num_list  = list(c(23, 34, 45), c(-1, -2, -3), NULL),
#'   list_list  = list(list(a=1, b=2, c=3), NULL, list(z=-1, y=-2, x=-3))
#' )
#'
#' collapse_to_string(df, char_list, num_list, list_list)
#'
#' @param .data Input tibble to modify
#' @param ... One or more unquoted expressions separated by commas (in the style
#'   of [dplyr::select()], etc.). Variable names can be used as if they were positions
#'   in the data frame, so expressions like `x:y` can be used to select a range
#'   of variables.
#' @param .sep Character scalar to use a separator when collapsing vectors.
#'   Defaults to `", "`.
#' @importFrom dplyr mutate mutate_at select
#' @importFrom tidyselect eval_select
#' @importFrom rlang expr
#' @importFrom purrr map_lgl modify_at map_chr
#' @importFrom checkmate assert_scalar
#' @importFrom utils capture.output
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
    modify_at(.at = cols, .f = function(x) {
      map_chr(x, .f = function(.vec) {
        if (inherits(.vec, c("character", "numeric", "logical"))) {
          .vec <- paste0(.vec, collapse = .sep)
        } else if (is.null(.vec)) {
          .vec <- NA_character_
        } else {
          .vec <- paste(
            capture.output(dput(.vec)),
            collapse = ""
          )
        }
        return(.vec)
      })
    })
}
