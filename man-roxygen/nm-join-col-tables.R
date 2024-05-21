#' @section Join column:
#'
#' Note also that, when `.join_col` is carried into table outputs, **there is no
#' need to table any other columns from the input data** as long as the
#' `nm_join()` approach is used; any column in the input data set, regardless
#' of whether it is listed in `$INPUT` or not, will be carried through from the
#' input data and therefore available in the joined result.
#'
#' **Duplicate Rows Warning for Join Column**
#'
#' If there are duplicate rows found in the specified `.join_col`, a warning
#' will be raised specifying a subset of the repeated rows. Duplicates may be
#' caused by lack of output width. `FORMAT` may be need to be stated in control
#' stream to have sufficient width to avoid truncating `.join_col`.
