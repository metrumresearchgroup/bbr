#' @section Join column:
#'
#' The `.join_col` is the name of a single column that should appear in both the
#' input data set and any tables you want to join. We recommend you make this
#' column a simple integer numbering the rows in the input data set (for example
#' `NUM`). When this column is carried into the output table files, there will
#' be unambiguous matching from the table file back to the input data set.
#'
#'   * The one exception to this are `FIRSTONLY` tables. If a table file has the
#'     same number of rows as the there are individuals in the input data set
#'     (accounting for any filtering of data in the NONMEM control stream), it
#'     will assumed to be a `FIRSTONLY` table. In this case, the table will be
#'     joined to the input data by the `ID` column. If `ID` is not present in
#'     the table, it will be using `.join_col`. Note that if _neither_ `ID` or
#'     the column passed to `.join_col` are present in the table, the join will
#'     fail.
#'
#' Note also that, when `.join_col` is carried into table outputs, **there is no
#' need to table any other columns from the input data** as long as the
#' `nm_join()` approach is used; any column in the input data set, regardless
#' of whether it is listed in `$INPUT` or not, will be carried through from the
#' input data and therefore available in the joined result.

