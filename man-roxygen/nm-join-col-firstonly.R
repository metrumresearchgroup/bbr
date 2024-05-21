#' @section Join column:
#'
#'   * The one exception to this are `FIRSTONLY` tables. If a table file has the
#'     same number of rows as the there are individuals in the input data set
#'     (accounting for any filtering of data in the NONMEM control stream), it
#'     will assumed to be a `FIRSTONLY` table. In this case, the table will be
#'     joined to the input data by the `ID` column. If `ID` is not present in
#'     the table, it will be using `.join_col`. Note that if _neither_ `ID` or
#'     the column passed to `.join_col` are present in the table, the join will
#'     fail.
