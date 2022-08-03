#' Return a single data frame with model output and input data
#'
#' For NONMEM models, when a unique row identifier (e.g an integer numbering the
#' rows) is included in the input data set (i.e. the file in `$DATA`) and
#' carried into each table output, `nm_join()` can read in all output table
#' files and join back to the input data set. By default, the input data is
#' joined to the table files so that the number of rows in the result will match
#' the number of rows in the table files (i.e. the number of rows _not_ bypassed via
#' `$IGNORE`). Use the `.superset` argument to join table outputs to the
#' (complete) input data set. This function will print the number of rows and
#' columns when each file is loaded, as well as some information about the
#' joins. This **printing can be suppressed** by setting `options(bbr.verbose =
#' FALSE)`.
#'
#' @inheritParams nm_tables
#' @param .join_col Character column name to use to join table files. Defaults to
#'   `NUM`. See Details.
#' @param .superset If `FALSE`, the default, the data will be joined to the
#'   NONMEM output and if `TRUE`, the NONMEM output will be joined to the data;
#'   that is, if you use `.superset`, you will get the same number of rows as
#'   you have in the input data and NONMEM output columns like `PRED` and
#'   `CWRES` will be filled with `NA`.
#' @param .bbi_args Named list passed to `model_summary(.bbi_args)`. See
#'   [print_bbi_args()] for valid options. Defaults to `list(no_grd_file = TRUE,
#'   no_shk_file = TRUE)` because [model_summary()] is only called internally to
#'   extract the number of records and individuals, so those files are
#'   irrelevant.
#'
#' @details
#'
#' **Join column**
#'
#' The `.join_col` is the name of a single column that should appear in both the
#' input data set and any tables you want to join. We recommend you make this
#' column a simple integer numbering the rows in the input data set (for example
#' `NUM`). When this column is carried into the output table files, there will
#' be unambiguous matching from the table file back to the input data set.
#'
#' The one exception to this are `FIRSTONLY` tables. If a table file has the
#' same number of rows as the there are individuals in the input data set
#' (accounting for any filtering of data in the NONMEM control stream), it will
#' assumed to be a `FIRSTONLY` table. In this case, the table will be joined to
#' the input data by the `ID` column. If `ID` is not present in the table, it
#' will be using `.join_col`. Note that if _neither_ `ID` or the column passed
#' to `.join_col` are present in the table, the join will fail.
#'
#' Note also that, when `.join_col` is carried into table outputs, **there is no
#' need to table any other columns from the input data** as long as the
#' `nm_join()` approach is used; any column in the input data set, regardless
#' of whether it is listed in `$INPUT` or not, will be carried through from the
#' input data and therefore available in the joined result.
#'
#' **Duplicate columns are dropped**
#'
#' If a table has columns with the same name as columns in the input data set,
#' or a table that has already been joined, those columns will be dropped from
#' the joined data. If `getOption(bbr.verbose) == TRUE` a message will be
#' printed about any columns dropped this way.
#'
#' The one exception to this is the `DV` column. If `DV` is present in the input
#' data _and_ at least one of the table files, the `DV` column from the input
#' data will be renamed to `DV.DATA` and the column from the table file kept as
#' `DV`.
#'
#' **Multiple tables per file incompatibility**
#'
#' Because `nm_tables()` calls [nm_file()] internally, it is _not_ compatible
#' with multiple tables written to a single file. See "Details" in [nm_file()]
#' for alternatives.
#'
#' @importFrom dplyr left_join select
#' @importFrom checkmate assert_string assert_character assert_logical assert_list
#' @seealso [nm_tables()], [nm_table_files()], [nm_file()]
#' @export
nm_join <- function(
  .mod,
  .join_col = "NUM",
  .files = nm_table_files(.mod),
  .superset = FALSE,
  .bbi_args = list(
    no_grd_file = TRUE,
    no_shk_file = TRUE
  )
) {

  if (inherits(.mod, "character")) {
    checkmate::assert_string(.mod)
    .mod <- read_model(.mod)
  }
  check_model_object(.mod, c(NM_MOD_CLASS, NM_SUM_CLASS))
  assert_string(.join_col)
  assert_character(.files)
  assert_logical(.superset, len = 1)
  assert_list(.bbi_args)

  df_list <- nm_tables(.mod, .files = .files)
  .d <- df_list$data
  .tbls <- df_list[2:length(df_list)]
  if (
    "DV" %in% names(.d) &&
    "DV" %in% unlist(map(.tbls, names))
  ) {
    .d <- rename(.d, DV.DATA = .data$DV)
  }
  col_order <- names(.d)

  .join_col <- toupper(.join_col)
  if (!(.join_col %in% names(.d))) {
    stop(glue("couldn't find `.join_col` {.join_col} in data with cols: {paste(names(.d), collapse = ', ')}"))
  }

  if(.d[.join_col] %>% anyDuplicated() != 0)
  {
   dup_row <- .d[.join_col][duplicated( .d[.join_col]) %>% which(),]
   stop(verbose_msg(glue("Duplicate rows in data in {.join_col}: {dup_row}")))
  }

  if (.superset) {
    join_fun <- function(x, y, ...) left_join(y, x, ...)
  } else {
    join_fun <- left_join
  }

  # get number of ID's and records
  .s <- if (!inherits(.mod, NM_SUM_CLASS)) {
    model_summary(.mod, .bbi_args = .bbi_args)
  } else {
    .mod
  }
  nid <-  .s$run_details$number_of_subjects
  nrec <- .s$run_details$number_of_data_records

  # do the join(s)
  for (.n in names(.tbls)) {
    tab <- .tbls[[.n]]
    has_id <- "ID" %in% names(tab)

    if (!(nrow(tab) %in% c(nrec, nid))) {
      # skip table if nrow doesn't match number of records or ID's
      # because if neither is true than this is the wrong kind of file
      # (or something is wrong with NONMEM output)
      warning(glue("{.n} skipped because number of rows ({nrow(tab)}) doesn't match number of records ({nrec}) or IDs ({nid})"), call. = FALSE)
    } else if (nrow(tab) == nid) {
      # if FIRSTONLY table join on ID
      verbose_msg(glue("{.n} is FIRSTONLY table"))

      # if ID is missing, get it from the data by using .join_col
      if (!has_id) {
        tab <- tab %>%
          left_join(select(.d, .data$ID, !!.join_col), by = .join_col)
      }

      # toss .join_col, if present, because we're joining on ID
      tab[[.join_col]] <- NULL

      # do the join
      tab <- drop_dups(tab, .d, "ID", .n)
      col_order <- union(col_order, names(tab))
      .d <- join_fun(tab, .d, by = "ID")
    } else if (nrow(tab) == nrec) {
      # otherwise, join on .join_col
      tab <- drop_dups(tab, .d, .join_col, .n)
      col_order <- union(col_order, names(tab))
      .d <- join_fun(tab, .d, by = .join_col)
    }
  }

  verbose_msg(c(
    glue("\nfinal join stats:"),
    glue("  rows: {nrow(.d)}"),
    glue("  cols: {ncol(.d)}")
  ))

  return(select(.d, !!col_order))
}


#' Drop duplicate columns to prepare for join
#' @keywords internal
drop_dups <- function(.new_table, .dest_table, .join_col, .table_name) {
  new_cols <- setdiff(names(.new_table), names(.dest_table))
  keep <- c(.join_col, new_cols)
  drop <- setdiff(names(.new_table), keep)

  verbose_msg(glue("{.table_name} adds {length(new_cols)} new cols"))
  if (length(drop) > 0) verbose_msg(glue("  dropping {length(drop)} duplicate cols: {paste(drop, collapse = ', ')}"))
  verbose_msg("") # for newline

  if(.new_table[.join_col] %>% anyDuplicated() != 0)
  {
    dup_row <- .new_table[.join_col][duplicated( .new_table[.join_col]) %>% which(),]
    stop(verbose_msg(glue("Duplicate rows in {.join_col}: {dup_row}")))
  }

   #if(.dest_table[.join_col] %>% anyDuplicated() != 0)
   #{
   #   dup_row <- .dest_table[.join_col][duplicated( .dest_table[.join_col]) %>% which(),]
   #   stop(verbose_msg(glue("Duplicate rows in {.join_col}: {dup_row}")))
   # }


  return(.new_table[keep])
}
