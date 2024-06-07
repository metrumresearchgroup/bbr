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
#' @template nm-join-col
#' @template nm-join-col-firstonly
#' @template nm-join-col-tables
#' @template nm-join-col-eg
#'
#' @section Details:
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
#' The origin of each column is attached to the return value via the
#' "nm_join_origin" attribute, a list that maps each source (as named by
#' [nm_tables()]) to the columns that came from that source.
#'
#'
#' **Multiple tables per file incompatibility**
#'
#' `nm_join()` is currently _not_ compatible with multiple tables written to a
#' single files. **To handle multiple tables**, consider reading in the tables via
#' [nm_tables()] (or individually via [nm_file()] & [nm_file_multi_tab()]), and
#' joining the data manually.
#'  - Note that `nm_join_sim()` _does_ support multiple tables for simulations.
#'
#' @importFrom dplyr left_join select
#' @importFrom checkmate assert_string assert_character assert_logical assert_list
#' @seealso [nm_join_sim()], [nm_tables()], [nm_table_files()], [nm_file()],
#' [nm_file_multi_tab()]
#' @return a tibble
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
  if (inherits(.mod, "bbi_nmbayes_model")) {
    stop(
      "nm_join() is not supported for nmbayes models; ",
      "use `bbr.bayes::nm_join_bayes()` instead."
    )
  }

  if (inherits(.mod, "bbi_nmsim_model")) {
    stop(
      "nm_join() is not supported for nmsim models; ",
      "use `nm_join_sim()` instead."
    )
  }

  if (inherits(.mod, "character")) {
    checkmate::assert_string(.mod)
    .mod <- read_model(.mod)
  }
  check_model_object(.mod, c(NM_MOD_CLASS, NM_SUM_CLASS))
  nm_join_impl(.mod, .join_col, .files, .superset, .bbi_args)
}


#' Implementation function for `nm_join()`
#' @inheritParams nm_join
#' @template nm-join-col
#' @template nm-join-col-firstonly
#' @template nm-join-col-tables
#' @template nm-join-col-eg
#' @keywords internal
nm_join_impl <- function(
    .mod,
    .join_col = "NUM",
    .files = nm_table_files(.mod),
    .superset = FALSE,
    .bbi_args = list(
      no_grd_file = TRUE,
      no_shk_file = TRUE
    )
) {
  assert_string(.join_col)
  assert_character(.files)
  assert_logical(.superset, len = 1)
  assert_list(.bbi_args)

  # Multi-tabled files are not currently supported
  df_list <- nm_tables(.mod, .files = .files, read_multi_tab = FALSE)
  .d <- df_list$data

  # Handling for when input data is the only element
  # This can happen if no _single table_ files were found
  .tbls <- if(length(df_list) >= 2){
    df_list[2:length(df_list)]
  }else{
    NULL
  }

  if (
    "DV" %in% names(.d) &&
    "DV" %in% unlist(map(.tbls, names))
  ) {
    .d <- rename(.d, DV.DATA = "DV")
  }
  col_order <- names(.d)

  # Keep track of where each column came from.
  origin <- vector(mode = "list", length = length(df_list))
  names(origin) <- names(df_list)
  origin$data <- col_order

  .join_col <- toupper(.join_col)
  if (!(.join_col %in% names(.d))) {
    stop(glue("couldn't find `.join_col` {.join_col} in data with cols: {paste(names(.d), collapse = ', ')}"))
  }

  if(anyDuplicated(.d[.join_col]) != 0)
  {
    dup_row <- .d[.join_col][duplicated( .d[.join_col]) %>% which(),]
    stop(glue("Duplicate rows were found in {.join_col}. Please see `?nm_join` for more details"))
  }

  if (.superset) {
    join_fun <- function(x, y, ...) left_join(y, x, ...)
    join_first_only_fun <- join_fun
  } else {
    join_fun <- left_join
    # For the FIRSTONLY case, joining the data to the table is expected to match
    # multiple table rows; use left_join_all() to avoid a warning.
    join_first_only_fun <- left_join_all
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
  if(!is.null(.tbls)){
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
            left_join(select(.d, "ID", !!.join_col), by = .join_col)
        }

        # toss .join_col, if present, because we're joining on ID
        tab[[.join_col]] <- NULL

        # do the join
        tab <- drop_dups(tab, .d, "ID", .n)
        col_order <- union(col_order, names(tab))
        .d <- join_first_only_fun(tab, .d, by = "ID")
      } else if (nrow(tab) == nrec) {
        # otherwise, join on .join_col
        tab <- drop_dups(tab, .d, .join_col, .n)
        col_order <- union(col_order, names(tab))
        .d <- join_fun(tab, .d, by = .join_col)
      }
      origin[[.n]] <- names(tab)
    }
  }


  verbose_msg(c(
    glue("\nfinal join stats:"),
    glue("  rows: {nrow(.d)}"),
    glue("  cols: {ncol(.d)}")
  ))

  res <- select(.d, !!col_order)
  attr(res, "nm_join_origin") <- origin

  return(res)
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
    stop(glue("Duplicate rows in {.join_col}: {dup_row}"))
  }
  return(.new_table[keep])
}


#' Helper to determine if `nm_join()` can be called on a `bbi_nonmem_model`
#' object
#' @inheritParams nm_tables
#' @noRd
can_be_nm_joined <- function(.mod){
  check_model_object(.mod, c(NM_MOD_CLASS, NM_SUM_CLASS))
  if(inherits(.mod, NM_SUM_CLASS)){
    .mod <- read_model(.mod[[ABS_MOD_PATH]])
  }

  # Model submission status checks
  is_finished <- model_is_finished(.mod)

  # Check for presence of table records
  ctl <- safe_read_ctl(.mod)
  table_recs <- nmrec::select_records(ctl, "table")
  has_tables <- !rlang::is_empty(table_recs)

  reasons <- c()
  if(isFALSE(is_finished)){
    reasons <- c(reasons, "Model has not finished executing")
  }

  if(isFALSE(has_tables)){
    reasons <- c(reasons, "Model has no table records. Nothing to join to `nm_data()`")
  }

  if(isFALSE(is_finished) || isFALSE(has_tables)){
    reasons_txt <- paste0(" - ", reasons, collapse = "\n")
    rlang::inform(
      c(
        "`nm_join()` cannot be used to join model output and input data",
        "i"="Reasons:",
        reasons_txt
      )
    )
    return(invisible(FALSE))
  }else{
    return(invisible(TRUE))
  }
}


#' Join simulation and input data
#'
#' Joins the simulation and input data of a `bbi_nonmem_model` object with an
#' attached simulation.
#' @param .mod A `bbi_nonmem_model` with an attached simulation, or a
#'  `bbi_nmsim_model` object.
#' @param .join_col Character column name(s) to use to join table files.
#'  Defaults to `NUM`. See Details.
#' @param .cols_keep Either `'all'`, or a vector of column name(s) to retain
#'  in the final dataset after joining. Defaults to keeping all columns.
#' @inheritParams nm_file_multi_tab
#'
#' @note The join column name(s) specified should match what you provided to
#' [add_simulation()].
#'
#' @template nm-join-col
#' @template nm-join-col-tables
#' @template nm-join-col-eg
#'
#' @seealso [add_simulation()], [nm_file_multi_tab()], [nm_tables()]
#'
#' @examples
#' \dontrun{
#'
#' add_simulation(.mod, .join_col = c("NUM", "ID"))
#'
#' nm_join_sim(.mod, .join_col = c("NUM", "ID"), .cols_keep = "ID")
#'
#' # These return the same thing (simulation model is automatically read-in):
#' nm_join_sim(.mod)
#' nm_join_sim(get_simulation(.mod))
#' }
#'
#'
#' @return a tibble
#' @export
nm_join_sim <- function(
    .mod,
    .join_col = "NUM",
    .cols_keep = "all",
    add_table_names = FALSE
){
  checkmate::assert_character(.join_col)
  checkmate::assert_character(.cols_keep)
  checkmate::assert_logical(add_table_names)

  if(!has_simulation(.mod)){
    rlang::abort("No attached simulation found")
  }
  # Support bbi_nonmem_models as well for when a user passes in the model with
  # the attached simulation
  check_model_object(.mod, c(NMSIM_MOD_CLASS, NM_MOD_CLASS, NM_SUM_CLASS))

  if(inherits(.mod, NM_SUM_CLASS)){
    .mod <- read_model(.mod[[ABS_MOD_PATH]])
  }

  # If passing a bbi_nonmem_model with an attached simulation, use the simulation model
  #  - i.e. this function supports passing in the simulation model directly, or
  #    the parent model
  if(inherits(.mod, NM_MOD_CLASS)){
    .mod <- get_simulation(.mod)
  }

  # Only support joining the table we add so we can make certain assumptions
  files <- nm_table_files(.mod)
  if(length(files) > 1){
    rlang::abort(
      "`nm_join_sim` only supports joining a single table file (the one bbr adds)",
      "Try using `nm_tables()` to manually join the tables"
    )
  }

  # Get list of all tables. Likely only one simulation table, but may include
  # other manually added tables
  df_list <- nm_tables(
    .mod, .files = files, read_multi_tab = TRUE, add_table_names = add_table_names
  )

  .d <- df_list$data
  keep_all_cols <- (length(.cols_keep) == 1 && .cols_keep == "all")
  if(isFALSE(keep_all_cols) && !all(.cols_keep %in% names(.d))){
    col_keep_txt <- paste0(.cols_keep, collapse = ", ")
    .cols_keep <- "all"
    rlang::warn(
      c(
        glue("Cannot find requested columns ({col_keep_txt}) in input data."),
        "i" = "See `names(nm_data(.mod))` for available `.cols_keep` options.",
        "Returning all columns..."
      )
    )
  }

  .tbls <- df_list[2:length(df_list)]

  if("DV" %in% names(.d) && "DV" %in% unlist(map(.tbls, names))){
    .d <- dplyr::rename(.d, DV.DATA = "DV")
  }
  col_order <- if(isTRUE(keep_all_cols)) names(.d) else .cols_keep

  # Keep track of where each column came from.
  origin <- vector(mode = "list", length = length(df_list))
  names(origin) <- names(df_list)
  origin$data <- col_order

  .join_col <- toupper(.join_col)
  if(!all(.join_col %in% names(.d))){
    join_col_txt <- paste0(.join_col[!(.join_col %in% names(.d))], collapse = ", ")
    stop(glue("couldn't find `.join_col` {join_col_txt} columns in input data"))
  }

  if(anyDuplicated(.d[.join_col]) != 0){
    join_col_txt <- paste0(.join_col, collapse = ", ")
    stop(glue("Duplicate rows were found in {join_col_txt}. Please see `?nm_join_sim` for more details"))
  }

  # Note: Kept for-loop in case we decide to support multiple table records
  #  (i.e. additional, manually added ones) at a later time
  for (.n in names(.tbls)) {
    tab <- .tbls[[.n]]
    if(!all(.join_col %in% names(tab))){
      join_col_txt <- paste0(.join_col[!(.join_col %in% names(tab))], collapse = ", ")
      stop(glue("couldn't find `.join_col` {join_col_txt} in data with cols: {paste(names(tab), collapse = ', ')}"))
    }
    # This assumes the multi-tabled file is a necessarily a simulation dataset
    if("table_id" %in% names(tab)){
      tmp <- tab %>% dplyr::group_by(.data$table_id) %>% dplyr::mutate(NN = dplyr::cur_group_id())
      tab <- dplyr::select(tab, -c("table_id"))
    }
    # Note: we dont drop duplicates here since there will inherently be duplicated
    # `NUM` (.join_col) values per replicate
    col_order <- union(col_order, names(tab))
    .d <- dplyr::left_join(tab, .d, by = .join_col)
    origin[[.n]] <- names(tab)
  }

  verbose_msg(c(
    glue("\nfinal join stats:"),
    glue("  rows: {nrow(.d)}"),
    glue("  cols: {ncol(.d)}")
  ))

  res <- dplyr::select(.d, !!col_order)
  attr(res, "nm_join_origin") <- origin

  return(res)
}
