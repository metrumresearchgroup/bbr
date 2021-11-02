#' Return a single data frame with model output and input data
#'
#' When a unique row identifier (e.g an integer numbering the rows) is included
#' in the source data set and carried into each table output, `nm_join()` can read
#' in all output table files and join back to the source data set. By default,
#' the input data is joined to the table files so that the number of rows in the
#' result will match the number of rows in the table files (or the number of
#' rows not bypassed via `$IGNORE`). Use the `.superset` argument to join table
#' outputs to the (complete) input data set.
#'
#' @param .mod the `bbi_nonmem_model` object or a path to a NONMEM run.
#' @param .join_col character column name to use to join tab files. See Details.
#' @param .files absolute paths to table files to try to join.
#' @param .more absolute paths to other files to try to join.
#' @param .verbose if TRUE, the default, messages are printed as files are read.
#'   Can be controlled package-wide by setting `options(bbr.verbose)`.
#' @param .superset if `FALSE`, the default, the data will be joined to the
#'   NONMEM output and if `TRUE`, the NONMEM output will be joined to the data;
#'   that is, if you use `.superset`, you will get the same number of rows as
#'   you have in the input data and NONMEM output columns like `PRED` and
#'   `CWRES` will be filled with `NA`.
#' @param .bbi_args list passed to `bbr::model_summary(.bbi_args)`. See
#'   [print_bbi_args()] for valid options.
#'
#' @details
#' The `.join_col` is currently limited to a single column that should appear in
#' both the source data set and any tables you want to join. We recommend you
#' make `.join_col` a simple integer numbering the rows in the input data set
#' (for example `NUM`). When this column is carried into the output table files,
#' there will be unambiguous matching from the table file back to the source
#' data set.
#'
#' Note also that, when `.join_col` is carried into table outputs, there is no
#' need to table any other columns from the input data as long as the
#' `nm_join()` approach is used; any column in the source data set, regardless
#' of whether it is listed in `$INPUT` or not, will be available in the joined
#' result.
#'
#' **NEED TO DISCUSS FIRSTONLY AND ID COL**
#'
#' You can get the absolute path to the model output directory with
#' [get_output_dir()] or [build_path_from_model()] if needed to build paths for
#' passing to `.files` or `.more`.
#'
#' @importFrom dplyr left_join select all_of everything
#' @importFrom readr read_csv read_table
#' @export
nm_join <- function(
  .mod,
  .join_col = "NUM",
  .files = tabfiles(.mod),
  .more = NULL,
  .verbose = getOption("bbr.verbose"),
  .superset = FALSE,
  .bbi_args = list(
    no_grd_file = TRUE,
    no_shk_file = TRUE
  )
) {

  if (!inherits(.mod, "bbi_nonmem_model")) {
    checkmate::assert_string(.mod)
    .mod <- read_model(.mod)
  }
  checkmate::assert_string(.join_col)
  checkmate::assert_character(.files)
  checkmate::assert_character(.more, null.ok = TRUE)
  checkmate::assert_logical(.verbose, len = 1)
  checkmate::assert_logical(.superset, len = 1)
  checkmate::assert_list(.bbi_args)

  # We always do a left join; with .superset, the data is on the left but
  # with non-.superset, nonmem table is on the left
  # when there is a FIRSTONLY table, we left_join the nonmem table on to
  # the current data by ID (regardless)
  if(.superset) {
    join_fun <- function(x, y, ...) left_join(y, x, ...)
  } else {
    join_fun <- left_join
  }

  # load input data
  summ <- model_summary(.mod, .bbi_args = .bbi_args)
  nid <-  summ$run_details$number_of_subjects
  nrec <- summ$run_details$number_of_data_records
  data_loc <- get_data_path(summ)
  if(.verbose) message("data file: ", basename(data_loc))

  .d <- read_csv(data_loc, na = ".", col_types = readr::cols())
  names(.d) <- toupper(names(.d))
  if(.verbose) {
    message("  rows: ", nrow(.d))
    message("  cols: ", ncol(.d))
  }

  # check for table files
  ### TODO: consider letting people pass relative paths somehow
  ### this is annoying: test_df <- nm_join(MOD1, .files = build_path_from_model(.mod, "cl.tab"))
  .files <- c(.files, .more)
  chk <- file.exists(.files)
  skippers <- .files[!chk & !(.files %in% tabfiles(.mod))]
  if (length(skippers) > 0) {
    # warn about any non-default files that are missing
    warning(paste(
      "The following table files do not exist and will be skipped:",
      paste(skippers, collapse = ", ")
    ), call. = FALSE)
  }
  .files <- .files[chk]

  if(length(.files)==0) {
    warning("Zero table files found; returning only input data.")
    return(.d)
  }

  .join_col <- toupper(.join_col)
  if(!(.join_col %in% names(.d))) {
    stop(glue("couldn't find `.join_col` {.join_col} in data with cols: {paste(names(.d), collapse = ', ')}"))
  }

  leading_cols <- names(.d)
  for(p in .files) {
    tab <- read_table(p, skip =1, na = ".", col_types = readr::cols())
    names(tab) <- toupper(names(tab))
    has_id <- "ID" %in% names(tab)

    # suffix for duplicated columns
    .tab_suf <- str_replace(basename(p), get_model_id(.mod), "")
    .suffix <- if (isTRUE(.superset)) {
      c("", .tab_suf)
    } else {
      c(.tab_suf, "")
    }

    # skip table if nrow doesn't match number of records or ID's
    # because if neither is true than this is the wrong kind of file
    # (or something is wrong with NONMEM output)
    if(!(nrow(tab) %in% c(nrec, nid))) {
      warning(glue("table file: {basename(p)} skipped because nrow doesn't match number of records or IDs"), call. = FALSE)
      if(.verbose) {
        message("  rows: ", nrow(tab))
        message("  hasid: ", has_id)
        message("  tabids: ", length(unique(tab[["ID"]])))
        message("  runids: ", nid)
      }
      next;
    }

    # first only tables (join on ID)
    if(nrow(tab) == nid) {
      if(.verbose) {
        message("\nfirstonly file: ", basename(p))
        message("  rows: ", nrow(tab))
        message("  cols: ", ncol(tab))
      }
      # keep <- c("ID",setdiff(names(tab), names(.d)))
      # if((ncol(tab) - length(keep) - 1) > 0) {
      #   if(.verbose) {
      #     message(
      #       "  droppping: ",
      #       ncol(tab) - length(keep) - 1,
      #       " columns in common"
      #     )
      #   }
      # }
      # .d <- left_join(.d, tab[,keep], by = "ID")

      # if ID is missing, get it from the data by using .join_col
      if (!has_id) {
        tab <- tab %>%
          left_join(select(.d, .data$ID, !!.join_col), by = .join_col)
      }

      tab[[.join_col]] <- NULL

      .d <- join_fun(tab, .d, by = "ID", suffix = .suffix)
      next;
    } # end ID join

    if(.verbose) message("\ntable file: ", basename(p))
    new_cols <- setdiff(names(tab), names(.d))
    # keep <- c(.join_col, new_cols)
    # drop <- setdiff(names(tab), new_cols)
    # drop <- drop[!(drop %in% .join_col)]
    # if(.verbose) {
    #   message("  rows: ", nrow(tab))
    #   message("  cols: ", length(new_cols), " new")
    #   for(d in drop) {
    #     message("  drop: ", d,  " common")
    #   }
    # }
    # .d <- join_fun(tab[,keep], .d, by = .join_col)
    if(.verbose) {
      message("  rows: ", nrow(tab))
      message("  cols: ", length(new_cols), " new")
    }
    .d <- join_fun(tab, .d, by = .join_col, suffix = .suffix)
  }

  if(.verbose) {
    message("\nfinal stats:")
    message("  rows: ", nrow(.d))
    message("  cols: ", ncol(.d))
  }

  return(select(.d, all_of(leading_cols), everything()))
}


#' @describeIn nm_join Return a vector of absolute paths to table files to look
#'   for by default.
#' @param .mod the `bbi_nonmem_model` object
#' @export
tabfiles <- function(.mod) {
  if (inherits(.mod, "character")) .mod <- read_model(.mod)
  output_dir <- get_output_dir(.mod)
  c(
    file.path(output_dir, "PAR_TAB"),
    file.path(output_dir, "TAB"),
    build_path_from_model(.mod, "par.tab"),
    build_path_from_model(.mod, ".tab")

  )
}
