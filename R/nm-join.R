#' Return a single data frame with model output and input data
#'
#' @param .mod the `bbi_nonmem_model` object or a path to a NONMEM run
#' @param .join_col character column name to use to join tab files
#' @param .files absolute paths to table files to try to join
#' @param .more absolute paths to other files to try to join
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
#' Note that you can get the absolute path to the model output directory with
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

  .files <- c(.files, .more)
  summ <- model_summary(.mod, .bbi_args = .bbi_args)
  nid <-  summ$run_details$number_of_subjects
  nrec <- summ$run_details$number_of_data_records
  data_loc <- get_data_path(summ)
  if(.verbose) message("data file: ", basename(data_loc))

  data <- read_csv(data_loc, na = ".", col_types = readr::cols())
  names(data) <- toupper(names(data))
  if(.verbose) {
    message("  rows: ", nrow(data))
    message("  cols: ", ncol(data))
  }

  chk <- file.exists(.files)
  .files <- .files[chk]
  if(length(.files)==0) {
    if (.verbose) message("  zero table files found; returning")
    return(data)
  }

  .join_col <- toupper(.join_col)
  if(!all(.join_col %in% names(data))) {
    stop(glue("couldn't find `.join_col` {.join_col} in data with cols: {paste(names(data), collapse = ', ')}"))
  }
  leading_cols <- names(data)
  for(p in .files) {
    tab <- read_table(p, skip =1, na = ".", col_types = readr::cols())
    names(tab) <- toupper(names(tab))
    has_id <- "ID" %in% names(tab)
    if(!(nrow(tab) %in% c(nrec,nid))) {
      if(.verbose) {
        message("\ntable file: ", basename(p), " (skipped)")
        message("  rows: ", nrow(tab))
        message("  hasid: ", has_id)
        message("  tabids: ", length(unique(tab[["ID"]])))
        message("  runids: ", nid)
      }
      next;
    }
    if(nrow(tab) == nid && has_id) {
      if(.verbose) {
        message("\nfirstonly file: ", basename(p))
        message("  rows: ", nrow(tab))
        message("  cols: ", ncol(tab))
      }
      keep <- c("ID",setdiff(names(tab), names(data)))
      if((ncol(tab) - length(keep) - 1) > 0) {
        if(.verbose) {
          message(
            "  droppping: ",
            ncol(tab) - length(keep) - 1,
            " columns in common"
          )
        }
      }
      data <- left_join(data, tab[,keep], by = "ID", suffix = c("", "y"))
      next;
    } # end ID join
    if(nrow(tab) != nrec) {
      if (.verbose) {
        message("\ntable file: ", basename(p), " (skipped)")
        message("  rows: ", nrow(tab))
        message("  hasid: ", has_id)
        message("  tabids: ", length(unique(tab[["ID"]])))
        message("  runids: ", nid)
      }
      next;
    }
    if(.verbose) message("\ntable file: ", basename(p))
    new_cols <- setdiff(names(tab), names(data))
    keep <- c(.join_col, new_cols)
    drop <- setdiff(names(tab), new_cols)
    drop <- drop[!(drop %in% .join_col)]
    if(.verbose) {
      message("  rows: ", nrow(tab))
      message("  cols: ", length(new_cols), " new")
      for(d in drop) {
        message("  drop: ", d,  " common")
      }
    }
    data <- join_fun(tab[,keep], data, by = .join_col)
  }
  if(.verbose) {
    message("\nfinal stats:")
    message("  rows: ", nrow(data))
    message("  cols: ", ncol(data))
  }
  select(data, all_of(leading_cols), everything())
}


#' Return a vector of absolute paths to table files to look for
#'
#' @param .mod the `bbi_nonmem_model` object
#'
#' @details
#' Called by redataset
#' @keywords internal
tabfiles <- function(.mod) {
  output_dir <- get_output_dir(.mod)
  c(
    file.path(output_dir, "PAR_TAB"),
    file.path(output_dir, "TAB"),
    build_path_from_model(.mod, "par.tab"),
    build_path_from_model(.mod, ".tab")

  )
}
