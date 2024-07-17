#' Read NONMEM files
#'
#' Reads in a whitespace-delimited NONMEM output file (for example .grd or .ext
#' or a table output) or a NONMEM input data file. Will print the number of rows
#' and columns when the file is loaded. This **printing can be suppressed** by
#' setting `options(bbr.verbose = FALSE)`.
#'
#' @details
#' `nm_file()` is called internally by the family of functions in this help doc,
#' as well as by [nm_tables()] and [nm_join()].
#'
#' `nm_file()` assumes there is only one table per file and therefore
#' `nm_file()` (and family) are _not_ compatible with files that have multiple
#' tables, for example an `.ext` file for a model with multiple estimation
#' methods or a table file from a model using `$SIM`. For these kinds of files,
#' consider using `nm_file_multi_tab`.
#'
#' @return A tibble with the data from the specified file and estimation method.
#'
#' @param .mod Either a `bbi_nonmem_model`, `bbi_nonmem_summary`, or a path to a
#'   file to read in. If passing model object to `nm_file()`, must also pass `.suffix` that
#'   will be passed through to [build_path_from_model()].
#' @inheritParams build_path_from_model
#' @param ... arguments passed through to methods. (Currently none.)
#' @seealso [nm_tables()], [nm_table_files()], [nm_file_multi_tab()], [nm_join()]
#' @export
nm_file <- function(.mod, .suffix = NULL, ...) {
  UseMethod("nm_file")
}

#' @export
nm_file.bbi_model <- function(.mod, .suffix = NULL, ...) {
  check_model_object(.mod, c(NM_MOD_CLASS, NM_SUM_CLASS))
  if (is.null(.suffix)) {
    stop("Must pass .suffix to use nm_file.bbi_model")
  }
  .path <- build_path_from_model(.mod, .suffix)
  nm_file(.path, .suffix = NULL)
}

#' @export
nm_file.character <- function(.mod, .suffix = NULL, ...) {
  checkmate::assert_string(.mod)
  if (!is.null(.suffix)) {
    stop("Cannot pass .suffix to use nm_file.character; pass only file path to .mod")
  }
  nm_file_impl(.mod)
}

#' @describeIn nm_file Reads `.grd` file from a `bbi_nonmem_model` or
#'   `bbi_nonmem_summary` object
#' @param .rename Logical (`T`/`F`). If `TRUE`, the default, will rename `.grd`
#'   columns to the relevant parameter names. Otherwise will leave column names
#'   as is.
#' @export
nm_grd <- function(.mod, .rename = TRUE) {
  check_model_object(.mod, c(NM_MOD_CLASS, NM_SUM_CLASS))
  grd_df <- nm_file(.mod, .suffix = ".grd")

  if (isTRUE(.rename)) {
    .s <- model_summary(.mod)

    .est_method <- length(.s$parameters_data)

    lbl <- c(.s$parameter_names$theta[.s$parameters_data[[.est_method]]$fixed$theta==0],
             .s$parameter_names$sigma[.s$parameters_data[[.est_method]]$fixed$sigma==0],
             .s$parameter_names$omega[.s$parameters_data[[.est_method]]$fixed$omega==0])
    names(grd_df) <- c("ITERATION", lbl)
  }
  return(grd_df)
}

#' @describeIn nm_file Reads `{get_model_id(.mod)}.tab` file from a
#'   `bbi_nonmem_model` or `bbi_nonmem_summary` object
#' @export
nm_tab <- function(.mod) {
  check_model_object(.mod, c(NM_MOD_CLASS, NM_SUM_CLASS))
  nm_file(.mod, .suffix = ".tab")
}

#' @describeIn nm_file Reads `{get_model_id(.mod)}par.tab` file from a
#'   `bbi_nonmem_model` or `bbi_nonmem_summary` object
#' @export
nm_par_tab <- function(.mod) {
  check_model_object(.mod, c(NM_MOD_CLASS, NM_SUM_CLASS))
  nm_file(.mod, .suffix = "par.tab")
}

#' @describeIn nm_file Reads the input data file from a `bbi_nonmem_model` or
#'   `bbi_nonmem_summary` object
#' @param filter Logical (`T`/`F`). If `TRUE`, filter data based on `IGNORE LIST`
#'  or `ACCEPT LIST` options defined in the `$DATA` record.
#' @importFrom data.table fread
#' @importFrom tibble as_tibble
#' @export
nm_data <- function(.mod, filter = FALSE) {
  check_model_object(.mod, c(NM_MOD_CLASS, NM_SUM_CLASS, NMSIM_MOD_CLASS))
  .path <- get_data_path(.mod)
  verbose_msg(glue("Reading data file: {basename(.path)}"))
  .d <- fread(.path, na.strings = ".", verbose = FALSE)
  .d <- remove_dup_cols(.d)
  .d <- as_tibble(.d)

  names(.d) <- toupper(names(.d))

  if(isTRUE(filter)){
    .d <- filter_nm_data(.mod, data = .d)
    recs_dropped <- attributes(.d)$n_records_dropped
    verbose_msg(paste("Number of records dropped:", cli::col_blue(recs_dropped)))
  }

  verbose_msg(glue("  rows: {nrow(.d)}"))
  verbose_msg(glue("  cols: {ncol(.d)}"))
  verbose_msg("") # for newline

  return(.d)
}


#####################################
# PRIVATE HELPERS AND IMPLEMENTATION

#' Implementation function for reading NONMEM files
#' @importFrom tibble as_tibble
#' @importFrom data.table fread
#' @importFrom stringr str_detect
#' @param .path a path to a table file.
#' @keywords internal
nm_file_impl <- function(.path) {
  # read file and find top of table
  verbose_msg(glue("Reading {basename(.path)}"))

  # get line numbers of TABLE lines
  checkmate::assert_file_exists(.path)

  # read the file, but catch warning that tells us there are multiple tables
  W <- NULL
  .d <- withCallingHandlers({
    withr::with_options(list(warn = 1), {
      data <- fread(
        .path,
        na.strings = ".",
        skip = 1,
        verbose = FALSE
      )
    })
    data <- remove_dup_cols(data)
    data <- as_tibble(data)
    data
  },
  warning = function(.w) {
    if (str_detect(.w$message, "Stopped early.+TABLE NO")) {
      W <<- paste(
        "nm_file() does not support files with multiple tables in a single file.",
        glue("{.path} appears to contain multiple tables and will be skipped."),
        sep = "\n"
      )
    } else {
      warning(.w)
    }
    invokeRestart("muffleWarning")
  })

  # if found multiple tables, raise custom warning and return NULL
  if (!is.null(W)) {
    warning(W)
    return(NULL)
  }

  # format, message, and return
  verbose_msg(glue("  rows: {nrow(.d)}"))
  verbose_msg(glue("  cols: {ncol(.d)}"))
  verbose_msg("") # for newline
  return(.d)
}



#' Read in table file with multiple `NONMEM` tables.
#' @inheritParams nm_file_impl
#' @param add_table_names Logical (T/F). If `TRUE`, include a column denoting
#'  the table names as specified in the file (e.g., `'TABLE NO.  1'`).
#' @param table_pattern character string defining the start of a new
#'  table (regex accepted).
#'
#' @details
#' The returned object will change depending on the types of tables contained
#' in the file:
#'  - If each table is the same number of rows and column names (e.g., simulation
#'  data), the file can be read-in all at once (significant performance
#'  improvements), and the data will be coerced to a single dataframe.
#'  - If the number of rows differ across _any_ of the tables, they must be read
#'  in one at a time.
#'  - If the column names differ across _any_ of the tables, a list of tables is
#'  returned.
#'
#' If a single dataframe can be returned, an additional `nn` column will be
#' appended denoting the table number. This would also be the simulation number
#' if running a simulation (removing the need to keep track of iterations within
#' the control stream itself).
#'
#' This function expects that tables are in the following format by default
#' (column names can differ as noted above):
#' ```
#' TABLE NO.  1
#' NUM         ID          DV
#' 1.0000E+00  1.0000E+00  5.45615E+00
#' TABLE NO.  2
#' NUM         ID          DV
#' 1.0000E+00  1.0000E+00  6.54565E+00
#' ```
#'
#' @returns either a `tibble` or list depending on the column names of each table
#' @export
nm_file_multi_tab <- function(
    .path,
    add_table_names = TRUE,
    table_pattern="TABLE NO"
){
  verbose_msg(glue("Reading {basename(.path)}"))
  checkmate::assert_file_exists(.path)

  x <- stringfish::sf_readLines(.path)

  # Get new table indices and names
  tab_rows <- which(stringfish::sf_grepl(x, pattern = table_pattern, fixed = TRUE))
  table_names <- x[tab_rows]
  col_names <- x[tab_rows + 1]

  # If each table is the same number of rows and column names, assume simulation
  # (takes advantage of faster read-in times)
  #  - subtract 2 for table name and column headers
  #  - add `(length(x) + 1)` to get the range of the last table, accounting for
  #    the table header (minus 1)
  nrow_each <- diff(c(tab_rows, (length(x) + 1))) - 2
  same_rows <- dplyr::n_distinct(nrow_each) == 1
  same_cols <- dplyr::n_distinct(col_names) == 1

  if(isTRUE(same_rows) && isTRUE(same_cols)){
    cols <- strsplit(trimws(x[2]), split = "\\s+")[[1]]
    # Remove table names before reading in
    x <- x[-c(tab_rows, tab_rows+1)]
    data <- data.table::fread(
      text = x,
      col.names = cols,
      colClasses = rep("numeric", length(cols)),
      na.strings = ".",
      header = FALSE
    )

    # Assign table number
    #  - This is useful for simulation data, where nn would be the replicate
    #  - This part removes the need to keep track of the replicate within the
    #    NONMEM model
    total <- nrow(data) / nrow_each[1]
    data$nn <- rep(seq(total), each = nrow_each[1])
    if(isTRUE(add_table_names)){
      data$table_name <- rep(table_names, each = nrow_each[1])
    }
    data <- as_tibble(data)
  }else{
    # Multi-tabled files that _aren't_ from simulations likely follow this method
    # - i.e. the number of rows per table can differ
    table_list <- list()
    # Read in and format each individual table
    for(idx in seq_along(tab_rows)){
      start_line <- tab_rows[idx] + 1
      end_line <- if(idx == length(tab_rows)) length(x) else tab_rows[idx + 1] - 1
      table.i <- withr::with_options(list(warn = 1), {
        data <- data.table::fread(
          file = .path,
          skip = (start_line - 1),
          nrows = (end_line - start_line),
          na.strings = ".",
          header = TRUE,
          verbose = FALSE
        )
        data <- remove_dup_cols(data)
        if(isTRUE(add_table_names)){
          data <- data %>% dplyr::mutate(table_name = table_names[idx])
        }
        as_tibble(data)
      })
      table_list[[idx]] <- table.i
    }

    # Attempt to coerce list to dataframe (same columns, different number of rows)
    if(isTRUE(same_cols)){
      data <- purrr::list_rbind(table_list, names_to = "nn") %>%
        dplyr::relocate("nn", .after = dplyr::everything())
    }else{
      data <- table_list
      verbose_msg(glue("Tables in {basename(.path)} cannot be coerced to a single dataframe"))
    }
  }

  if(inherits(data, "list")){
    verbose_msg(glue("  tables: {length(table_list)}"))
  }else{
    verbose_msg(glue("  rows: {nrow(data)}"))
    verbose_msg(glue("  cols: {ncol(data)}"))
  }

  return(data)
}


#' Check if a `NONMEM` table file contains one or more tables in the
#'  specified format
#' @inheritParams nm_file_multi_tab
#' @param check_multiple Logical (T/F). If `TRUE`, check if there are multiple
#'  tables (most single table files still contain the default `table_pattern`).
#' @seealso [nm_file_multi_tab()]
#' @return logical
#' @keywords internal
assert_nm_table_format <- function(
    .path,
    check_multiple = FALSE,
    table_pattern="TABLE NO"
){
  checkmate::assert_file_exists(.path)

  # Determine start of each new table
  x <- stringfish::sf_readLines(.path)
  new_table_def <- stringfish::sf_grepl(x, pattern = table_pattern, fixed = TRUE)

  if(isTRUE(check_multiple)){
    return(sum(new_table_def) > 1)
  }else{
    return(any(new_table_def))
  }
}
