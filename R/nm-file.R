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
#' consider using `data.table::fread()` with the `skip` and `nrows` arguments.
#'
#' @return A tibble with the data from the specified file and estimation method.
#'
#' @param .mod Either a `bbi_nonmem_model`, `bbi_nonmem_summary`, or a path to a
#'   file to read in. If passing model object to `nm_file()`, must also pass `.suffix` that
#'   will be passed through to [build_path_from_model()].
#' @inheritParams build_path_from_model
#' @param ... arguments passed through to methods. (Currently none.)
#' @seealso [nm_tables()], [nm_table_files()], [nm_join()]
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
#' @param .rename If `TRUE`, the default, will rename `.grd` columns to the
#'   relevant parameter names. Otherwise will leave column names as is.
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
#' @importFrom data.table fread
#' @importFrom tibble as_tibble
#' @export
nm_data <- function(.mod) {
  check_model_object(.mod, c(NM_MOD_CLASS, NM_SUM_CLASS, NMSIM_MOD_CLASS))
  .path <- get_data_path(.mod)
  verbose_msg(glue("Reading data file: {basename(.path)}"))
  .d <- fread(.path, na.strings = ".", verbose = FALSE)
  .d <- remove_dup_cols(.d)
  .d <- as_tibble(.d)
  names(.d) <- toupper(names(.d))
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
#' @param header Logical (T/F). Does the first data line contain column names?
#'  Passed to [data.table::fread()].
#' @param simplify Logical (T/F). If `TRUE`, simplify list of tables into a
#'  single tibble. Table names will show up as a new `table_id` column.
#' @param sanitize Logical (T/F). If `TRUE`, sanitize columns to ensure no
#'  remaining column headers or other text pollution makes it into the data.
#' @param ... additional arguments passed to [data.table::fread()]
#' @param table_pattern character string defining the start of a new
#'  table (regex accepted).
#'
#' @note This is modified from pmxTools, which is licensed under GPL-2.
#' @seealso tab_is_multi_table
#' @keywords internal
nm_file_multi_table <- function(
    .path,
    header = TRUE,
    simplify = TRUE,
    sanitize = TRUE,
    ...,
    table_pattern="^TABLE NO"
) {
  verbose_msg(glue("Reading {basename(.path)}"))
  checkmate::assert_file_exists(.path)

  # TODO: do we want this?
  if(!assert_nm_table_format(.path, table_pattern, check_multiple = TRUE)){
    rlang::warn(
      glue("{.path} may not be parsed correctly. Consider changing `table_pattern`")
    )
  }

  # Determine start of each new table
  file_data <- readLines(.path)
  new_tables <- grep(file_data, pattern = table_pattern)

  # make unique for duplicated table names (simulation does)
  file_data[new_tables] <- make.unique(file_data[new_tables])

  table_list <- list()
  for(idx in seq_along(new_tables)){
    # Read in & format each individual table
    start_line <- new_tables[idx] + 1
    end_line <-
      if(idx == length(new_tables)){
        length(file_data)
      }else{
        new_tables[idx + 1] - 1
      }
    current_table_name <- file_data[new_tables[idx]]

    table.i <- withr::with_options(list(warn = 1), {
      data <- data.table::fread(
        .path, header = header, na.strings = ".", verbose = FALSE,
        skip = (start_line - 1), nrows = (end_line - start_line),
        ...
      )
      data <- remove_dup_cols(data)
      as_tibble(data)
    })

    if(isTRUE(sanitize)){
      # Scan to ensure no remaining column headers or other text pollution
      pattern <- "^[[:space:]]*([[:alpha:]].*)"
      indices <- which(!grepl(pattern, table.i[[1]]))
      table.i <- table.i[indices,]
    }

    table_list[[current_table_name]] <- as_tibble(table.i)
  }

  # Format and return
  if(length(length(table_list)) > 1){
    verbose_msg(glue("  tables: {length(table_list)}"))
  }

  if(isTRUE(simplify)){
    table_list <- purrr::list_rbind(table_list, names_to = "table_id") %>%
      dplyr::relocate("table_id", .after = dplyr::everything())

    verbose_msg(glue("  rows: {nrow(table_list)}"))
    verbose_msg(glue("  cols: {ncol(table_list)}"))
  }
  verbose_msg("") # for newline

  return(table_list)
}


#' Check if a `NONMEM` table file contains one or more tables in the
#'  specified format
#' @inheritParams nm_file_multi_table
#' @param check_multiple Logical (T/F). If `TRUE`, check if there are multiple
#'  tables (most single table files still contain the default `table_pattern`).
#' @seealso nm_file_multi_table
#' @return logical
#' @keywords internal
assert_nm_table_format <- function(
    .path,
    check_multiple = FALSE,
    table_pattern="^TABLE NO"
    ){
  checkmate::assert_file_exists(.path)

  # Determine start of each new table
  file_data <- readLines(.path)
  new_table_def <- grepl(file_data, pattern = table_pattern)

  if(isTRUE(check_multiple)){
    return(sum(new_table_def) > 1)
  }else{
    return(any(new_table_def))
  }
}
