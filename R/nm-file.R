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
#' `nm_file()` looks for `TABLE NO` to find the beginning of the requested
#' table. For this reason, `nm_file()` (and family) are _not_ compatible with
#' the `NOHEADER` or `NOTITLE` options in `$TABLE`.
#'
#' @return A tibble with the data from the specified file and estimation method.
#'   All column names will be converted to uppercase.
#'
#' @param .mod Either a `bbi_nonmem_model`, `bbi_nonmem_summary`, or a path to a
#'   file to read in. If passing model object to `nm_file()`, must also pass `.suffix` that
#'   will be passed through to [build_path_from_model()].
#' @param .est_method If `NULL`, the default, pulls the data from the final
#'   estimation method. If an integer, pulls the data from that estimation
#'   method. Can also pass `"fail"` in which case a warning will be raised
#'   and `NULL` returned if more than one table is found in the file. This
#'   is useful for table outputs that should not have multiple tables in a
#'   single file.
#' @inheritParams build_path_from_model
#' @param ... arguments passed through to methods. (Currently none.)
#' @seealso [nm_tables()], [nm_table_files()], [nm_join()]
#' @export
nm_file <- function(.mod, .suffix = NULL, .est_method = NULL, ...) {
  UseMethod("nm_file")
}

#' @export
nm_file.bbi_model <- function(.mod, .suffix = NULL, .est_method = NULL, ...) {
  check_model_object(.mod, c(NM_MOD_CLASS, NM_SUM_CLASS))
  if (is.null(.suffix)) {
    stop("Must pass .suffix to use nm_file.bbi_model")
  }
  .path <- build_path_from_model(.mod, .suffix)
  nm_file(.path, .suffix = NULL, .est_method = .est_method)
}

#' @export
nm_file.character <- function(.mod, .suffix = NULL, .est_method = NULL, ...) {
  checkmate::assert_string(.mod)
  if (!is.null(.suffix)) {
    stop("Cannot pass .suffix to use nm_file.character; pass only file path to .mod")
  }
  nm_file_impl(.mod, .est_method)
}

#' @describeIn nm_file Reads `.grd` file from a `bbi_nonmem_model` or
#'   `bbi_nonmem_summary` object
#' @param .rename If `TRUE`, the default, will rename `.grd` columns to the
#'   relevant parameter names. Otherwise will leave column names as is.
#' @export
nm_grd <- function(.mod, .est_method = NULL, .rename = TRUE) {
  check_model_object(.mod, c(NM_MOD_CLASS, NM_SUM_CLASS))
  grd_df <- nm_file(.mod, .suffix = ".grd", .est_method = .est_method)

  if (isTRUE(.rename)) {
    .s <- model_summary(.mod)

    if (is.null(.est_method)) {
      .est_method <- length(.s$parameters_data)
    }

    lbl <- c(.s$parameter_names$theta[.s$parameters_data[[.est_method]]$fixed$theta==0],
             .s$parameter_names$sigma[.s$parameters_data[[.est_method]]$fixed$sigma==0],
             .s$parameter_names$omega[.s$parameters_data[[.est_method]]$fixed$omega==0])
    names(grd_df) <- c("ITERATION", lbl)
  }
  return(grd_df)
}

#' @describeIn nm_file Reads `.ext` file from a `bbi_nonmem_model` or
#'   `bbi_nonmem_summary` object
#' @export
nm_ext <- function(.mod, .est_method = NULL) {
  check_model_object(.mod, c(NM_MOD_CLASS, NM_SUM_CLASS))
  nm_file(.mod, .suffix = ".ext", .est_method = .est_method)
}

#' @describeIn nm_file Reads `{get_model_id(.mod)}.tab` file from a
#'   `bbi_nonmem_model` or `bbi_nonmem_summary` object
#' @export
nm_tab <- function(.mod) {
  check_model_object(.mod, c(NM_MOD_CLASS, NM_SUM_CLASS))
  nm_file(.mod, .suffix = ".tab", .est_method = "fail")
}

#' @describeIn nm_file Reads `{get_model_id(.mod)}par.tab` file from a
#'   `bbi_nonmem_model` or `bbi_nonmem_summary` object
#' @export
nm_par_tab <- function(.mod) {
  check_model_object(.mod, c(NM_MOD_CLASS, NM_SUM_CLASS))
  nm_file(.mod, .suffix = "par.tab", .est_method = "fail")
}

#' @describeIn nm_file Reads the input data file from a `bbi_nonmem_model` or
#'   `bbi_nonmem_summary` object
#' @importFrom data.table fread
#' @importFrom tibble as_tibble
#' @export
nm_data <- function(.mod) {
  check_model_object(.mod, c(NM_MOD_CLASS, NM_SUM_CLASS))
  .path <- get_data_path(.mod)
  verbose_msg(glue("Reading data file: {basename(.path)}"))
  .d <- as_tibble(fread(.path, na.strings = ".", verbose = FALSE))
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
#' @keywords internal
nm_file_impl <- function(.path, .est_method) {
  # read file and find top of table
  verbose_msg(glue("Reading {basename(.path)}"))

  # get line numbers of TABLE lines
  checkmate::assert_file_exists(.path)
  .est <- processx::run(
      "grep",
      c("-n", "^ *TABLE NO", .path),
      error_on_status = FALSE # grep returns 1 if line not found
    )$stdout %>%
    str_split("\n") %>%
    unlist() %>%
    str_replace("\\:.*", "") %>%
    as.integer()
  .est <- .est[!is.na(.est)]

  if (length(.est) == 0) {
    stop(paste(
      "Found no 'TABLE NO...' lines in file. Perhaps not a NONMEM output file?",
      "Note: using NOHEADER or NOTITLE in table files can cause this. bbr::nm_file() is NOT compatible with these options.",
      sep = "\n"
    ))
  }

  if (is.null(.est_method)) {
    .est_method <- length(.est)
  }
  if (.est_method == "fail") {
    if (length(.est) > 1) {
      warning(glue("{.path} has {length(.est)} tables in it, but .est_method='fail' expects only one table per file."), call. = FALSE)
      return(invisible(NULL))
    } else {
      .est_method <- 1
    }
  }

  checkmate::assert_integerish(.est_method, lower = 1, upper = length(.est), len = 1, null.ok = TRUE)

  # parse to tibble
  .start <- .est[.est_method]
  .end <- if (.est_method == length(.est)) {
    Inf
  } else {
    .est[.est_method+1] # line before next estimation method
  }

  .d <- suppressSpecificWarning({
    as_tibble(fread(
      .path,
      na.strings = ".",
      skip = .start,
      nrows = .end - .start - 1, # if there's a header this should be -2 but see note below
      verbose = FALSE
    ))},
    .regexpr = "Stopped early.+TABLE NO"
  )
  # Note: we don't check for a header, but fread handles that either way
  # however, doing -1 above means we'll never lose a row by accident (if there's no header)
  # and we just catch the warning that you get for doing -1 when there is a header.
  # Note also that NOHEADER breaks nm_file() (see error ^) so this is just for NOLABEL

  names(.d) <- toupper(names(.d))
  verbose_msg(glue("  rows: {nrow(.d)}"))
  verbose_msg(glue("  cols: {ncol(.d)}"))
  verbose_msg("") # for newline
  return(.d)
}
