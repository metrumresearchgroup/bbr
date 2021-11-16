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
#' consider using
#' [PKPDmisc::read_nonmem](https://metrumresearchgroup.github.io/PKPDmisc/reference/read_nonmem.html).
#'
#' @return A tibble with the data from the specified file and estimation method.
#'   All column names will be converted to uppercase.
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
nm_file_impl <- function(.path) {
  # read file and find top of table
  verbose_msg(glue("Reading {basename(.path)}"))

  # get line numbers of TABLE lines
  checkmate::assert_file_exists(.path)

  # read the file, but catch warning that tells us there are multiple tables
  W <- NULL
  .d <- withCallingHandlers({
    as_tibble(fread(
      .path,
      na.strings = ".",
      skip = 1,
      verbose = FALSE
    ))
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
  names(.d) <- toupper(names(.d))
  verbose_msg(glue("  rows: {nrow(.d)}"))
  verbose_msg(glue("  cols: {ncol(.d)}"))
  verbose_msg("") # for newline
  return(.d)
}
