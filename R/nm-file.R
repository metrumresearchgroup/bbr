#' Read NONMEM files
#'
#' Reads in a whitespace-delimited NONMEM output file (for example .grd or .ext
#' or a table output) or a NONMEM input data file.
#'
#' @return A tibble with the data from the specified file and estimation method.
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
#' @param .sep Single character used to separate fields within a record. Passed
#'   through to `readr::read_delim()`. Defaults to `","`.
#' @importFrom readr read_delim cols
#' @export
nm_data <- function(.mod, .sep = ",") {
  check_model_object(.mod, c(NM_MOD_CLASS, NM_SUM_CLASS))
  .path <- get_data_path(.mod)
  verbose_msg(glue("Reading {.path}"))
  read_delim(.path, delim =.sep, na = ".", col_types = readr::cols())
}


#####################################
# PRIVATE HELPERS AND IMPLEMENTATION

#' Implementation function for reading NONMEM files
#' @importFrom readr read_table read_table2 cols read_lines
#' @importFrom stringr str_detect
#' @keywords internal
nm_file_impl <- function(.path, .est_method) {
  # read file and find top of table
  .txt <- read_lines(.path)
  .est <- which(str_detect(.txt, "^ *TABLE NO"))

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
  .start <- .est[.est_method] + 1
  .end <- if (.est_method == length(.est)) {
    length(.txt) # end of file
  } else {
    .est[.est_method+1] - 1 # line before next estimation method
  }

  .est_lines <- .txt[.start:.end]
  if (utils::packageVersion("readr") >= package_version("2.0.0")) {
    read_table(I(.est_lines), na = ".", col_types = cols())
  } else {
    read_table2(I(.est_lines), na = ".", col_types = cols())
  }
}
