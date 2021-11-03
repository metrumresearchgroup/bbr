#' Read NONMEM output files
#'
#' Reads in a whitespace-delimited NONMEM output file, for example .grd or .ext
#' or a table output.
#' @param .mod Either a `bbi_nonmem_model`, `bbi_nonmem_summary`, or a path to a
#'   file to read in. If passing model object to `nm_file()`, must also pass `.suffix` that
#'   will be passed through to [build_path_from_model()].
#' @inheritParams build_path_from_model
#' @param ... arguments passed through to methods. (Currently none.)
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
  nm_file(.path)
}

#' @importFrom readr read_table read_table2 cols
#' @export
nm_file.character <- function(.mod, .suffix = NULL, ...) {
  checkmate::assert_string(.mod)
  if (!is.null(.suffix)) {
    stop("Cannot pass .suffix to use nm_file.character; pass only file path to .mod")
  }

  # read file
  if (packageVersion("readr") >= package_version("2.0.0")) {
    read_table(.mod, skip=1, na = ".", col_types = cols())
  } else {
    read_table2(.mod, skip=1, na = ".", col_types = cols())
  }
}

#' @describeIn nm_file Reads `.grd` file from a `bbi_nonmem_model` or
#'   `bbi_nonmem_summary` object
#' @export
nm_grd <- function(.mod) {
  check_model_object(.mod, c(NM_MOD_CLASS, NM_SUM_CLASS))
  nm_file(.mod, ".grd")
}

#' @describeIn nm_file Reads `.ext` file from a `bbi_nonmem_model` or
#'   `bbi_nonmem_summary` object
#' @export
nm_ext <- function(.mod) {
  check_model_object(.mod, c(NM_MOD_CLASS, NM_SUM_CLASS))
  nm_file(.mod, ".ext")
}

#' @describeIn nm_file Looks for `{get_model_id(.mod)}.tab` or `TAB` file in
#'   NONMEM output folder and reads whichever is found.
#' @export
nm_tab <- function(.mod) {
  check_model_object(.mod, c(NM_MOD_CLASS, NM_SUM_CLASS))
  nm_candidates(
    "nm_tab()",
    c(
      file.path(get_output_dir(.mod), "TAB"),
      build_path_from_model(.mod, ".tab")
    )
  )
}

#' @describeIn nm_file Looks for `{get_model_id(.mod)}par.tab` or `PAR_TAB` file
#'   in NONMEM output folder and reads whichever is found.
#' @export
nm_par_tab <- function(.mod) {
  check_model_object(.mod, c(NM_MOD_CLASS, NM_SUM_CLASS))
  nm_candidates(
    "nm_par_tab()",
    c(
      file.path(get_output_dir(.mod), "PAR_TAB"),
      build_path_from_model(.mod, "par.tab")
    )
  )
}

#' Read in the file that exists
#'
#' Helper for [nm_tab()] and [nm_par_tab()]. Looks for all files passed to
#' `.candidates`. If only one of them exists, it reads that file. Otherwise it
#' errors informatively.
#' @param .func name of the calling function (for potential error message)
#' @param .candidates Files to look for
#' @keywords internal
nm_candidates <- function(.func, .candidates) {

  # check that only one of these exists
  .fe <- fs::file_exists(.candidates)
  if (all(.fe)) {
    stop(glue("Cannot use `{.func}` because all of {paste(.candidates, collapse = ' and ')} exist."))
  }
  if (all(!.fe)) {
    stop(glue("Cannot use `{.func}` because none of {paste(.candidates, collapse = ' or ')} exist."))
  }

  .path <- .candidates[which(.fe)]
  verbose_msg(glue("Reading {.path}"))
  nm_file(.path)
}


