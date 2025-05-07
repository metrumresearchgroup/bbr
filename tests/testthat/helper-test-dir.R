#' Create a temporary directory with dummy NONMEM models
#'
#' Populate a temporary directory with a tree of models derived from
#' inst/model/nonmem/basic/1. This includes copying a subset of files from the
#' run directory and changing the base name to match the new model name. The
#' resulting model is invalid in ways (e.g., the bbi_config.json is for the
#' original run), but it is useful for testing functionality that requires
#' completed runs but does not rely on the invalid details.
#'
#' The layout parameter defines the directory layout and allows specifying a
#' minimum number of adjustments to the model files. It should be a list of
#' lists. The inner list supports the following keys:
#'
#'  * subpath: Path of the model relative to the temporary directory (e.g., "01"
#'    or "sub/01"). This is the only required key.
#'
#'  * based_on: A character vector specifying a based_on value to inject into
#'    the model's yaml file.
#'
#'  * data_set: Value to substitute for the data set path in $DATA line of the
#'    .lst file.
#'
#'  * final_est_method: Value to substitute for the final #METH line of the .lst
#'    file.
#'
#'  * number_of_obs: Value to substitute for the "TOT. NO. OF OBS RECS" value
#'    reported in the .lst file.
#'
#'  * ofv_no_constant: Value to substitute for the #OBJV value reported in the
#'    .lst file.
#'
#'  * ofv_with_constant: Value to substitute for the
#'    "OBJECTIVE FUNCTION VALUE WITH CONSTANT" value reported in the .lst file.
#'
#' The following call, for example, would return the path to a temporary
#' directory with models 1 and 2, recording model 1 as the parent of model 2:
#'
#'     local_model_dir_nonmem_dummies(
#'       list(
#'         list(subpath = "01"),
#'         list(subpath = "02", based_on = "01")
#'       )
#'     )
#'
#' The clean and .local_envir arguments are passed to withr::local_tempdir.
local_model_dir_nonmem_dummies <- function(
  layout,
  clean = TRUE,
  .local_envir = parent.frame()
) {
  tdir <- withr::local_tempdir(
    pattern = "bbr-tests-model-dir-",
    clean = clean,
    .local_envir = .local_envir
  )
  # Call normalizePath to align with ABS_MOD_PATH handling (see new_model and
  # read_model).
  tdir <- normalizePath(tdir)
  mdir_ref <- system.file(
    "model",
    "nonmem",
    "basic",
    "1",
    package = "bbr",
    mustWork = TRUE
  )
  ctl_ref <- paste0(mdir_ref, ".ctl")
  yaml_ref <- paste0(mdir_ref, ".yaml")

  # These are extensions for files that `bbi nonmem summary` pulls in.
  exts_to_copy <- c(".ext", ".grd", ".lst", ".shk")
  for (x in layout) {
    subpath <- x[["subpath"]]
    if (is.null(subpath)) {
      stop("layout item must include 'subpath'")
    }
    name <- basename(subpath)
    mdir <- file.path(tdir, subpath)

    fs::dir_create(mdir)
    fs::file_copy(
      file.path(mdir_ref, paste0(basename(mdir_ref), exts_to_copy)),
      file.path(mdir, paste0(name, exts_to_copy))
    )
    fs::file_copy(
      file.path(mdir_ref, "bbi_config.json"),
      mdir
    )
    fs::file_copy(ctl_ref, paste0(mdir, ".ctl"))

    yaml_file <- paste0(mdir, ".yaml")
    fs::file_copy(yaml_ref, yaml_file)

    based_on <- x[["based_on"]]
    if (!is.null(based_on)) {
      minfo <- yaml::read_yaml(yaml_file)
      minfo[["based_on"]] <- as.list(based_on)
      yaml::write_yaml(minfo, yaml_file)
    }

    update_lst(
      file.path(mdir, paste0(name, ".lst")),
      dataset = x[["data_set"]],
      final_method = x[["final_est_method"]],
      nobs = x[["number_of_obs"]],
      ofv_nc = x[["ofv_no_constant"]],
      ofv_wc = x[["ofv_with_constant"]]
    )
  }

  return(tdir)
}

update_lst <- function(
  fname,
  dataset = NULL,
  final_method = NULL,
  nobs = NULL,
  ofv_nc = NULL,
  ofv_wc = NULL
) {
  lines <- readr::read_lines(fname)

  if (!is.null(dataset)) {
    lines <- replace_item(
      lines,
      "$DATA",
      paste("$DATA", dataset)
    )
  }

  if (!is.null(final_method)) {
    idxs <- grep("#METH: ", lines, fixed = TRUE)
    if (!length(idxs)) {
      stop("lst file does not contain a #METH: line")
    }
    lines[max(idxs)] <- paste(" #METH: ", final_method)
  }

  if (!is.null(nobs)) {
    lines <- replace_item(
      lines,
      "TOT. NO. OF OBS RECS: ",
      sprintf(" TOT. NO. OF OBS RECS:      %d", nobs)
    )
  }

  if (!is.null(ofv_nc)) {
    lines <- replace_item(
      lines,
      "#OBJV:",
      sprintf(
        " #OBJV:********************************************     %f       **************************************************",
        ofv_nc
      )
    )
  }

  if (!is.null(ofv_wc)) {
    lines <- replace_item(
      lines,
      "OBJECTIVE FUNCTION VALUE WITH CONSTANT:",
      sprintf(" OBJECTIVE FUNCTION VALUE WITH CONSTANT:       %f", ofv_wc)
    )
  }

  readr::write_lines(lines, fname)
}

#' Find the single item in xs that contains substring and replace it with value.
#' Abort unless there is one and only one match.
replace_item <- function(xs, substring, value) {
  idx <- grep(substring, xs, fixed = TRUE)
  if (!length(idx)) {
    stop("No matches substring: ", substring)
  }
  if (length(idx) > 1) {
    rlang::abort(c(
      sprintf("got more than one match for substring '%s'", substring),
      xs[idx]
    ))
  }

  xs[idx] <- value
  return(xs)
}
