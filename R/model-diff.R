#' Compare model files between models
#'
#' By default, this compares a model's model file on disk to the model file of
#' its "parent" model (the model referenced in its `based_on` field).  It can
#' optionally take a second model (passed to `.mod2`), in which case it
#' will compare the first model to the second model, ignoring the `based_on`
#' field entirely.
#'
#' @details
#' **Printing vs. knitting:** When using this function in an `.Rmd` file that
#' you intend to knit to HTML, be sure to **set the chunk option `results =
#' "asis"`** for the relevant chunk. If you do this, the `model_diff()` output
#' will render nicely in color in the HTML doc. Note **this only works for HTML
#' output.** If you are are rendering to a different format (PDF, DOCX, etc.)
#' you can still use `model_diff()` _without_ `results = "asis"` and it will
#' render, although it won't look particularly nice.
#'
#' **`based_on` details:** As described above, if `.mod2` is `NULL` (the default),
#' then the `.mod` will be compared to the model returned by `get_based_on(.mod)`.
#' However, _if there is more than one model_ (or no models) in `.mod$based_on`, then user
#' will be prompted to explicitly pass the model they want to compare against to
#' the `.mod2` argument.
#'
#' @return Returns a `"Diff"` object from the `diffobj` package that renders
#'   when printed or called in the console.
#'
#' @param .mod The `bbi_{.model_type}_model` to compare.
#' @param .mod2 If a `bbi_{.model_type}_model` object is passed, compare `.mod` to
#'   `.mod2`. If `.mod2 = NULL`, the default, compare `.mod` to the model at
#'   `get_based_on(.mod)`. See "`based_on` details" in Details section.
#' @param .file Defaults to `"model"` which compares the default model file for
#'   that model type. For NONMEM models, the control stream is compared. Other
#'   model types may accept additional values and support comparing multiple
#'   files that can be compared. See "`.file` argument" of the specific S3
#'   method for details.
#' @param ... arguments passed through to methods. (Currently none.)
#' @param .viewer If `FALSE`, the default, prints diff to console or renders in
#'   Rmd. If `TRUE`, render the diff in the Viewer window. Note: this option
#'   will hold the console with `Press ENTER to continue...` so it is only
#'   recommended for interactive use.
#' @export
model_diff <- function(.mod, .mod2 = NULL, .file = "model", ..., .viewer = FALSE) {
  UseMethod("model_diff")
}

#' @rdname model_diff
#' @export
model_diff.bbi_nonmem_model <- function(
  .mod,
  .mod2 = NULL,
  .file = c("model"),
  ...,
  .viewer = FALSE
) {
  .file <- match.arg(.file)

  .mod2 <- model_diff_get_comp(.mod, .mod2)

  .file1 <- get_model_path(.mod)
  .file2 <- get_model_path(.mod2)

  model_diff_impl(.file1, .file2, .viewer = .viewer)
}


###################################
# PRIVATE IMPLEMENTATION FUNCTIONS
###################################

# SHARED: model_diff_get_comp() is used by bbr.bayes, so any changes here should
# be compatible with its use there.

#' Private helper to get a valid comparison model
#' @inheritParams model_diff
#' @keywords internal
model_diff_get_comp <- function(.mod, .mod2) {
  check_yaml_in_sync(.mod)

  if (is.null(.mod2)) {
    based_on_path <- get_based_on(.mod)
    mod_id <- get_model_id(.mod)
    if (length(based_on_path) == 0) {
      stop(paste(glue("Model {mod_id} has no models in `based_on`."), MODEL_DIFF_ERR_MSG), call. = F)
    } else if (length(based_on_path) > 1) {
      stop(paste(
        glue("Model {mod_id} has multiple models in `based_on`."),
        MODEL_DIFF_ERR_MSG,
        glue("Models returned from `get_based_on(.mod)`: {paste(based_on_path, collapse = ', ')}")
      ), call. = F)
    } else {
      .mod2 <- read_model(based_on_path)
    }
  }
  check_yaml_in_sync(.mod2)
  return(.mod2)
}

# SHARED: model_diff_impl() is used by bbr.bayes, so any changes here should be
# compatible with its use there.

#' Private helper to diff two model files
#' @importFrom diffobj diffFile
#' @param model_A Path to first model file
#' @param model_B Path to second model file
#' @inheritParams model_diff
#' @keywords internal
model_diff_impl <- function(model_A, model_B, .viewer) {
  # check inputs
  if (!all(c(
    checkmate::check_string(model_A),
    checkmate::check_string(model_B),
    fs::file_exists(c(model_A, model_B))
  ))) {
    dev_error(paste(
      "model_diff_impl() got the following:",
      paste(model_A, collapse =" -- "),
      paste(model_B, collapse =" -- ")
    ))
  }

  if (tools::md5sum(model_A) == tools::md5sum(model_B)) {
    message("Relevant model files are identical")
    return(invisible(NULL))
  }

  # print some warnings if knitting
  knitting <- isTRUE(getOption('knitr.in.progress'))
  if (knitting) {
    requireNamespace("knitr", quietly = TRUE) # technically this option could be set without knitr being installed
    if (!knitr::is_html_output()) {
      warning(paste(
        "bbr::model_diff() may not render legibly in non-HTML knitr output.",
        "Consider setting `output: html_document` if you want to use model_diff()."
      ), call. = FALSE)
    }
    if (isTRUE(.viewer)) {
      warning(paste(
        "bbr::model_diff(.viewer = TRUE) is not supported when knitting an `.Rmd` or similar.",
        "For best results remove this argument and set `results = 'asis'` in your chunk options."
      ), call. = FALSE)
    }
  }

  suppressSpecificWarning({
    # if knitting and `results = "asis"` set, render HTML
    if (knitting &&
        knitr::opts_current$get("results") == 'asis' &&
        knitr::is_html_output()
        ) {
      diffobj::diffFile(
        model_A,
        model_B,
        mode = "sidebyside",
        tar.banner=get_model_id(model_A),
        cur.banner=get_model_id(model_B),
        format = "html",
        style = list(html.output = "diff.w.style"),
        interactive = F
      )
    } else {
      # otherwise, print to either console or viewer
      diffobj::diffFile(
        model_A,
        model_B,
        mode = "sidebyside",
        tar.banner=get_model_id(model_A),
        cur.banner=get_model_id(model_B),
        interactive = .viewer
      )
    }
  }, .regexpr = "incomplete final line")
}
