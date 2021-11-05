# Helper functions for copying models, mostly for NONMEM models


#' Update model ID (run number) in the control stream
#'
#' Helper to update the model ID (aka the "run number" if models are named
#' numerically) in the new control stream of a model created with
#' [copy_model_from()]. See Details section.
#'
#' @details This function updates all occurrences of the model ID from
#' the parent model and replaces them with model ID from the
#' new model. Importantly, it relies on the assumption that `.mod` will have the parent model ID as
#' the first entry in `.mod$based_on`. This will be true if `.mod` was created by
#' [copy_model_from()].
#'
#' It will look for that parent model ID before all strings
#' passed to `.suffixes` or `.additional_suffixes` (e.g. `{parent_mod}.MSF`, etc.)
#' replace it with `get_model_id(.mod)` wherever found.
#'
#' `.suffixes` defaults to the following:
#'
#' * `.msf`
#' * `.ext`
#' * `.chn`
#' * `.tab`
#' * `par.tab`
#'
#' All matches are _not_ case sensitive but replacements _are_. For example, if
#' `.msf` is passed as a suffix, `{parent_mod}.MSF` and `{parent_mod}.msf` would
#' _both_ be replaced, but the case of the suffix in the control stream will be
#' maintained as is.
#'
#' @return Invisibly returns `.mod`, to enable piping (i.e. from
#'   `copy_model_from()`)
#'
#' @importFrom readr read_lines write_lines
#' @importFrom checkmate assert_character
#' @importFrom stringr str_replace_all fixed
#'
#' @param .mod The `bbi_nonmem_model` object associated with the control stream
#'   that will be modified.
#' @param .suffixes Character vector of suffixes to be matched for replacement.
#'   Matching is case insensitive (see Details). Note that **if passed suffixes
#'   are file extensions, you must include the leading period** (e.g. pass
#'   `".tab"` instead of `"tab"`). This is to enable passing things like
#'   `"par.tab"` which should (and will) match `{parent_mod}par.tab` but _not_
#'   `{parent_mod}.par.tab`.
#' @param .additional_suffixes Character vector of suffices to match _in
#'   addition to_ those passed in `.suffixes`. `NULL` by default. This is useful
#'   if you want to use the defaults for `.suffixes`, but also add new ones, as
#'   opposed to replacing the defaults. Also, see note in `.suffixes` about file
#'   extensions.
#' @export
update_model_id <- function(
  .mod,
  .suffixes = c(
    '.msf',
    '.ext',
    '.tab',
    '.chn',
    'par.tab'
  ),
  .additional_suffixes = NULL
){
  check_yaml_in_sync(.mod)
  mod_id <- get_model_id(.mod)
  modelfile <- get_model_path(.mod)
  based_on <- .mod$based_on[1]
  if (is.null(based_on)) {
    stop(glue("Cannot call update_model_id() because .mod$based_on is empty for model {mod_id}"))
  }
  message(glue("replacing {based_on} with {mod_id} in {modelfile}"))

  ## construct suffixes regex string
  assert_character(.suffixes)
  assert_character(.additional_suffixes, null.ok = TRUE)
  .suffixes <- .suffixes %>%
    c(.additional_suffixes) %>%
    str_replace_all(fixed("."), "\\.") %>%
    paste(collapse = "|") %>%
    paste0("(", ., ")\\b")

  ## edit text of new model file
  txt <- suppressSpecificWarning(
    read_lines(modelfile),
    .regexpr = "incomplete final line"
  )

  txt <- gsub(
    paste0(based_on, .suffixes),
    paste0(mod_id, "\\1"),
    txt,
    ignore.case = TRUE
  )

  ## write updated model file
  write_lines(txt, modelfile)

  ## return model to make this a pipeable function
  return(invisible(.mod))
}
