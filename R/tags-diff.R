#' Compare tags between models
#'
#' By default, this compares a model's tags to the tags on any of its "parent"
#' models. A parent model is any model referenced in the relevant model's
#' `based_on` field.  The `bbi_model` dispatch can optionally take a second
#' model, in which case it will compare the first model to the second model,
#' ignoring the `based_on` field entirely.
#'
#' @return
#' **In all cases:**
#'
#' * `tags_added` contains any tags that are on the relevant model, but _not_ on
#'   any of the models it is based on.
#'
#' * `tags_removed` contains any tags that are on at least one of the models it
#'   is based on, but _not_ on the relevant model.
#'
#' **`tags_diff.bbi_model()`** invisibly returns a list with two elements:
#'   `tags_added` and `tags_removed`. The list is returned invisibly because, by
#'   default, a nicely formatted version of the same information is printed to the
#'   console. User can pass `.print = FALSE` to turn this off.
#'
#' **`tags_diff.bbi_run_log_df()`** returns a named list of lists, with one
#'   element for each row in the input tibble, with the name corresponding to the
#'   value in the `run` column for that row. Each element of the list will contain
#'   the two-element list returned from `tags_diff.bbi_model()` (described above)
#'   for the relevant model.
#'
#' **`add_tags_diff()`** returns the same tibble that was passed to it, but
#'   it, but with two additional columns `tags_added` and `tags_removed`
#'   appended.
#'
#' @param .bbi_object The object to compare. Could be a
#'   `bbi_{.model_type}_model` object or a
#'   tibble of class `bbi_run_log_df`.
#' @param .mod2 If a `bbi_{.model_type}_model` object is passed, compare `.bbi_object`
#'   to `.mod2`. If `.mod2 = NULL`, the default, compare the `.bbi_object` model to
#'   any models in its `based_on` field. **Only valid for `bbi_model`
#'   method.**
#' @param .print If `TRUE`, the default, will print a nicely formatted
#'   version of the returned list to the console. **Only valid for `bbi_model`
#'   method.**
#' @param ... arguments passed through to methods. (Currently none.)
#' @export
tags_diff <- function(.bbi_object, .mod2 = NULL, .print = TRUE, ...) {
  UseMethod("tags_diff")
}

#' @importFrom purrr map
#' @export
tags_diff.bbi_model <- function(.bbi_object, .mod2 = NULL, .print = TRUE, ...) {
  # check that it's a model and not a summary, etc.
  check_yaml_in_sync(.bbi_object)
  mod_name <- get_model_id(.bbi_object)
  mod_tags <- .bbi_object[[YAML_TAGS]]

  # collect tags to compare
  if (!is.null(.mod2)) {
    check_model_object(.mod2)
    compare_name <- get_model_id(.mod2)
    compare_tags <- .mod2[[YAML_TAGS]]
  } else {
    # collect tags from based_on
    compare_name <- "parent(s)"
    compare_tags <- map(get_based_on(.bbi_object), function(.b) {
      .m <- read_model(.b)
      .m[[YAML_TAGS]]
    }) %>%
      unlist() %>%
      unique()
  }

  # compare tags, print, and return
  mod_diff     <- setdiff(mod_tags, compare_tags) %||% ""
  compare_diff <- setdiff(compare_tags, mod_tags) %||% ""

  if (isTRUE(.print)) {
    cat(paste(
      glue("In {mod_name} but not {compare_name}:\t{paste(mod_diff, collapse = ', ')}\n"),
      glue("In {compare_name} but not {mod_name}:\t{paste(compare_diff, collapse = ', ')}\n"),
      sep = "\n"
    ))
  }

  invisible(rlang::list2(
    !!TAGS_ADD := mod_diff,
    !!TAGS_REM := compare_diff
  ))
}

#' @importFrom purrr map map2
#' @importFrom rlang list2
#' @export
tags_diff.bbi_run_log_df <- function(.bbi_object, .mod2 = NULL, .print = NULL, ...) {
  if (!is.null(.mod2)) {
    warning("`.mod2` is not a valid argument for `tags_diff.bbi_run_log_df()` ignoring passed value.")
  }
  if (!is.null(.print)) {
    warning("`.print` is not a valid argument for `tags_diff.bbi_run_log_df()` ignoring passed value.")
  }

  diff_list <- map(.bbi_object[[ABS_MOD_PATH]], function(.p) {
    read_model(.p) %>%
      tags_diff(.print = FALSE)
  })

  names(diff_list) <- .bbi_object[[RUN_ID_COL]]
  return(diff_list)
}

#' @rdname tags_diff
#' @importFrom purrr map
#' @importFrom checkmate assert_logical assert_true makeAssertCollection
#' @param .log_df a `bbi_run_log_df` tibble (the output of [run_log()])
#' @param .collapse logical. Whether or not to collapse the new columns into a character string
#' @param .format Can either be `NULL` or `'html'`. If set to `'html'`, return an additional column formatted for html rendering. See @details.
#'
#' @details
#'
#' If `.format` is set to `'html'`, a new column (`tags_diff`), will be added that combines `tags_added` and `tags_removed`.
#' This new column will surround the removed tags with strikethrough html (`<s>`/`</s>`). For this to take effect when rendering the table
#' as html, you *must* specify **`escape = FALSE`** in the knitr call (see @examples).
#' Additionally, `.collapse` will automatically be set to `TRUE` if `.format` is set to `'html'`.
#'
#' @examples
#' \dontrun{
#' log_df <- bbr::run_log("inst/model/nonmem/basic")
#'
#' log_html <- log_df %>% add_tags_diff(.format = "html") %>% dplyr::relocate(tags_diff)
#'
#' log_html %>% knitr::kable(escape = F) %>% kableExtra::kable_styling()
#' }
#'
#' @export
add_tags_diff <- function(.log_df, .collapse = FALSE, .format = NULL) {

  check_bbi_run_log_df_object(.log_df)

  diff_list <- tags_diff(.log_df)

  .log_df <- .log_df %>%
    mutate(
      !!TAGS_ADD := map(diff_list, ~ .x[[TAGS_ADD]]),
      !!TAGS_REM := map(diff_list, ~ .x[[TAGS_REM]])
    )


  assert_logical(.collapse)

  if(!is.null(.format)){
    coll = makeAssertCollection()
    assert_true(.format == "html", add = coll)
    if(!coll$isEmpty()){
      stop("`.format` can only be NULL or 'html'")
    }
    .collapse <- TRUE # must be TRUE if .format is not NULL
  }

  if(.collapse){
    .log_df <- bbr::suppressSpecificWarning(
      collapse_to_string(.log_df, tags_added, tags_removed),
      .regexpr = "collapse_to_string\\(\\) only works on list columns"
    )
  }

  if("html" %in% .format){
    .log_df <- .log_df %>%
      mutate(
        tags_diff = paste0(tags_added, "; <s>", tags_removed, "</s>") %>%
          str_replace("; <s></s>","") %>%
          str_replace("^; ", "")
      )
  }
  return(.log_df)
}

