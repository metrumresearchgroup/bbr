#' Summarize multiple models in batch
#'
#' Run multiple [model_summary()] calls in batch. Note: if you need to pull in
#' _only_ the parameter estimates for a large number of NONMEM models, consider
#' using [param_estimates_batch()] instead, as it will be faster than
#' `model_summaries()` for this purpose.
#'
#' @details
#' The summary call will error if it does not find certain files in the output folder.
#' However, you can override this behavior with the following file-specific flags:
#'  * `no_ext_file`
#'  * `no_grd_file`
#'  * `no_shk_file`
#'
#' If some of your runs are using an estimation method that does not produce any of the following files,
#' or they are missing for some other legitimate reason, pass the appropriate flags through the `.fail_flags` argument.
#'
#' Additionally, **if you have renamed your `.ext` file, you will need to pass the new name through**, to `.bbi_args` or `.fail_flags` like so:
#' `model_summaries(..., .bbi_args = list(ext_file = "whatever_you_named_it"))`
#'
#' @return All dispatches will return a list of `bbi_{.model_type}_summary` objects.
#'
#' @param .mods The model objects to summarize. Could be
#' a `bbi_run_log_df` tibble, or
#' a list of `bbi_{.model_type}_model` objects.
#' @param .fail_flags Same as `.bbi_args` except these are used _only_ when a [model_summary()] call fails.
#' In that case, flags are appended to anything in `.bbi_args` and the summary is tried again.
#' See details section for more info on these flags.
#' @seealso [model_summary()], [summary_log()], [param_estimates_batch()]
#' @inheritParams model_summary
#' @export
model_summaries <- function(
  .mods,
  .bbi_args = NULL,
  .fail_flags = NULL,
  ...,
  .dry_run = FALSE
) {
  UseMethod("model_summaries")
}

# model_summaries_serial() can go away once bbr.bbi_min_version is 3.2.0 or
# later.

#' @importFrom purrr map
model_summaries_serial <- function(.mods, .bbi_args, .fail_flags) {
  format_error <- function(err) {
    return(list(error_msg = paste(as.character(err$message),
                                  collapse = " -- ")))
  }

  map(.mods, function(.m) {
    .s <- tryCatch(
      {
        model_summary(.m, .bbi_args = .bbi_args)
      },
      error = function(.e) {
        if (is.null(.fail_flags)) {
          return(format_error(.e))
        }
        # if fails, try again with flags
        tryCatch({
          if (!is.null(.bbi_args)) {
            .fail_flags <- combine_list_objects(.fail_flags, .bbi_args)
          }
          .retry <- bbr::model_summary(.m, .bbi_args = .fail_flags)
          .retry$needed_fail_flags <- TRUE
          return(.retry)
        },
        error = format_error)
      }
    )

    res <- list()
    res[[ABS_MOD_PATH]] = tools::file_path_sans_ext(get_model_path(.m))
    res[[SL_SUMMARY]] = .s
    res[[SL_ERROR]] = .s$error_msg %||% NA_character_
    res[[SL_FAIL_FLAGS]] = .s$needed_fail_flags %||% FALSE

    return(res)
  })
}

bbi_exec_model_summaries <- function(args, paths) {
  args <- c(args %||% list(), json = TRUE)
  cmd <- bbi_exec(c("nonmem", "summary", check_bbi_args(args), paths),
                  .wait = TRUE, .check_status = FALSE)
  res <- jsonlite::fromJSON(paste(cmd[["stdout"]], collapse = ""),
                            simplifyDataFrame = FALSE)
  if (length(paths) == 1) {
    # If passed a single model, bbi will output a single object rather than a
    # list of objects. Reshape it into the same form for downstream code.
    res <- list(Results = list(res),
                Errors = if (res$success) list() else 0)
  }

  return(res)
}

#' Summarize multiple models via single call to bbi
#'
#' `bbi nonmem summary` accepts more than one model as of v2.3.0, and v3.2.0
#' reworked the output so that the errors are included in the JSON structure.
#' This function consumes the v3.2.0 output, returning an object that can be
#' passed to [create_summary_list()].
#'
#' @importFrom purrr map_chr modify_at modify_if walk
#' @keywords internal
model_summaries_concurrent <- function(.mods, .bbi_args, .fail_flags) {
  walk(.mods, check_yaml_in_sync)
  paths <- map_chr(
    .mods,
    # Take relative paths to minimize argument length.
    ~ fs::path_rel(build_path_from_model(.x, ".lst")))

  summaries <- bbi_exec_model_summaries(.bbi_args, paths)
  if (length(summaries$Errors) > 0 && !is.null(.fail_flags)) {
    idx_failed <- summaries$Errors + 1
    args_retry <- .fail_flags
    if (!is.null(.bbi_args)) {
      args_retry <- combine_list_objects(args_retry, .bbi_args)
    }
    summaries_retry <- bbi_exec_model_summaries(args_retry, paths[idx_failed])
    summaries_retry$Results <- modify_if(summaries_retry$Results,
                                         ~ .x$success,
                                         ~ c(.x, needed_fail_flags = TRUE))
    if (length(summaries_retry$Errors) > 0) {
      summaries$Errors <- summaries$Errors[summaries_retry$Errors + 1]
    } else {
      summaries$Errors <- list()
    }
    summaries$Results[idx_failed] <- summaries_retry$Results
  }

  if (length(summaries$Errors) > 0) {
    summaries$Results <- modify_at(
      summaries$Results,
      .at = summaries$Errors + 1,
      ~ {
        # As of bbi v3.2.0, we get the errors from the JSON returned `bbi nonmem
        # summary`, and that includes a default-value object for run_details and
        # run_heuristics. Set it to the value that downstream code expects.
        .x[[SUMMARY_DETAILS]] <- NA
        .x[[SUMMARY_HEURISTICS]] <- NA
        .x
      })
  }

  # Reshape the results into the form expected by create_summary_list().
  n_results <- length(.mods)
  results <- vector(mode = "list", length = n_results)
  for (i in seq_len(n_results)) {
    s <- summaries$Results[[i]]
    res <- list()
    res[[ABS_MOD_PATH]] <- tools::file_path_sans_ext(get_model_path(.mods[[i]]))
    results[[i]] <- c(
      res,
      rlang::list2(
        !!SL_SUMMARY := create_summary_object(c(res, s), "nonmem"),
        !!SL_FAIL_FLAGS := s$needed_fail_flags %||% FALSE,
        !!SL_ERROR := if (s$success) NA_character_ else s$error_msg))
  }

  return(results)
}

#' @describeIn model_summaries Summarize a list of `bbi_{.model_type}_model` objects.
#' @export
model_summaries.list <- function(
  .mods,
  .bbi_args = NULL,
  .fail_flags = NULL,
  ...,
  .dry_run = FALSE
) {
  # check that each element is a model object
  check_model_object_list(.mods)
  res_list <- if (test_bbi_version(.min_version = "3.2.0") &&
                    purrr::every(.mods, ~ inherits(.x, NM_MOD_CLASS))) {
    model_summaries_concurrent(.mods, .bbi_args, .fail_flags)
  } else {
    model_summaries_serial(.mods, .bbi_args, .fail_flags)
  }

  return(create_summary_list(res_list))
}

#' @describeIn model_summaries Takes a `bbi_run_log_df` tibble and summarizes all models in it.
#' @export
model_summaries.bbi_run_log_df <- function(
  .mods,
  .bbi_args = NULL,
  .fail_flags = NULL,
  ...,
  .dry_run = FALSE
) {

  # extract models
  .mod_paths <- get_model_path(.mods)
  .mod_list <-
    .mod_paths %>%
    purrr::map(fs::path_ext_remove) %>%
    purrr::map(read_model)

  # pass to character dispatch
  res_df <- model_summaries(
    .mods = .mod_list,
    .bbi_args = .bbi_args,
    .fail_flags = .fail_flags,
    ...,
    .dry_run = .dry_run
  )

  return(res_df)
}


###################
# helper functions
###################

#' Convert object to `bbi_summary_list`
#'
#' This is used to convert an object containing `bbi_{.model_type}_summary` objects into a `bbi_summary_list`.
#' Currently it is only used for converting a `bbi_summary_log_df` into a `bbi_summary_list`
#' (primarily so that it can more easily be mapped over), but theoretically it could be used for other purposes in the future.
#' Note this is primarily intended as a developer function, though it was exposed because users may have a use for it as well.
#' @param .sums Object to convert.
#' @export
as_summary_list <- function(.sums) {
  UseMethod("as_summary_list")
}

#' @describeIn as_summary_list Convert a `bbi_summary_log_df` into a `bbi_summary_list`
#' @importFrom dplyr group_split select row_number
#' @importFrom purrr map
#' @export
as_summary_list.bbi_summary_log_df <- function(.sums) {
  .sums <- .sums[SUMMARY_LIST_REQ_KEYS]

  # create list of lists
  .sum_list <- group_split(.sums, rn = row_number())
  .sum_list <- map(.sum_list, function(.row) {
    .row <- select(.row, -"rn")
    .row <- as.list(.row)
    .row[[SL_SUMMARY]] <- .row[[SL_SUMMARY]][[1]] # this gets buried one level deep by the as.list call
    return(.row)
  })

  .sum_list <- create_summary_list(.sum_list)
  return(.sum_list)
}

