#' Summarize multiple models in batch
#'
#' Run multiple `model_summary()` calls in batch.
#' All these dispatches will return a list of `bbi_{.model_type}_summary` objects.
#'
#' @details
#' The summary call will error if it does not find certain files in the output folder.
#' However, you can override this behavior with the following file-specific flags:
#'  * `no_cor_file`
#'  * `no_cov_file`
#'  * `no_ext_file`
#'  * `no_grd_file`
#'  * `no_shk_file`
#'
#' If some of your runs are using an estimation method that does not produce any of the following files,
#' or they are missing for some other legitimate reason, pass the appropriate flags through the `.fail_flags` argument.
#' For example, if have asked to skip the `$COV` step, you would call `model_summary(..., .fail_flags = list(no_cov_file = TRUE))`.
#'
#' Additionally, **if you have renamed your `.ext` file, you will need to pass the new name through**, to `.bbi_args` or `.fail_flags` like so:
#' `model_summaries(..., .bbi_args = list(ext_file = "whatever_you_named_it"))`
#'
#' @param .mods The model object to summarize. Could be
#' a `bbi_run_log_df` tibble,
#' a list of `bbi_{.model_type}_model ` objects,
#' a character vector of file paths to models,
#' a numeric vector of integers corresponding to a file names of a models.
#' @param .fail_flags Same as `.bbi_args` except these are used _only_ when a `model_summary()` call fails.
#' In that case, flags are appended to anything in `.bbi_args` and the summary is tried again.
#' Defaults to `list(no_grd_file = TRUE, no_shk_file = TRUE)`.
#' This is a "dumb" way of automatically catching Bayesian or something else that doesn't produce certain output files.
#' See details section for more info on these flags.
#' @inheritParams model_summary
#' @export
model_summaries <- function(
  .mods,
  .bbi_args = NULL,
  .fail_flags = list(no_grd_file = TRUE, no_shk_file = TRUE), ##### WE DONT WANT THIS SOLUTION
  ...,
  .dry_run = FALSE,
  .directory = NULL
) {
  UseMethod("model_summaries")
}

#' @describeIn model_summaries Summarize a list of `bbi_{.model_type}_model` objects.
model_summaries.list <- function(
  .mods,
  .bbi_args = NULL,
  .fail_flags = list(no_grd_file = TRUE, no_shk_file = TRUE), ##### WE DONT WANT THIS SOLUTION
  ...,
  .dry_run = FALSE,
  .directory = NULL
) {

  if (!is.null(.directory)) {
    warning(paste(glue("Passed `.directory = {.directory}` to model_summaries.list().") ,
                  "This argument is only valid when passing a path to `model_summaries()`.",
                  "Directory will be extracted from each model object."))
  }

  # check that each element is a model object
  check_model_object_list(.mods)

  res_list <- map(.mods, function(.m) {
    .s <- tryCatch(
      {
        model_summary(.m, .bbi_args = .bbi_args)
      },
      error = function(.e) {
        # if fails, try again with flags
        tryCatch({
          if (!is.null(.bbi_args)) {
            .fail_flags <- combine_list_objects(.fail_flags, .bbi_args)
          }
          .retry <- rbabylon::model_summary(.m, .bbi_args = .fail_flags)
          .retry$needed_fail_flags <- TRUE
          return(.retry)
        },
        error = function(.e) {
          .error_msg <- paste(as.character(.e$message), collapse = " -- ")
          return(list(error_msg = .error_msg))
        })
      }
    )
    return(list(
      absolute_model_path = tools::file_path_sans_ext(get_model_path(.m)),
      error_msg = .s$error_msg %||% NA_character_,
      needed_fail_flags = .s$needed_fail_flags %||% FALSE,
      bbi_summary = .s
      #bbi_summary = ifelse(is.null(.s$error_msg), .s, NA)
    ))
  })

  return(res_list)
}


#' @describeIn model_summaries Takes a character vector of file paths to models and summarizes all of them.
#' @importFrom purrr map map_chr
#' @export
model_summaries.character <- function(
  .mods,
  .bbi_args = NULL,
  .fail_flags = list(no_grd_file = TRUE, no_shk_file = TRUE), ##### WE DONT WANT THIS SOLUTION
  ...,
  .dry_run = FALSE,
  .directory = get_model_directory()
) {

  # load models to summarize
  .mods <- map_chr(.mods, ~combine_directory_path(.directory, .x))
  .mods <- map(.mods, ~read_model(.x))

  # pass to list dispatch
  res_df <- model_summaries(
    .mods = .mods,
    .bbi_args = .bbi_args,
    .fail_flags = .fail_flags,
    ...,
    .dry_run = .dry_run,
    .directory = NULL
  )

  return(res_df)
}


#' @describeIn model_summaries Takes a numeric vector of file paths to models and summarizes all of them.
#' This will only work if you are calling from the same directory as the models, or if you have set `options('rbabylon.model_directory')` to the directory constaining the relevant model.
#' @export
model_summaries.numeric <- function(
  .mods,
  .bbi_args = NULL,
  .fail_flags = list(no_grd_file = TRUE, no_shk_file = TRUE), ##### WE DONT WANT THIS SOLUTION
  ...,
  .dry_run = FALSE,
  .directory = get_model_directory()
) {

  .mods <- as.character(.mods)

  # pass to character dispatch
  res_df <- model_summaries(
    .mods = .mods,
    .bbi_args = .bbi_args,
    .fail_flags = .fail_flags,
    ...,
    .dry_run = .dry_run,
    .directory = .directory
  )

  return(res_df)
}


#' @describeIn model_summaries Takes a `bbi_run_log_df` tibble and summarizes all models in it.
#' @export
model_summaries.bbi_run_log_df <- function(
  .mods,
  .bbi_args = NULL,
  .fail_flags = list(no_grd_file = TRUE, no_shk_file = TRUE), ##### WE DONT WANT THIS SOLUTION
  ...,
  .dry_run = FALSE,
  .directory = NULL
) {

  if (!is.null(.directory)) {
    warning(paste(glue("Passed `.directory = {.directory}` to model_summaries.bbi_run_log_df().") ,
                  "This argument is only valid when passing a path to `model_summaries()`.",
                  "Directory will be extracted from each model object."))
  }

  # extract paths
  .mod_paths <- get_model_path(.mods)

  # pass to character dispatch
  res_df <- model_summaries(
    .mods = .mod_paths,
    .bbi_args = .bbi_args,
    .fail_flags = .fail_flags,
    ...,
    .dry_run = .dry_run,
    .directory = .directory
  )

  return(res_df)
}

