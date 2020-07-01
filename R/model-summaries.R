

#' @describeIn model_summaries Takes a `bbi_run_log_df` tibble and returns a new tibble with summary data.
#' **If you want to append these columns to the `bbi_run_log_df` tibble, consider using `add_summary()` instead.
#' @export
model_summaries.bbi_run_log_df <- function(
  .mods,
  .model_type = c("nonmem", "stan"),
  .bbi_args = NULL,
  ...,
  .dry_run = FALSE,
  .directory = get_model_directory()
) {

  # extract paths
  .mod_paths <- get_model_path(.mod)

  # pass to character dispatch
  res_df <- model_summaries(
    .mods = .mod_paths,
    .model_type = .model_type,
    .bbi_args = .bbi_args,
    ...,
    .dry_run = .dry_run,
    .directory = .directory
  )

  return(res_list)
}

