#' Create a boostrap model object from an existing model
#'
#' @param .mod a `bbr` model object
#' @param .suffix a suffix for the bootstrap run directory. Will be prefixed by
#'  the model id of `.mod`.
#' @inheritParams copy_model_from
#' @param remove_cov,remove_tables Logical (T/F). Optionally remove `$COVARIANCE`
#' and `$TABLE` records respectively, allowing for notably faster run times.
#'
#' @seealso setup_bootstrap_run summarize_bootstrap_run
#' @return S3 object of class `bbi_nmboot_model`.
#' @export
new_bootstrap_run <- function(
    .mod,
    .suffix = "boot",
    .inherit_tags = TRUE,
    .overwrite = FALSE,
    remove_cov = TRUE,
    remove_tables = TRUE
){

  check_model_object(.mod, NM_MOD_CLASS)

  model_dir <- get_model_working_directory(.mod)
  boot_dir <- glue("{get_model_id(.mod)}-{.suffix}")

  boot_run <- copy_model_from(
    .parent_mod = .mod,
    .new_model = boot_dir,
    .add_tags = "BOOTSTRAP_SUBMISSION",
    .inherit_tags = .inherit_tags,
    .update_model_file = TRUE,
    .overwrite = .overwrite
  )

  boot_run[[YAML_MOD_TYPE]] <- "nmboot"
  boot_run <- save_model_yaml(boot_run)

  # Change problem statement
  prob <- glue("Bootstrap run of model {get_model_id(.mod)}")
  modify_prob_statement(boot_run, prob)

  # Optionally remove $TABLE and $COV statements here
  if(isTRUE(remove_cov)) remove_records(boot_run, type = "covariance")
  if(isTRUE(remove_tables)){
    remove_records(boot_run, type = "table")
  }else{
    # Update table names if present (not used, but for consistency)
    boot_run <- update_model_id(boot_run) %>% suppressMessages()
  }

  # Return read-in model to get the updated class as part of the model object
  return(read_model(file.path(model_dir, boot_dir)))
}


#' Set up a bootstrap model run.
#'
#' Creates a new model object and re-sampled dataset per model run.
#'
#' @param .boot_run a `bbi_nmboot_model` object.
#' @param n number of model runs.
#' @param strat_cols columns to maintain proportion for stratification
#' @param seed a seed for sampling the data
#' @param .overwrite logical (T/F) indicating whether or not to overwrite existing
#'  setup for a bootstrap run.
#'
#' @seealso new_bootstrap_run summarize_bootstrap_run
#'
#' @examples
#' \dontrun{
#'
#' # Setup
#' .boot_run <- new_bootstrap_run(.mod)
#' .boot_run <- setup_bootstrap_run(.boot_run, n = 1000, seed = 1234)
#'
#' # Submit
#' submit_model(.boot_run)
#'
#' # Check status of runs during submission
#' get_model_status(.boot_run)
#' }
#' @export
setup_bootstrap_run <- function(
    .boot_run,
    n = 100,
    strat_cols = NULL,
    seed = 1234,
    .overwrite = FALSE
){
  # TODO: make sure all _relevant_ resample_df args are part of this function
  check_model_object(.boot_run, NMBOOT_MOD_CLASS)
  checkmate::assert_number(n, lower = 1)
  checkmate::assert_number(seed)

  boot_dir <- get_output_dir(.boot_run, .check_exists = FALSE)
  boot_data_dir <- file.path(boot_dir, "data")

  # Bootstrap directory setup
  if(!fs::dir_exists(boot_dir) || .overwrite == TRUE){
    if(fs::dir_exists(boot_dir)) fs::dir_delete(boot_dir)
    fs::dir_create(boot_dir)
    fs::dir_create(boot_data_dir)

    # New model setup
    n_seq <- seq(n)
    mod_names <- purrr::map_chr(n_seq, max_char = nchar(n), pad_left)
    mod_paths <- file.path(boot_dir, mod_names)
    orig_mod <- read_model(get_based_on(.boot_run))

    # Store data within run folder and gitignore & ignore individual model files
    f_sep <- .Platform$file.sep
    default_ignore <- paste0(
      c("*.ctl", "*.mod", "*.yaml", paste0(f_sep, "data")), collapse = "\n"
    )
    ignore_models <- paste0(f_sep, mod_names, collapse = "\n")
    ignore_lines <- paste(default_ignore, ignore_models, sep = "\n\n")
    writeLines(ignore_lines, file.path(boot_dir, ".gitignore"))

    # Only include subjects that entered the original problem by default
    can_be_joined <- can_be_nm_joined(orig_mod)
    if(isTRUE(can_be_joined)){
      starting_data <- nm_join(orig_mod) %>% suppressMessages()

      # select only columns from original data set
      starting_data <- starting_data %>%
        dplyr::select(attr(starting_data, "nm_join_origin")$data)

      if ("DV.DATA" %in% names(starting_data)) {
        starting_data <- dplyr::rename(starting_data, "DV" = "DV.DATA")
      }

    }else{
      rlang::inform(
        paste(
          "Defaulting to input data, which may include data that doesn't enter",
          "the final problem (i.e. ignored subjects)"
        )
      )
      starting_data <- nm_data(orig_mod) %>% suppressMessages()
    }

    if(!is.null(strat_cols)){
      checkmate::assert_true(all(strat_cols %in% names(starting_data)))
    }

    boot_args <- list(
      boot_run = .boot_run,
      all_mod_names = mod_names,
      boot_mod_path = get_model_path(.boot_run),
      orig_mod_path = get_model_path(orig_mod),
      orig_mod_id = get_model_id(orig_mod),
      orig_mod_bbi_args = orig_mod$bbi_args,
      orig_data = starting_data,
      strat_cols = strat_cols,
      seed = seed,
      n_samples = n,
      boot_dir = boot_dir,
      boot_data_dir = boot_data_dir,
      overwrite = .overwrite
    )

    # Create model object per boot run
    boot_models <- purrr::map(mod_paths, make_boot_run, boot_args)
    make_boot_spec(boot_models, boot_args)

    # Garbage collect - may help after handling many (potentially large) datasets
    #  - It can be useful to call gc() after a large object has been removed, as
    #    this may prompt R to return memory to the operating system.
    gc()
  }else{
    rlang::abort(
      c(
        glue("Bootstrap run has already been set up at `{boot_dir}`"),
        "pass `.overwrite = TRUE` to overwrite"
      )
    )
  }
  return(invisible(.boot_run))
}


#' Set up a single bootstrap model run
#'
#' @param mod_path absolute model path (no file extension) of a bootstrap model run.
#' @param boot_args list of parameters needed to create a bootstrap model run.
#'
#' @keywords internal
make_boot_run <- function(mod_path, boot_args){

  # Update the user every 100 runs (plus beginning and end)
  new_mod_index <- which(basename(mod_path) == boot_args$all_mod_names)
  if(new_mod_index == 1 || new_mod_index == length(boot_args$all_mod_names) ||
     new_mod_index %% 100 == 0){
    verbose_msg(glue("Sampling run {new_mod_index}/{length(boot_args$all_mod_names)}"))
  }

  # Copy over control stream
  #  - Cant use copy_model_from, as we want these individual model runs to be
  #    regular `bbi_base_model` objects
  boot_mod_path <- boot_args$boot_mod_path
  mod_path_ext <- paste0(mod_path, ".", fs::path_ext(boot_mod_path))
  fs::file_copy(
    boot_mod_path, mod_path_ext, overwrite = boot_args$overwrite
  )


  # Sample data and assign new IDs
  data_new <- resample_df(
    boot_args$orig_data,
    key_cols = "ID", # TODO: should this be a user arg?
    strat_cols = boot_args$strat_cols,
    replace = TRUE
  ) %>% dplyr::rename("OID" = "ID", "ID" = "KEY") %>% # TODO: should this be a user arg?
    dplyr::select(all_of(unique(c(names(boot_args$orig_data), "OID")))) # TODO: should this be a user arg?

  mod_name <- basename(mod_path)
  orig_mod_id <- boot_args$orig_mod_id

  # Write out new dataset
  data_boot_name <- glue("{mod_name}.csv")
  data_path_boot <- file.path(boot_args$boot_data_dir, data_boot_name)
  data.table::fwrite(data_new, data_path_boot , na = '.', quote = FALSE)

  # Set Problem and relative datapath (to be sourced in control stream)
  prob <- glue("Bootstrap run {mod_name} of model {orig_mod_id}")
  data_path_rel <- fs::path_rel(data_path_boot, boot_args$boot_dir) %>%
    adjust_data_path_ext(mod_path = mod_path_ext, reverse = TRUE)

  # Create new model object
  mod <- new_model(
    mod_path,
    .overwrite = boot_args$overwrite,
    .tags = "BOOTSTRAP_RUN",
    .based_on = boot_mod_path,
    .bbi_args = boot_args$orig_mod_bbi_args

  )

  # Overwrite $PROB and $DATA records
  modify_prob_statement(mod, prob)
  modify_data_path_ctl(mod, data_path_rel)

  # Update table names if present
  if(isTRUE(mod_has_record(mod, "table"))){
    mod <- update_model_id(mod) %>% suppressMessages()
  }

  return(mod)
}


#' Store bootstrap run details before submission
#'
#' @param boot_runs list of boostrap model objects created by `make_boot_run()`.
#' @inheritParams make_boot_run
#'
#' @details
#' This is mainly meant to ensure traceability and enhance the portability of
#' bootstrap "model" objects.
#'
#'
#' @keywords internal
make_boot_spec <- function(boot_models, boot_args){
  boot_dir <- boot_args$boot_dir
  json_path <- file.path(boot_dir, "bbr_boot_spec.json")

  if(fs::file_exists(json_path) && isFALSE(boot_args$overwrite)){
    rlang::abort(
      c(
        glue("A bootstrap specification file already exists at `{json_path}`"),
        "i" = "Pass `.overwrite = TRUE` to overwrite."
      )
    )
  }

  overall_boot_spec <- list(
    problem = glue("Bootstrap of {basename(boot_args$orig_mod_path)}"),
    strat_cols = boot_args$strat_cols,
    seed = boot_args$seed,
    n_samples = boot_args$n_samples,
    model_path = get_model_path(boot_args$boot_run),
    based_on_model_path = boot_args$orig_mod_path,
    based_on_data_path = get_data_path(boot_args$boot_run),
    model_md5 = tools::md5sum(get_model_path(boot_args$boot_run)),
    based_on_model_md5 = tools::md5sum(boot_args$orig_mod_path),
    based_on_data_md5 = tools::md5sum(get_data_path(boot_args$boot_run)),
    output_dir = boot_args$boot_run[[ABS_MOD_PATH]]
  )

  boot_run_ids <- purrr::map_chr(boot_models, function(boot_run){
    paste0("run_", basename(boot_run[[ABS_MOD_PATH]]))
  })

  boot_run_spec <- purrr::map(boot_models, function(boot_run){
    list(
      mod_path = get_model_path(boot_run) %>% fs::path_rel(boot_dir),
      yaml_path = get_yaml_path(boot_run) %>% fs::path_rel(boot_dir)
    )
  }) %>% stats::setNames(boot_run_ids)

  spec_lst <- c(
    bootstrap_spec = list(overall_boot_spec),
    bootstrap_runs = list(boot_run_spec)
  )

  spec_lst_json <- jsonlite::toJSON(spec_lst, pretty = TRUE, simplifyVector = TRUE)
  writeLines(spec_lst_json, json_path)
  return(invisible(json_path))
}


#' Summarize a bootstrap run
#'
#' @inheritParams setup_bootstrap_run
#' @param force_resummarize logical (T/F). If `TRUE`, force re-summarization.
#'  Will _only_ update the saved out `RDS` file when specified via
#'  `summarize_bootstrap_run()`. See details for more information.
#'
#' @details
#'  - `bootstrap_estimates()` _quickly_ extracts and formats the parameter estimates
#'  from each model run. If the data was previously summarized, the data will be
#'  read in instead of re-executing (this can be overridden via
#'  `force_resummarize = TRUE`).
#'  - `summarize_bootstrap_run()` does the following things:
#'     - Tabulates run details and heuristics.
#'     - Calls `summary_log()` and binds the results to the parameter estimates.
#'       - `bootstrap_estimates()` will include this appended model summary
#'       information if a `boot_summary.RDS` data file exists.
#'     - Either saves this data out to `boot_summary.RDS`, or reads it in if it
#'     already exists (see section below).
#'     - Formats the returned object as a `bbi_nmboot_summary` S3 object, and
#'     displays key summary information when printed to the console.
#'
#' ## Saved out data file:
#' The first time `summarize_bootstrap_run()` is called (or if
#' `force_resummarize = TRUE`), it will save the results to a `boot_summary.RDS`
#' data file within the bootstrap run directory. If one already exists, that data
#' set will be read in by default instead of being re-summarized.
#'  - The purpose of this is functionality two fold. For one, it helps avoid the
#'  need of re-executing `model_summary()` calls for a large number of runs. It
#'  also helps to reduce the number of files you need to commit via version
#'  control (see `cleanup_bootstrap_run()`).
#'
#' @seealso param_estimates_compare cleanup_bootstrap_run
#'
#' @examples
#' \dontrun{
#'
#' .boot_run <- read_model(file.path(MODEL_DIR, "1-boot"))
#' boot_sum <- summarize_bootstrap_run(.boot_run)
#'
#' # Optionally compare to original estimates
#' param_estimates_compare(boot_sum)
#'
#'
#' # Long format is helpful for plotting estimates:
#' bootstrap_estimates(.boot_run, format_long = TRUE) %>%
#' dplyr::filter(grepl("THETA", parameter_names)) %>%
#'   ggplot(aes(x = estimate)) +
#'   facet_wrap(~parameter_names, scales = "free") +
#'   geom_histogram(color = "white", alpha = 0.7) +
#'   theme_bw()
#'
#' }
#'
#' @name summarize_bootstrap
NULL


#' @describeIn summarize_bootstrap Summarize a bootstrap run and store results
#' @importFrom tidyselect any_of
#' @export
summarize_bootstrap_run <- function(
    .boot_run,
    force_resummarize = FALSE
){
  check_model_object(.boot_run, NMBOOT_MOD_CLASS)
  boot_dir <- .boot_run[[ABS_MOD_PATH]]
  boot_sum_path <- file.path(boot_dir, "boot_summary.RDS")

  if(!fs::file_exists(boot_sum_path) || isTRUE(force_resummarize)){
    # Check that runs can still be summarized (e.g, after cleanup)
    bootstrap_can_be_summarized(.boot_run)

    param_ests <- bootstrap_estimates(
      .boot_run, force_resummarize = force_resummarize
    )

    boot_sum_log <- summary_log(
      boot_dir, .bbi_args = list(
        no_grd_file = TRUE, no_ext_file = TRUE, no_shk_file = TRUE
      )
    ) %>% dplyr::select(-"error_msg") # only join based on model run

    # Tabulate all run details and heuristics
    run_details <- purrr::map_dfr(boot_sum_log$bbi_summary, function(sum){
      as_tibble(
        c(list2(!!ABS_MOD_PATH := sum[[ABS_MOD_PATH]]), sum$run_details)
      ) %>% tidyr::nest("output_files_used" = "output_files_used")
    })

    run_heuristics <- purrr::map_dfr(boot_sum_log$bbi_summary, function(sum){
      as_tibble(
        c(list2(!!ABS_MOD_PATH := sum[[ABS_MOD_PATH]]), sum[[SUMMARY_HEURISTICS]])
      )
    })

    # Run details, heuristics, and other information will be displayed elsewhere
    run_cols <- c(
      unique(c(names(run_details), names(run_heuristics))),
      "estimation_method", "problem_text", "needed_fail_flags", "param_count"
    )
    run_cols <- run_cols[-grepl(ABS_MOD_PATH, run_cols)]

    boot_sum_df <- dplyr::full_join(
      param_ests, boot_sum_log %>% dplyr::select(-any_of(run_cols)),
      by = c(ABS_MOD_PATH, "run")
    )

    if(any(!is.na(boot_sum_df$error_msg))){
      err_msgs <- unique(boot_sum_df$error_msg[!is.na(boot_sum_df$error_msg)])
      rlang::warn(
        c(
          "The following error messages occurred for at least one model:",
          err_msgs
        )
      )
    }

    boot_spec <- get_boot_spec(.boot_run)

    boot_sum <- c(
      list2(!!ABS_MOD_PATH := boot_dir),
      list(
        estimation_method = unique(boot_sum_log$estimation_method),
        based_on_model_path = boot_spec$based_on_model_path,
        based_on_data_set = boot_spec$based_on_data_path,
        strat_cols = boot_spec$strat_cols,
        seed = boot_spec$seed,
        n_samples = boot_spec$n_samples,
        run_details = run_details,
        run_heuristics = run_heuristics
      ),
      list(
        boot_summary = boot_sum_df
      )
    )

    # Assign class early for param_estimate_compare method
    class(boot_sum) <- c(NMBOOT_SUM_CLASS, class(boot_sum))

    # This gets done here instead of the print method, because we dont want
    # the printing of the _saved_ model summary object to be tied to the existence
    # of the based_on model.
    boot_compare <- param_estimates_compare(boot_sum)
    boot_sum$boot_compare <- boot_compare

    saveRDS(boot_sum, boot_sum_path)
  }else{
    verbose_msg(
      glue("Reading in bootstrap summary: {fs::path_rel(boot_sum_path, getwd())}\n\n")
    )
    boot_sum <- readRDS(boot_sum_path)
  }

  return(boot_sum)
}


#' @describeIn summarize_bootstrap Tabulate parameter estimates for each model
#'  submission in a bootstrap run
#' @param format_long logical (T/F). If `TRUE`, format data as a long table,
#'  making the data more portable for plotting.
#' @export
bootstrap_estimates <- function(
    .boot_run,
    format_long = FALSE,
    force_resummarize = FALSE
){
  check_model_object(.boot_run, NMBOOT_MOD_CLASS)


  boot_dir <- .boot_run[[ABS_MOD_PATH]]
  boot_sum_path <- file.path(boot_dir, "boot_summary.RDS")

  if(!fs::file_exists(boot_sum_path) || isTRUE(force_resummarize)){
    bootstrap_can_be_summarized(.boot_run)
    param_ests <- param_estimates_batch(.boot_run[[ABS_MOD_PATH]])
  }else{
    verbose_msg(
      glue("Reading in bootstrap summary: {fs::path_rel(boot_sum_path, getwd())}\n\n")
    )
    boot_sum <- readRDS(boot_sum_path)
    param_ests <- boot_sum$boot_summary
  }

  if(isTRUE(format_long)){
    # Long format - only keep estimates and error/termination columns for filtering
    param_ests <- param_ests %>% dplyr::select(
      all_of(ABS_MOD_PATH), "run", "error_msg", "termination_code",
      starts_with(c("THETA", "SIGMA", "OMEGA"))
    ) %>% tidyr::pivot_longer(
      starts_with(c("THETA", "SIGMA", "OMEGA")),
      names_to = "parameter_names", values_to = "estimate"
    ) %>% dplyr::relocate(
      c("error_msg", "termination_code"), .after = dplyr::everything()
    )
  }
  return(param_ests)
}

bootstrap_can_be_summarized <- function(.boot_run){
  # Check that runs can still be summarized (e.g, after cleanup)
  cleaned_up <- bootstrap_is_cleaned_up(.boot_run)
  if(isTRUE(cleaned_up)){
    rlang::abort(
      paste(
        "The bootstrap run has been cleaned up, and cannot be summarized again",
        "without resubmitting"
      )
    )
  }else{
    if(!bootstrap_is_finished(.boot_run)){
      rlang::abort(
        c(
          "One or more bootstrap runs have not finished executing.",
          "i" = "Run `get_model_status(.boot_run)` to check the submission status."
        )
      )
    }
  }
  return(invisible(TRUE))
}



#' @describeIn summarize_bootstrap Read in all bootstrap run model objects
#' @inheritParams get_boot_spec
#' @export
get_boot_models <- function(.boot_run){
  check_model_object(.boot_run, c(NMBOOT_MOD_CLASS, NMBOOT_SUM_CLASS))
  if(inherits(.boot_run, NMBOOT_SUM_CLASS)){
    .boot_run <- read_model(.boot_run[[ABS_MOD_PATH]])
  }

  output_dir <- get_output_dir(.boot_run, .check_exists = FALSE)
  if(!fs::file_exists(output_dir)){
    verbose_msg(
      glue("Bootstrap run `{get_model_id(.boot_run)}` has not been set up.")
    )
    return(invisible(NULL))
  }

  if(bootstrap_is_cleaned_up(.boot_run)){
    verbose_msg(
      glue("Bootstrap run `{get_model_id(.boot_run)}` has been cleaned up.")
    )
    return(invisible(NULL))
  }

  boot_spec <- get_boot_spec(.boot_run)
  boot_models <- tryCatch(
    purrr::map(boot_spec$bootstrap_runs$mod_path_abs, read_model),
    error = function(cond){
      # Suppress 'does not exist' message - handle separately
      if(stringr::str_detect(cond$parent$message, "does not exist")){
        return(NULL)
      }else{
        # Likely would only happen if there was a bbi/submission issue
        message(cond$parent$message)
      }
    }
  )

  # This shouldnt happen, but could if the directory existed and models
  # referenced in the spec file aren't found for any reason _other than_
  # cleaning up the run
  if(is.null(boot_models) || rlang::is_empty(boot_models)){
    boot_dir <- .boot_run[[ABS_MOD_PATH]]
    rlang::warn(
      c(
        glue("At least one bootstrap run model does not exist in `{boot_dir}`")
      )
    )
  }
  return(boot_models)
}


#' Cleanup bootstrap run directory
#'
#' This will delete all child models, and only keep the information
#' you need to read in estimates or summary information
#'
#' @details
#' The intent of this function is to help reduce the number of files you need to
#' commit via version control. Collaborators will be able to read in the
#' bootstrap model and summary objects without needing individual run files.
#'  - Note that this will prevent `force_resummarize = TRUE` from working
#'
#' **This should only be done** if you no longer need to re-summarize, as this
#'  will clean up (delete) the *individual* bootstrap model files
#'
#' @examples
#' \dontrun{
#'
#' .boot_run <- read_model(file.path(MODEL_DIR, "1-boot"))
#' cleanup_bootstrap_run(.boot_run)
#' }
#'
#' @inheritParams setup_bootstrap_run
#' @seealso summarize_bootstrap_run
#'
#' @export
cleanup_bootstrap_run <- function(.boot_run){
  check_model_object(.boot_run, NMBOOT_MOD_CLASS)
  boot_dir <- .boot_run[[ABS_MOD_PATH]]
  boot_sum_path <- file.path(boot_dir, "boot_summary.RDS")
  boot_data_dir <- file.path(boot_dir, "data")

  if(!bootstrap_is_finished(.boot_run)){
    rlang::abort(
      c(
        "One or more bootstrap runs have not finished executing.",
        "i" = "Run `get_model_status(.boot_run)` to check the submission status."
      )
    )
  }

  if(!fs::file_exists(boot_sum_path)){
    rlang::abort(
      c(
        "Model has not been summarized yet.",
        "Run `summarize_bootstrap_run() before cleaning up"
      )
    )
  }

  if(bootstrap_is_cleaned_up(.boot_run)){
    rlang::abort("Bootstrap run has already been cleaned")
  }

  # Overwrite spec file
  spec_path <- get_boot_spec_path(.boot_run)
  boot_spec <- jsonlite::read_json(spec_path, simplifyVector = TRUE)
  # Set cleaned up - impacts status checking
  boot_spec$bootstrap_spec$cleaned_up <- TRUE
  # Delete individual run specs
  # - dont need to store this information anymore since we wont be reading in
  #   individual models anymore
  boot_spec$bootstrap_runs <- NULL
  spec_lst_json <- jsonlite::toJSON(boot_spec, pretty = TRUE, simplifyVector = TRUE)

  # Delete individual model files
  boot_models <- get_boot_models(.boot_run)
  delete_models(boot_models, .tags = "BOOTSTRAP_RUN")

  # Save out updated spec and delete data directory only if the user says 'yes'
  if(!bootstrap_is_finished(.boot_run)){
    writeLines(spec_lst_json, spec_path)
    if(fs::dir_exists(boot_data_dir)) fs::dir_delete(boot_data_dir)
    message(glue("Bootstrap run `{get_model_id(.boot_run)}` has been cleaned up"))
  }
}



### Resampling functions ###
# TODO: delete these functions at the end before merging
# TODO: make sure this behaves how we want it to
# If we make notable edits, update mrgmisc


#' Resample data
#'
#' @param df data frame
#' @param key_cols key columns to resample on
#' @param strat_cols columns to maintain proportion for stratification
#' @param n number of unique sampled keys, defaults to match dataset
#' @param key_col_name name of outputted key column. Default to "KEY"
#' @param replace whether to stratify with replacement
#'
#' @keywords internal
resample_df <- function(df,
                        key_cols,
                        strat_cols = NULL,
                        n = NULL,
                        key_col_name = "KEY",
                        replace = TRUE) {

  names <- c(key_col_name, names(df))
  key <- get_key(df, key_cols)
  if(is.null(n)) n <- nrow(key)

  if(is.null(strat_cols)) {
    sample <- dplyr::sample_n(key, size = n, replace=replace)
    sample[[key_col_name]] <- 1:n
  } else {
    strat_key <- get_key(df, c(key_cols, strat_cols))
    if(nrow(strat_key) != nrow(key)) {
      warning(
        paste("Non-unique keys introduced from stratification. Check that",
              "all keys only have one stratification variable associated")
      )
    }
    sample <- stratify_df(strat_key, strat_cols, n, replace = replace)
    # drop strat cols so won't possibly mangle later left join
    sample <- dplyr::ungroup(sample)
    sample <- sample[, key_cols, drop=F]
    sample[[key_col_name]] <- 1:nrow(sample)
  }
  resampled_df <- dplyr::left_join(sample, df, by = key_cols)


  # reorder columns to match original df with key column appended
  return(resampled_df[,names, drop=F])
}

#' Stratify dataframe based on some columns
#'
#' @param df dataframe
#' @param strat_cols columns to stratify on
#' @param n number of samples
#' @param replace whether to resample with replacement
#' @noRd
stratify_df <- function(df,
                        strat_cols,
                        n,
                        replace = TRUE
) {
  frac <- n/nrow(df)
  sample <- df %>% dplyr::group_by(!!!rlang::syms(strat_cols)) %>%
    dplyr::sample_frac(frac, replace=replace)

  nsample <- nrow(sample)
  nleft <- n - nsample
  if(nleft != 0) {
    if(nleft > 0){
      extras <- dplyr::sample_n(df, size = nleft)
      sample <- dplyr::bind_rows(sample, extras)
    }else{
      removals <- sample.int(nsample, abs(nleft))
      sample <- sample[-removals,]
    }
  }
  return(sample)
}

#' Find unique values for key
#' @param df data frame
#' @param key_cols vector of column names. Defaults to all columns
#' @noRd
get_key <- function(df, key_cols = names(df)) {
  # add check to see if all key_cols available
  unique_df <- df[, key_cols, drop=F] %>% data.table::as.data.table() %>%
    unique(by=key_cols)
  return(tibble::as_tibble(unique_df))
}


pad_left <- function(x, padding = "0", max_char = 4){
  n_pad <- max_char - nchar(x)
  checkmate::assert_true(n_pad >= 0)
  val <- if(n_pad >= 1){
    pad <- paste(rep(padding, n_pad), collapse = "")
    glue::glue("{pad}{x}")
  }else if(n_pad == 0){
    as.character(x)
  }
  return(val)
}
