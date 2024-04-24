#' Create a boostrap model object from an existing model
#'
#' @param .mod a `bbr` model object
#' @param .suffix a prefix for the boostrap model directory. Will be appended by
#'  the boostrap run number for a given model.
#' @inheritParams copy_model_from
#' @param increment Logical (T/F). If `TRUE`, create a new bootstrap model run
#'  from `.mod`. Useful for managing multiple bootstrap runs originating from
#'  the same starting model. Will append `_run_[x]` to the provided `.suffix`.
#'  Note that the`.overwrite` arg will have no impact if incrementing the
#'  bootstrap run.
#' @param remove_cov,remove_tables Logical (T/F). Optionally remove `$COVARIANCE`
#' and `$TABLE` records respectively, allowing for notably faster run times.
#'
#' @seealso setup_bootstrap_run
#' @return S3 object of class `bbi_nmboot_model`.
#' @export
new_bootstrap_run <- function(
    .mod,
    .suffix = glue("{get_model_id(.mod)}_boot"),
    .inherit_tags = TRUE,
    .overwrite = FALSE,
    increment = FALSE,
    remove_cov = TRUE,
    remove_tables = TRUE
){

  checkmate::assert_class(.mod, NM_MOD_CLASS)

  model_dir <- get_model_working_directory(.mod)

  # Increments the bootstrap run
  # (i.e. impossible to overwrite an existing run via this function)
  if(isTRUE(increment)){
    new_run_id <- get_next_boot_run(model_dir, .suffix)
    boot_run_id <- paste0(.suffix, "_run_", new_run_id)
  }else{
    new_run_id <- 1
    boot_run_id <- paste0(.suffix)
  }


  boot_run <- copy_model_from(
    .parent_mod = .mod,
    .new_model = boot_run_id,
    .add_tags = "BOOTSTRAP_SUBMISSION",
    .inherit_tags = .inherit_tags,
    .update_model_file = TRUE,
    .overwrite = .overwrite
  )

  boot_run[[YAML_MOD_TYPE]] <- "nmboot"
  boot_run <- save_model_yaml(boot_run)

  # Change problem statement
  prob <- glue("Bootstrap run {new_run_id} of model {get_model_id(.mod)}")
  modify_prob_statement(boot_run, prob)

  # Optionally remove $TABLE and $COV statements here
  if(isTRUE(remove_cov)) remove_records(boot_run, type = "covariance")
  if(isTRUE(remove_tables)) remove_records(boot_run, type = "table")

  # Return read-in model to get the updated class as part of the model object
  return(read_model(file.path(model_dir, boot_run_id)))
}


get_next_boot_run <- function(model_dir, .suffix){
  model_dir_files <- fs::dir_ls(model_dir, glob = "*.yaml")
  boot_runs <- model_dir_files[grepl(.suffix, model_dir_files)]
  if(!rlang::is_empty(boot_runs)){
    boot_runs <- fs::path_ext_remove(basename(boot_runs))
    run_ids <- gsub(.suffix, "", boot_runs)
    run_ids <- readr::parse_number(run_ids) %>% suppressWarnings()
    run_ids[is.na(run_ids)] <- 1 # assign 1 if no `_run_[x]` is found
    new_run <- max(run_ids) + 1
  }else{
    new_run <- 1
  }
  return(new_run)
}


#' Set up a bootstrap model run.
#'
#' Creates a new model object and re-sampled dataset per model run.
#'
#' @param .boot_run a `bbi_nmboot_model` object.
#' @param n number of unique sampled keys, defaults to match dataset
#' @param strat_cols columns to maintain proportion for stratification
#' @param replace whether to stratify with replacement
#' @param seed a seed for sampling the data
#' @param .overwrite logical (T/F) indicating whether or not to overwrite existing
#'  setup for a bootstrap run.
#'
#' @seealso new_bootstrap_run
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
    replace = TRUE,
    seed = 1234,
    .overwrite = FALSE
){
  # TODO: make sure all _relevant_ resample_df args are part of this function
  checkmate::assert_class(.boot_run, NMBOOT_MOD_CLASS)
  checkmate::assert_number(n, lower = 1)
  checkmate::assert_number(seed)

  boot_dir <- get_output_dir(.boot_run, .check_exists = FALSE)
  boot_data_dir <- file.path(boot_dir, "data")

  # Bootstrap directory setup
  if(!fs::dir_exists(boot_dir) || .overwrite == TRUE){
    if(fs::dir_exists(boot_dir)) fs::dir_delete(boot_dir)
    fs::dir_create(boot_dir)
    fs::dir_create(boot_data_dir)
    # Store data within run folder and gitignore
    ##### TODO: UPDATE TO IGNORE MODEL FILES...
    writeLines("/data\n*.ctl\n*.mod\n*.yaml", file.path(boot_dir, ".gitignore"))

    # New model setup
    n_seq <- seq(n)
    mod_names <- purrr::map_chr(n_seq, max_char = nchar(n), pad_left)
    mod_paths <- file.path(boot_dir, mod_names)
    orig_mod <- read_model(get_based_on(.boot_run))

    boot_args <- list(
      boot_run = .boot_run,
      all_mod_names = mod_names,
      orig_mod_path = get_model_path(orig_mod),
      orig_mod_id = get_model_id(orig_mod),
      orig_data = nm_data(orig_mod) %>% suppressMessages(),
      strat_cols = strat_cols,
      replace = replace,
      seed = seed,
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
  orig_mod_path <- boot_args$orig_mod_path
  mod_path_ext <- paste0(mod_path, ".", fs::path_ext(orig_mod_path))
  fs::file_copy(
    orig_mod_path, mod_path_ext, overwrite = boot_args$overwrite
  )


  # Sample data and assign new IDs
  data_new <- resample_df(
    boot_args$orig_data,
    key_cols = "ID", # TODO: should this be a user arg?
    strat_cols = boot_args$strat_cols,
    replace = boot_args$replace
  ) %>% dplyr::rename("OID" = "ID", "ID" = "KEY") %>% # TODO: should this be a user arg?
    dplyr::select(all_of(unique(c(names(boot_args$orig_data), "OID")))) # TODO: should this be a user arg?

  mod_name <- basename(mod_path)
  orig_mod_id <- boot_args$orig_mod_id

  # Write out new dataset
  data_boot_name <- glue("boot_{orig_mod_id}_{mod_name}.csv")
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
    .tags = "BOOTSTRAP_RUN"
  )

  # Overwrite $PROB and $DATA records
  modify_prob_statement(mod, prob)
  modify_data_path_ctl(mod, data_path_rel)

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
    replace = boot_args$replace,
    seed = boot_args$seed,
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
      problem = modify_prob_statement(boot_run),
      mod_path = get_model_path(boot_run) %>% fs::path_rel(boot_dir),
      yaml_path = get_yaml_path(boot_run) %>% fs::path_rel(boot_dir),
      data_path = get_data_path(boot_run) %>% fs::path_rel(boot_dir)
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


# TODO: refactor this after a new config file is made post-submission
# this will allow this, and other functions to avoid model iteration and speed
# up execution times for functions that should be close to instantaneous.
bootstrap_is_finished <- function(.boot_run){
  boot_models <- get_boot_models(.boot_run)

  if(!is.null(boot_models)){
    models_finished <- map_lgl(boot_models, ~nonmem_finished_impl(.x))
    return(all(models_finished))
  }else{
    return(FALSE)
  }
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
#'  also helps to reduce the number of files you need to commit via version control.
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
    param_ests <- bootstrap_estimates(
      .boot_run, force_resummarize = force_resummarize,
      format_long = TRUE
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
        replace = boot_spec$replace,
        seed = boot_spec$seed,
        run_details = run_details,
        run_heuristics = run_heuristics
      ),
      list(
        boot_summary = boot_sum_df
      )
    )

    # Assign class and save out
    class(boot_sum) <- c(NMBOOT_SUM_CLASS, class(boot_sum))
    saveRDS(boot_sum, boot_sum_path)
  }else{
    verbose_msg(
      glue("Reading in bootstrap summary: {fs::path_rel(boot_sum_path, getwd())}\n\n")
    )
    boot_sum <- readRDS(boot_sum_path)
  }

  return(boot_sum)
}


#' @describeIn summarize_bootstrap Tabulate parameter estimates for each model submission in a bootstrap run
#' @param format_long logical (T/F). If `TRUE`, format data as a long table,
#'  making the data more portable for plotting.
#' @export
bootstrap_estimates <- function(
    .boot_run,
    format_long = FALSE,
    force_resummarize = FALSE
){
  check_model_object(.boot_run, NMBOOT_MOD_CLASS)

  if(!bootstrap_is_finished(.boot_run)){
    rlang::abort(
      c(
        "One or more bootstrap runs have not finished executing.",
        "i" = "Run `get_model_status(.boot_run)` to check the submission status."
      )
    )
  }

  boot_dir <- .boot_run[[ABS_MOD_PATH]]
  boot_sum_path <- file.path(boot_dir, "boot_summary.RDS")

  # TODO: change this approach. Reading in takes _longer_ than just calling
  # param_estimates_batch. We should only read in the data if the individual
  # model runs have been deleted. `force_resummarize` should force
  # `summarize_bootstrap_run` to be called again
  # if(!fs::file_exists(boot_sum_path) || isTRUE(force_resummarize)){
  #   param_ests <- param_estimates_batch(.boot_run[[ABS_MOD_PATH]])
  # }else{
  #   verbose_msg(
  #     glue("Reading in bootstrap summary: {fs::path_rel(boot_sum_path, getwd())}\n\n")
  #   )
  #   boot_sum <- readRDS(boot_sum_path)
  #   param_ests <- boot_sum$boot_summary
  # }
  param_ests <- param_estimates_batch(.boot_run[[ABS_MOD_PATH]])

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


#' Cleanup bootstrap run directory
#'
#' This will delete all child models, and only keep the information
#' you need to read in estimates or summary information
#'
#' @inheritParams setup_bootstrap_run
#'
#' @export
cleanup_bootstrap_run <- function(.boot_run){
  check_model_object(.boot_run, NMBOOT_MOD_CLASS)

  if(!bootstrap_is_finished(.boot_run)){
    rlang::abort(
      c(
        "One or more bootstrap runs have not finished executing.",
        "i" = "Run `get_model_status(.boot_run)` to check the submission status."
      )
    )
  }

  boot_dir <- .boot_run[[ABS_MOD_PATH]]
  boot_sum_path <- file.path(boot_dir, "boot_summary.RDS")
  if(!fs::file_exists(boot_sum_path)){
    rlang::abort(
      c(
        "Model has not been summarized yet.",
        "Run `summarize_bootstrap_run() before consolidating summary information."
      )
    )
  }

  # Delete individual model files
  boot_models <- get_boot_models(.boot_run)
  delete_models(boot_models, .tags = NULL)
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
