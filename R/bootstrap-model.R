#' Create a boostrap model object from an existing model
#'
#' @param .mod a `bbr` model object
#' @param .suffix a prefix for the boostrap model directory. Will be appended by
#'  the boostrap run number for a given model.
#' @inheritParams copy_model_from
#' @param increment Logical (T/F). If `TRUE`, create a new bootstrap model run.
#'  Will append `_run_[x]` to the provided `.suffix`. `.overwrite` will have no
#'  impact if incrementing the bootstrap run.
#'
#' @export
new_bootstrap_run <- function(
    .mod,
    .suffix = glue("{get_model_id(.mod)}_boot"),
    .inherit_tags = TRUE,
    .overwrite = FALSE,
    increment = FALSE
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

  # TODO: remove $TABLE and $COV statements here

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


#' Set up a bootstrap model run. Creates a new model object per run and generates
#' a new re-sampled dataset per run.
#'
#' @param .boot_run a `bbi_nmboot_model` object.
#' @param n number of unique sampled keys, defaults to match dataset
#' @param strat_cols columns to maintain proportion for stratification
#' @param replace whether to stratify with replacement
#' @param seed a seed for sampling the data
#' @param .overwrite logical (T/F) indicating whether or not to overwrite existing
#'  setup for a bootstrap run.
#'
#' @details
#' This setup is automatically done internally as part of submitting a bootstrap
#' run. However, users can call this function explicitly before submitting a
#' bootstrap run in order save some initial overhead. This can be useful for
#' large datasets or a large number of samples `n`.
#'
#' @export
setup_bootrap_run <- function(
    .boot_run,
    n = 10,
    strat_cols = NULL,
    replace = TRUE,
    seed = 1234,
    .overwrite = FALSE
){
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
    ##### UPDATE TO IGNORE MODEL FILES...
    writeLines("/data\n*.ctl\n*.mod\n*.yaml", file.path(boot_dir, ".gitignore"))
  }


  # New model setup
  n_seq <- seq(n)
  mod_names <- purrr::map_chr(n_seq, max_char = nchar(n), pad_left)
  mod_paths <- file.path(boot_dir, mod_names)
  orig_mod <- read_model(get_based_on(.boot_run))

  boot_args <- list(
    boot_run = .boot_run,
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
  # TODO: confirm if this is actually helpful, as this can take a couple seconds
  # - maybe check object.size of datasets and only perform if `n` is greater than some number
  gc()

  return(invisible(.boot_run))
}


#' Set up a single bootstrap model run
#'
#' @param mod_path absolute model path (no file extension) of a bootstrap model run.
#' @param boot_args list of parameters needed to create a bootstrap model run.
#'
#' @keywords internal
make_boot_run <- function(mod_path, boot_args){

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
    key_cols = "ID",
    strat_cols = boot_args$strat_cols,
    replace = boot_args$replace
  ) %>% dplyr::rename("OID" = "ID", "ID" = "KEY") %>%
    dplyr::select(all_of(unique(c(names(boot_args$orig_data), "OID"))))

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


#' Summarize a bootstrap run
#'
#' @inheritParams setup_bootrap_run
#' @param add_summary logical (T/F). Set to `TRUE` to include additional
#'  model summary parameters. If `FALSE`, only includes the parameter estimates
#'  (much faster for a large number of runs).
#'
#' @export
summarize_bootstrap_run <- function(
    .boot_run,
    add_summary = FALSE,
    force_resummarize = FALSE
){

  boot_dir <- .boot_run[[ABS_MOD_PATH]]
  boot_sum_path <- file.path(boot_dir, "boot_summary.RDS")

  if(!fs::file_exists(boot_sum_path) || isTRUE(force_resummarize)){
    param_ests <- param_estimates_batch(boot_dir)

    if(isTRUE(add_summary)){
      boot_sum_log <- summary_log(
        boot_dir, .bbi_args = list(
          no_grd_file = TRUE, no_ext_file = TRUE, no_shk_file = TRUE
        )
      ) %>% dplyr::select(-"error_msg")

      boot_sum <- dplyr::full_join(
        param_ests, boot_sum_log, by = c(ABS_MOD_PATH, "run")
      )
    }else{
      boot_sum <- param_ests
    }
    saveRDS(boot_sum, boot_sum_path)
  }else{
    boot_sum <- readRDS(boot_sum_path)
    if(isFALSE("bbi_summary" %in% names(boot_sum)) && isTRUE(add_summary)){
      rlang::warn(
        c(
          "A bootstrap summary _without_ summary columns was already saved to:",
          glue("`{boot_sum_path}`"),
          "i" = paste(
            "Re-run the summary call with `force_resummarize = TRUE` to add",
            "summary columns."
          )
        )
      )
    }
  }
  return(boot_sum)
}


### Resampling functions ###

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
