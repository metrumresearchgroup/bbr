#' Create a boostrap model object from an existing model
#'
#' @param .mod a `bbr` model object
#' @param n number of unique sampled keys, defaults to match dataset
#' @param strat_cols columns to maintain proportion for stratification
#' @param replace whether to stratify with replacement
#' @param seed a seed for sampling the data
#' @param overwrite Logical to specify whether or not to overwrite existing
#'  model output from a previous run.
#'
#' @export
bootstrap_model <- function(
    .mod,
    n = 100,
    strat_cols = NULL,
    replace = TRUE,
    seed = 1234,
    overwrite = FALSE
){

  # Input data and control stream
  data <- nm_data(.mod) %>% suppressMessages()
  orig_mod_path <- get_model_path(.mod)

  # Bootstrap Directory
  model_dir <- get_model_working_directory(.mod)
  boot_dir <- file.path(model_dir, glue("boot_{get_model_id(.mod)}"))
  boot_data_dir <- file.path(boot_dir, "data")
  if(!fs::dir_exists(boot_dir)) fs::dir_create(boot_dir)
  if(!fs::dir_exists(boot_data_dir)) fs::dir_create(boot_data_dir)

  # New model setup
  n_seq <- seq(n)
  mod_names <- purrr::map_chr(n_seq, max_char = nchar(n), pad_left)
  mod_paths <- file.path(boot_dir, mod_names)
  mod_paths_abs <- paste0(mod_paths, ".", fs::path_ext(orig_mod_path))
  for(path.i in mod_paths_abs){
    fs::file_copy(orig_mod_path, path.i)
  }

  boot_args <- list(
    orig_mod_id = get_model_id(.mod),
    orig_mod_path = orig_mod_path,
    orig_data = data,
    strat_cols = strat_cols,
    replace = replace,
    seed = seed,
    boot_dir = boot_dir,
    boot_data_dir = boot_data_dir,
    overwrite = overwrite
  )

  # Create model object per boot run
  boot_runs <- purrr::map(mod_paths, make_boot_run, boot_args)

  gc() # This may help recover some RAM after handling many csvs

  # TODO: make the list of bootstrap models a new bbr model type object
  # This will make printing and handling (reading/writing) the object easier
}


make_boot_run <- function(mod_path, boot_args){

  # Read in boot run if it already exists
  # TODO: check that the referenced data exists too?
  if(fs::file_exists(yaml_ext(mod_path)) && !boot_args$overwrite) {
    return(read_model(mod_path))
  }

  # Sample data and assign new IDs
  data_new <- resample_df(
    boot_args$orig_data,
    key_cols = "ID",
    strat_cols = boot_args$strat_cols,
    replace = boot_args$replace
  ) %>% dplyr::rename("OID" = "ID", "ID" = "KEY") %>%
    dplyr::select(all_of(unique(c(names(boot_args$orig_data), "OID"))))

  # Write out new dataset
  mod_name <- basename(mod_path)
  data_boot_name <- glue("boot_{boot_args$orig_mod_id}_{mod_name}.csv")
  data_path_boot <- file.path(boot_args$boot_data_dir, data_boot_name)
  data.table::fwrite(data_new, data_path_boot , na = '.', quote = FALSE)

  # Set Description
  desc <- glue("Bootstrap run {mod_name} for model {boot_args$orig_mod_id}")
  data_path_rel <- fs::path_rel(data_path_boot, boot_args$boot_dir) %>%
    adjust_data_path_ext(mod_path = ".mod")

  # Create new model object
  mod <- new_model(
    mod_path,
    .description = desc,
    .overwrite = boot_args$overwrite
  )

  # Overwrite $PROB and $DATA records
  modify_prob_statement(mod, desc)
  modify_data_path_ctl(mod, data_path_rel)

  return(mod)
}


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
