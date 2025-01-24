#' Create a bootstrap or SSE run from an existing model
#'
#' @param .mod A `bbr` model object.
#' @param .suffix A suffix for the new run directory. Will be prefixed by
#'  the model id of `.mod`.
#' @param .type Either `"nmboot"` or `"nmsse"`
#' @inheritParams copy_model_from
#' @param .inherit_tags If `TRUE`, the default, inherit any tags from `.mod`.
#' @param remove_cov,remove_tables If `TRUE`, the default, remove `$COVARIANCE`
#'  and `$TABLE` records respectively, allowing for notably faster run times.
#'
#' @keywords internal
new_analysis_run <- function(
    .mod,
    .suffix,
    .type = c("nmboot", "nmsse"),
    .add_tags = NULL,
    .inherit_tags = TRUE,
    .overwrite = FALSE,
    remove_cov = TRUE,
    remove_tables = TRUE
){
  .type <- match.arg(.type)
  check_model_object(.mod, NM_MOD_CLASS)

  # Create new run directory
  model_dir <- get_model_working_directory(.mod)
  run_dir <- glue("{get_model_id(.mod)}-{.suffix}")

  # Copy the model
  new_run <- copy_model_from(
    .parent_mod = .mod,
    .new_model = run_dir,
    .add_tags = .add_tags,
    .inherit_tags = .inherit_tags,
    .update_model_file = TRUE,
    .overwrite = .overwrite
  )

  # Modify YAML to set model type (e.g., "nmboot" or "nmsse")
  new_run[[YAML_MOD_TYPE]] <- .type
  new_run <- save_model_yaml(new_run)

  # Update `$PROBLEM` block
  prob_type <- dplyr::case_when(
    .type == "nmboot" ~ "Bootstrap run",
    .type == "nmsse" ~ "SSE"
  )
  problem_text <- glue("{prob_type} of model {get_model_id(.mod)}")
  modify_prob_statement(new_run, problem_text)

  # Optionally remove `$TABLE` and `$COV` blocks
  if (isTRUE(remove_cov)) remove_records(new_run, type = "covariance")
  if (isTRUE(remove_tables)) remove_records(new_run, type = "table")

  # Update table and estimation record file paths
  new_run <- update_model_id(new_run) %>% suppressMessages()

  # Return read-in model to get the updated class as part of the model object
  return(read_model(file.path(model_dir, run_dir)))
}


#' Set up a bootstrap or SSE run
#'
#' @param .run A `bbi_nmboot_model` or `bbi_nmsse_model` object.
#' @param .run_type Either `"bootstrap"` or `"sse"`. Determines behavior for dataset handling.
#' @param n Number of datasets and model runs to generate.
#' @param sample_size Sample size for each new dataset. Defaults to `NULL`, which
#'  matches the length of the original dataset.
#' @param strat_cols Columns to maintain proportion for stratification.
#' @param replace Logical (T/F). If `TRUE`, stratify with replacement.
#' @param seed A numeric seed to set prior to resampling the data; use `NULL` to
#'   avoid setting a seed.
#' @param data Custom dataset to use for setup. Defaults to `NULL`.
#' @param sim_col Column denoting the simulation run number. This will be `"nn"`,
#'  if using [bbr::add_simulation()], though `"IREP"` is also common (e.g., when
#'  using `mrgsolve`).
#' @param .bbi_args Named list passed to `model_summary(orig_mod, .bbi_args)`,
#'   where `orig_mod` is the model `.boot_run` is based on. See
#'   [print_bbi_args()] for valid options. Defaults to `list(no_grd_file = TRUE,
#'   no_shk_file = TRUE)` because [model_summary()] is only called internally to
#'   extract the number of records, so those files are irrelevant. Only used if
#'   the based on model (the model the analysis is being performed on) has been
#'   executed.
#' @param .overwrite Logical (T/F) indicating whether or not to overwrite
#'   existing setup.
#' @keywords internal
setup_analysis_run <- function(
    .run,
    run_type = c("bootstrap", "sse"),
    n = 200,
    sample_size = NULL,
    strat_cols = NULL,
    replace = FALSE,
    seed = 1234,
    data = NULL,
    sim_col = NULL,
    .bbi_args = list(
      no_grd_file = TRUE,
      no_shk_file = TRUE
    ),
    .overwrite = FALSE
){
  run_type <- match.arg(run_type)
  checkmate::assert_number(n, lower = 1)
  checkmate::assert_number(seed, null.ok = TRUE)
  checkmate::assert_logical(.overwrite)

  # Run directory and data directory
  run_dir <- get_output_dir(.run, .check_exists = FALSE)
  data_dir <- file.path(run_dir, "data")

  # Create or overwrite directories
  if(!fs::dir_exists(run_dir) || isTRUE(.overwrite)){
    if(fs::dir_exists(run_dir)) fs::dir_delete(run_dir)
    fs::dir_create(run_dir)
    fs::dir_create(data_dir)
  }else{
    cli::cli_abort(
      c(
        glue("{run_type} run has already been set up at `{run_dir}`"),
        "pass `.overwrite = TRUE` to overwrite"
      )
    )
  }

  # New model setup
  n_seq <- seq(n)
  mod_names <- purrr::map_chr(n_seq, max_char = nchar(n), pad_left)
  mod_paths <- file.path(run_dir, mod_names)
  orig_mod <- read_model(get_based_on(.run))

  # Store data within run folder and gitignore & ignore individual model files
  f_sep <- .Platform$file.sep
  default_ignore <- paste0(
    c("*.ctl", "*.mod", "*.yaml", paste0(f_sep, "data"), "OUTPUT"),
    collapse = "\n"
  )
  ignore_models <- paste0(f_sep, mod_names, collapse = "\n")
  ignore_lines <- paste(default_ignore, ignore_models, sep = "\n\n")
  writeLines(ignore_lines, file.path(run_dir, ".gitignore"))

  if(is.null(data)){
    if(run_type == "bootstrap"){
      # Overwrite data path in control stream
      #  - This is not necessary in most cases, but is if overwriting a previous
      #    run where a starting dataset was provided. The data path must then
      #    be updated to reflect the original control stream
      data_path <- get_data_path(.run, .check_exists = FALSE)
      if(!fs::file_exists(data_path)){
        data_path_rel <- get_data_path_from_ctl(orig_mod, normalize = FALSE)
        modify_data_path_ctl(.run, data_path_rel)
      }

      # Only include subjects that entered the original problem by default
      starting_data <- tryCatch({
        nm_data(.run, filter = TRUE) %>% suppressMessages()
      }, error = function(cond){
        fs::dir_delete(run_dir)
        # If IGNORE/ACCEPT expressions cant be turned into dplyr expressions
        cli::cli_div(theme = list(span.code = list(color = "blue")))
        cli::cli_abort(
          c(
            cond$message,
            "i" = "Please check your control stream or provide a starting dataset ({.var data} arg)",
            "i" = "You may try {.code setup_bootstrap_run(.run, data = nm_join(mod))}"
          )
        )
      })

      # Check the number of records to ensure the filtering was done correctly
      check_nm_data_filter(.run, starting_data, .bbi_args)
    }else if(run_type == "sse"){
      # TODO: maybe abort here if we want to require `data`
      sim <- add_simulation(
        orig_mod, n = n, seed = seed, .bbi_args = .bbi_args, .mode = "local"
      )
      starting_data <- nm_join_sim(sim)
    }
  }else{
    checkmate::assert_data_frame(data)
    data_name <- ifelse(run_type == "bootstrap", "boot-data.csv", "sse-data.csv")

    # Get input columns from dataset referenced in based_on model
    #  - must be from based_on model, as the data path of .boot_run may already
    #    have been adjusted to point to a new dataset (which wont exist if overwriting)
    input_cols <- get_input_columns(orig_mod)
    if(!all(input_cols %in% names(data))){
      mod_name <- ifelse(run_type == "bootstrap", ".boot_run", ".sse_run")
      missing_cols <- input_cols[!(input_cols %in% names(data))]
      missing_txt <- paste(missing_cols, collapse = ", ")
      fs::dir_delete(run_dir)
      cli::cli_abort(
        c(
          glue("The following required input columns were not found in the input data: {missing_txt}"),
          "Check `nm_data(read_model(get_based_on({mod_name})))` to see expected columns."
        )
      )
    }

    # Remove any extra columns
    starting_data <- dplyr::select(data, all_of(c(input_cols, sim_col)))

    # Save data to run_dir
    data_path_new <- file.path(run_dir, data_name)
    readr::write_csv(starting_data, data_path_new, na = ".")

    # Update data path in control stream (adjusting for .mod vs .ctl extension)
    data_path_rel <- adjust_data_path_ext(
      file.path(basename(run_dir), basename(data_path_new)),
      get_model_path(.run), reverse = TRUE
    )
    modify_data_path_ctl(.run, data_path_rel)
  }

  # Check stratification columns
  if(!is.null(strat_cols)){
    if(!all(strat_cols %in% names(starting_data))){
      strat_cols_miss <- strat_cols[!(strat_cols %in% names(starting_data))]
      # Clean up files before aborting (leave if .overwrite is TRUE)
      if(fs::dir_exists(run_dir)) fs::dir_delete(run_dir)
      cli::cli_abort(
        "The following `strat_cols` are missing from the input data: {.val {strat_cols_miss}}"
      )
    }
  }

  # Check simulation column (SSE only)
  if(run_type == "sse"){
    if(!is.null(sim_col)){
      if(!(sim_col %in% names(starting_data))){
        if(fs::dir_exists(run_dir)) fs::dir_delete(run_dir)
        cli::cli_abort(
          "`sim_col` ({.val {sim_col}}) is missing from the input data"
        )
      }

      n_sim <- dplyr::n_distinct(starting_data[[sim_col]])
      if(n_sim != n){
        if(fs::dir_exists(run_dir)) fs::dir_delete(run_dir)
        cli::cli_abort(
          "The number of simulations ({.val {n_sim}}) does not match the provided `n` ({.val {n}})"
        )
      }
    }else{
      if(fs::dir_exists(run_dir)) fs::dir_delete(run_dir)
      cli::cli_abort("sim_col is a required argument for SSE runs")
    }
  }


  metadata <- list(
    run = .run,
    run_type = run_type,
    all_mod_names = mod_names,
    run_mod_path = get_model_path(.run),
    orig_mod_path = get_model_path(orig_mod),
    orig_mod_id = get_model_id(orig_mod),
    orig_mod_bbi_args = orig_mod$bbi_args,
    orig_data = starting_data,
    sample_size = sample_size,
    strat_cols = strat_cols,
    replace = replace,
    seed = seed,
    sim_col = sim_col,
    n_samples = n,
    run_dir = run_dir,
    data_dir = data_dir,
    overwrite = .overwrite
  )

  # Create model object per run
  if(!is.null(seed)) withr::local_seed(seed)
  run_models <- purrr::map(mod_paths, make_analysis_model, metadata)
  make_analysis_spec(run_models, metadata)

  return(invisible(.run))
}


#' Set up a single bootstrap or SSE model run
#'
#' @param mod_path Absolute model path (no file extension) of a model run.
#' @param metadata List of parameters needed to create a model run.
#'
#' @keywords internal
make_analysis_model <- function(mod_path, metadata){
  # Update the user every 100 runs (plus beginning and end)
  new_mod_index <- which(basename(mod_path) == metadata$all_mod_names)
  if(new_mod_index == 1 || new_mod_index == length(metadata$all_mod_names) ||
     new_mod_index %% 100 == 0){
    verbose_msg(glue("Sampling run {new_mod_index}/{length(metadata$all_mod_names)}"))
  }

  # Copy over control stream
  #  - Cant use copy_model_from, as we want these individual model runs to be
  #    regular `bbi_base_model` objects
  run_mod_path <- metadata$run_mod_path
  mod_path_ext <- paste0(mod_path, ".", fs::path_ext(run_mod_path))
  fs::file_copy(
    run_mod_path, mod_path_ext, overwrite = metadata$overwrite
  )

  # Filter to simulation rep for SSE runs
  if(metadata$run_type == "sse" && !is.null(metadata$sim_col)){
    sim_col <- metadata$sim_col
    irep <- unique(data$nn)[new_mod_index]
    data <- metadata$orig_data %>% dplyr::filter(!!rlang::sym(sim_col) == irep)
  }else{
    data <- metadata$orig_data
  }

  # Sample data and assign new IDs
  data_new <- mrgmisc::resample_df(
    data,
    key_cols = "ID",
    strat_cols = metadata$strat_cols,
    n = metadata$sample_size,
    replace = metadata$replace
  ) %>% dplyr::rename("OID" = "ID", "ID" = "KEY") %>%
    dplyr::select(all_of(unique(c(names(data), "OID"))))

  mod_name <- basename(mod_path)
  orig_mod_id <- metadata$orig_mod_id

  # Write out new dataset
  data_run_name <- glue("{mod_name}.csv")
  data_path <- file.path(metadata$data_dir, data_run_name)
  data.table::fwrite(data_new, data_path , na = '.', quote = FALSE)

  # Set Problem and relative datapath (to be sourced in control stream)
  prob <- glue("{metadata$run_type} run {mod_name} of model {orig_mod_id}")
  data_path_rel <- fs::path_rel(data_path, metadata$run_dir) %>%
    adjust_data_path_ext(mod_path = mod_path_ext, reverse = TRUE)

  # Create new model object
  mod <- new_model(
    mod_path,
    .overwrite = metadata$overwrite,
    .tags = paste0(toupper(metadata$run_type), "_RUN"),
    .based_on = run_mod_path,
    .bbi_args = metadata$orig_mod_bbi_args

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


#' Read in all analysis run model objects
#' @inheritParams get_analysis_spec
#' @keywords internal
get_analysis_models <- function(.run){
  run_dir <- .run[[ABS_MOD_PATH]]
  output_dir <- get_output_dir(.run, .check_exists = FALSE)

  run_type <- dplyr::case_when(
    .run[[YAML_MOD_TYPE]] == "nmboot" ~ "bootstrap run",
    .run[[YAML_MOD_TYPE]] == "nmsse" ~ "SSE"
  )

  if(!fs::file_exists(output_dir)){
    verbose_msg(
      glue("{run_type} `{get_model_id(.run)}` has not been set up.")
    )
    return(invisible(NULL))
  }

  if(analysis_is_cleaned_up(.run)){
    verbose_msg(
      glue("{run_type} `{get_model_id(.run)}` has been cleaned up.")
    )
    return(invisible(NULL))
  }

  spec <- get_analysis_spec(.run)
  model_ids <- fs::path_ext_remove(basename(spec$analysis_runs$mod_path_abs))

  mods <- tryCatch({
    find_models(.run[[ABS_MOD_PATH]], .recurse = FALSE, .include = model_ids)
  }, warning = function(cond){
    if(!stringr::str_detect(cond$message, "All models excluded|Found no valid model")){
      warning(cond)
    }
    return(NULL)
  })


  # This shouldnt happen, but could if the directory existed and models
  # referenced in the spec file aren't found for any reason _other than_
  # cleaning up the run
  if(is.null(mods) || rlang::is_empty(mods)){
    rlang::abort(
      c(
        glue("At least one {run_type} model does not exist in `{run_dir}`")
      )
    )
  }else{
    if(length(model_ids) != length(mods)){
      rlang::warn(
        c(
          glue("Found an unexpected number of models in {run_dir}"),
          glue("Expected number of models: {length(model_ids)}"),
          glue("Discovered number of models: {length(mods)}")
        )
      )
    }
  }

  return(mods)
}

#' Store bootstrap run details before submission
#'
#' @param run_models List of model objects created by `make_analysis_model()`.
#' @inheritParams make_analysis_model
#'
#' @details
#' This is mainly meant to ensure traceability and enhance the portability of
#' bootstrap "model" objects.
#'
#'
#' @keywords internal
make_analysis_spec <- function(run_models, metadata){
  run_dir <- metadata$run_dir
  json_name <- ifelse(metadata$run_type == "bootstrap", "boot", "sse")
  json_path <- file.path(run_dir, glue("bbr_{json_name}_spec.json"))

  if(fs::file_exists(json_path) && isFALSE(metadata$overwrite)){
    rlang::abort(
      c(
        glue("A specification file already exists at `{json_path}`"),
        "i" = "Pass `.overwrite = TRUE` to overwrite."
      )
    )
  }

  analysis_spec <- list(
    problem = glue("{metadata$run_type} of {basename(metadata$orig_mod_path)}"),
    strat_cols = metadata$strat_cols,
    seed = metadata$seed,
    n_samples = metadata$n_samples,
    sample_size = metadata$sample_size,
    model_path = get_model_path(metadata$run),
    based_on_model_path = metadata$orig_mod_path,
    based_on_data_path = get_data_path_from_ctl(metadata$run, normalize = FALSE),
    model_md5 = tools::md5sum(get_model_path(metadata$run)),
    based_on_model_md5 = tools::md5sum(metadata$orig_mod_path),
    based_on_data_md5 = tools::md5sum(get_data_path(metadata$run)),
    output_dir = metadata$run[[ABS_MOD_PATH]]
  )

  run_ids <- purrr::map_chr(run_models, function(.mod){
    paste0("run_", basename(.mod[[ABS_MOD_PATH]]))
  })

  analysis_runs <- purrr::map(run_models, function(.mod){
    list(
      mod_path = get_model_path(.mod) %>% fs::path_rel(run_dir),
      yaml_path = get_yaml_path(.mod) %>% fs::path_rel(run_dir)
    )
  }) %>% stats::setNames(run_ids)

  spec_lst <- c(
    analysis_spec = list(analysis_spec),
    analysis_runs = list(analysis_runs)
  )

  spec_lst_json <- jsonlite::toJSON(
    spec_lst, pretty = TRUE, simplifyVector = TRUE, null = "null"
  )
  writeLines(spec_lst_json, json_path)
  return(invisible(json_path))
}

# helpers -----------------------------------------------------------------


#' If model has finished, check the number of records to ensure the filtering
#' was done correctly via `nm_data(.mod, filter = TRUE)`
#' @param data a dataset created via `nm_data(.mod. filter = TRUE)`
#' @inheritParams setup_analysis_run
#' @noRd
check_nm_data_filter <- function(.run, data, .bbi_args = NULL){
  run_dir <- get_output_dir(.run, .check_exists = FALSE)
  orig_mod <- read_model(get_based_on(.run))

  # If model has finished, check the number of records to ensure the filtering
  # was done correctly
  if(check_nonmem_finished(orig_mod)){
    .s <- model_summary(orig_mod, .bbi_args = .bbi_args)
    nrec <- .s$run_details$number_of_data_records
    nrec_f <- nrow(data)
    if(nrec != nrec_f){
      fs::dir_delete(run_dir)
      cli::cli_div(theme = list(.code = list(color = "blue"), .val = list(color = "red3")))
      cli::cli_abort(
        c(
          "!" = "The filtered dataset does not have the same number of records as the original model:",
          "*" = "{.code nm_data(.boot_run, filter = TRUE)} returned {.val {nrec_f}} records",
          "*" = "{.code model_summary(orig_mod)} returned {.val {nrec}} records",
          "i" = "where {.code orig_mod <- read_model(get_based_on(.boot_run))}",
          "i" = "Try providing a starting dataset (e.g., {.code setup_bootstrap_run(.boot_run, data = nm_join(orig_mod))})"
        )
      )
    }
  }else{
    orig_mod_name <- fs::path_rel(
      get_model_path(orig_mod),
      get_model_working_directory(orig_mod)
    )
    cli::cli_div(theme = list(.code = list(color = "blue"), .val = list(color = "red3")))
    cli::cli_warn(
      c(
        "The parent model ({.code {orig_mod_name}}) has not been submitted",
        "i" = "Consider executing {.code {orig_mod_name}} to perform additional checks"
      )
    )
  }
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
