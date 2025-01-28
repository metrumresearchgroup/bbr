


# Helper Functions --------------------------------------------------------

#' Function to create nmrec object
#' @param case character string assigned to the PROBLEM record
#' @param input_ctl character string defining required record blocks
#'
#' @keywords internal
make_fake_mod <- function(case = NULL, input_ctl = NULL){

  template_lines <- glue::glue("$PROBLEM {case}\n\n{input_ctl}") %>%
    as.character() %>% strsplit("\n") %>% unlist()

  ctl <- nmrec::parse_ctl(template_lines)

  # Write new model and load object
  new_mod_path <- file.path(tempdir(), "example.ctl")
  nmrec::write_ctl(ctl, new_mod_path)
  new_mod <- new_model(new_mod_path, .overwrite = TRUE)

  return(new_mod)
}


#' Modify the specified data path in a control stream file

#' @param mod a bbr model object
#' @param data_path Data path to set in a `$DATA` record.
#'
#' @keywords internal
modify_data_path_ctl <- function(mod, data_path){
  mod_path <- get_model_path(mod)

  # Get data record
  ctl <- nmrec::read_ctl(mod_path)
  data_rec <- nmrec::select_records(ctl, "data")[[1]]
  data_rec$parse()

  # Overwrite 'filename' option
  data_rec$values <- purrr::map(data_rec$values, function(data_opt){
    if(inherits(data_opt, "nmrec_option_pos") && data_opt$name == "filename"){
      data_opt$value <- data_path
    }
    data_opt
  })

  # Write out modified ctl
  nmrec::write_ctl(ctl, mod_path)
}


modify_data_path_json <- function(mod, data_path){
  cfg_path <- get_config_path(mod)

  json <- jsonlite::read_json(cfg_path)
  json$data_path <- data_path
  jsonlite::write_json(json, cfg_path)
}


# Add `MSFO=1.MSF` option to an `EST` record
add_msf_opt <- function(mod, msf_path = paste0(get_model_id(mod), ".MSF")){
  ctl <- bbr:::get_model_ctl(mod)
  mod_path <- get_model_path(mod)
  est <- nmrec::select_records(ctl, "est")[[1]]

  msf_path_ctl <- bbr:::get_msf_path(mod, .check_exists = FALSE)
  if(is.null(msf_path_ctl)){
    nmrec::set_record_option(est, "MSFO", msf_path)
    nmrec::write_ctl(ctl, mod_path)
  }else{
    rlang::inform(glue("MSF option already exists: \n - {est$format()}"))
  }
  return(mod)
}


# This function makes a fake bootstrap run that appears to have been run
# MOD1 results are copied `n` times, and the summary results are jittered
# to reflect an actual bootstrap run
make_fake_boot <- function(mod, n = 100, strat_cols = c("SEX", "ETN")){
  boot_run <- new_bootstrap_run(mod, .overwrite = TRUE)
  boot_dir <- boot_run$absolute_model_path
  fs::dir_create(boot_dir)

  model_dir <- dirname(boot_dir)
  boot_dir_rel <- fs::path_rel(boot_dir, model_dir)

  # Need to explicitly point to internal function for vignette building
  mod_names <- purrr::map_chr(seq(n), max_char = nchar(n), bbr:::pad_left)

  boot_mods <- purrr::map(mod_names, function(id.i){
    output_dir.i <- file.path(boot_dir_rel, id.i)
    new_mod <- copy_model_from(
      .parent_mod = mod,
      .new_model = output_dir.i,
      .add_tags = "BOOTSTRAP_RUN",
      .overwrite = TRUE
    )
    new_dir_path <- file.path(boot_dir, id.i)
    fs::dir_copy(mod$absolute_model_path, new_dir_path)
    # replace file names with new model ID (needed for summary call)
    orig_mod_id <- get_model_id(mod)
    new_mod_id <- basename(new_dir_path)
    purrr::walk(fs::dir_ls(new_dir_path), ~ {
      if (stringr::str_detect(basename(.x), glue::glue("^{orig_mod_id}"))) {
        new_path.i <- stringr::str_replace(basename(.x), glue::glue("^{orig_mod_id}"), new_mod_id)
        fs::file_move(.x, file.path(dirname(.x), new_path.i))
      }
    })
    new_mod
  })

  boot_data_dir <- file.path(boot_dir, "data")
  boot_args <- list(
    run = boot_run,
    run_type = "bootstrap",
    all_mod_names = mod_names,
    run_mod_path = get_model_path(boot_run),
    orig_mod_path = get_model_path(mod),
    orig_mod_id = get_model_id(mod),
    orig_mod_bbi_args = mod$bbi_args,
    orig_data = nm_data(mod) %>% suppressMessages(),
    strat_cols = strat_cols,
    seed = 1234,
    n_samples = n,
    run_dir = boot_dir,
    data_dir = boot_data_dir,
    overwrite = TRUE
  )

  # Need to explicitly point to internal function for vignette building
  bbr:::make_analysis_spec(boot_mods, boot_args)

  # Read in summary to adjust estimates to look like real bootstrap
  boot_sum <- summarize_bootstrap_run(boot_run)

  # Adjust estimates to look like real bootstrap
  #  - jitter and then make normal distribution
  boot_sum$analysis_summary <- boot_sum$analysis_summary %>% dplyr::mutate(
    dplyr::across(starts_with(c("THETA", "OMEGA")), ~ jitter(.x, factor = 10))
  ) %>% dplyr::mutate(
    dplyr::across(starts_with(c("THETA", "OMEGA")), ~ rnorm(n = n, mean = mean(.x), sd = sd(.x)))
  )

  # Adjust comparison table
  boot_sum$boot_compare <- param_estimates_compare(boot_sum)

  # Save out
  boot_sum_path <- bbr:::get_analysis_sum_path(boot_run, .check_exists = FALSE)
  saveRDS(boot_sum, boot_sum_path)
  return(boot_run)
}

# This function creates a new model, and attaches a simulation to it
# Unlike make_fake_boot however, the simulation will have a status of "Not Run"
make_fake_sim <- function(mod, mod_id = "mod-sim", n = 100){
  mod_sim <- copy_model_from(mod, mod_id) %>% update_model_id()
  model_dir <- bbr:::get_model_working_directory(mod)
  new_dir_path <- file.path(model_dir, mod_id)
  fs::dir_copy(mod$absolute_model_path, new_dir_path)
  mod_sim <- add_msf_opt(mod_sim)

  # Remove all table records, and add one back (for nm_table_files to work)
  bbr:::remove_records(mod_sim, "table")
  table_lines <- glue::glue("NUM IPRED NPDE CWRES NOPRINT ONEHEADER FILE={mod_id}.tab")
  bbr:::add_new_record(mod_sim, "table", lines = table_lines)

  # Replace file names with new model ID (needed for reading in table files)
  orig_mod_id <- get_model_id(mod)
  purrr::walk(fs::dir_ls(new_dir_path), ~ {
    if (stringr::str_detect(basename(.x), glue::glue("^{orig_mod_id}"))) {
      new_path.i <- stringr::str_replace(basename(.x), glue::glue("^{orig_mod_id}"), mod_id)
      fs::file_move(.x, file.path(dirname(.x), new_path.i))
    }
  })

  # Duplicate 1.tab multiple times in new MSF file
  tab1_path <- nm_table_files(mod_sim)[1]
  new_tab_path <- file.path(dirname(tab1_path), glue::glue("{mod_id}.MSF"))
  fs::file_copy(tab1_path, new_tab_path)
  base_tab_lines <- readLines(new_tab_path)
  readr::write_lines(rep(base_tab_lines, 5), new_tab_path, append = TRUE)

  # Create fake simulation (not run)
  sim_inc <- bbr:::new_sim_model(mod_sim, n = n, .overwrite = TRUE)
  bbr:::make_sim_spec(sim_inc, sim_args = list(n = n, seed = 1234))
  return(mod_sim)
}
