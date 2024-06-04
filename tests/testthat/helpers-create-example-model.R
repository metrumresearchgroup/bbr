


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
  ctl <- safe_read_ctl(mod)
  mod_path <- get_model_path(mod)
  est <- nmrec::select_records(ctl, "est")[[1]]

  msf_path_ctl <- get_msf_path(mod, .check_exists = FALSE)
  if(is.null(msf_path_ctl)){
    nmrec::set_record_option(est, "MSFO", msf_path)
    nmrec::write_ctl(ctl, mod_path)
  }else{
    rlang::inform(glue("MSF option already exists: \n - {est$format()}"))
  }
  return(mod)
}
