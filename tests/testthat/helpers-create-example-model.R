


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

#' @param mod_path Path to a control stream file
#' @param data_path Data path to set in a `$DATA` record.
#'
#' @keywords internal
modify_data_path_ctl <- function(mod_path, data_path){
  checkmate::assert_file_exists(mod_path)

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
