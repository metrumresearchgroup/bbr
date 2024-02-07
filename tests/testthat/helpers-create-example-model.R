


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

