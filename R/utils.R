#' Checks status code from processx::run output
#' @param .path Full path to a model that has been run that user wants to summarize
#' @param .model_type Type of model to summarize. Currently only supports "nonmem"
check_status_code <- function(.output) {
  if (.output$status != 0) {
    err_msg <- paste0(
      "`bbi ", paste(cmd_args, collapse=" "), .path, "` returned status code ", .output$status,
      " -- STDOUT: ", .output$stdout,
      " -- STDERR: ", .output$stderr
    )
    stop(err_msg)
  }
}

#' Checks that all passed NONMEM command line args are valid and formats
#' @param .args A named list of .args to check
#' @return
format_nonmem_args <- function(.args) {
  print(options("rbabylon.nonmen_args"))
}
