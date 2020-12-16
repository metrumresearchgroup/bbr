#' Print methods for rbabylon objects
#'
#' The various objects defined by `rbabylon` have their own print methods to
#' allow users to get a quick view of the contents of the object.
#' @param x Object to format or print.
#' @param ... Other arguments passed on to individual methods.
#'
#' @name print_bbi
NULL

#' @describeIn print_bbi Prints the call made to bbi and whether the process is still running or has finished.
#' @param .call_limit Integer scalar for the max number of characters to print before truncating the call string.
#' @importFrom stringr str_split str_detect
#' @importFrom fs path_norm
#' @importFrom cli cat_line
print.babylon_process <- function(x, ..., .call_limit = 250) {
  call_str <- glue("{x[[PROC_BBI]]} {paste(x[[PROC_CMD_ARGS]], collapse = ' ')}")

  # truncate model list if too long, keeping flags at the end (if any present)
  if (nchar(call_str) > .call_limit) {
    split_call_str <- str_split(call_str, " \\-\\-", n = 2)
    mod_str <- split_call_str[[1]][1]
    flag_str <- split_call_str[[1]][2]

    call_str <- paste(substr(mod_str, 1, .call_limit), "... [truncated]")
    if(!is.na(flag_str)) {
      call_str <- paste0(call_str, " --", flag_str)
    }
  }

  # print call string
  cli::cat_line("Running:", col = "green")
  cat(paste0("  ", call_str, "\n"))

  # format and print run directory string
  run_dir <- fs::path_norm(x[[PROC_WD]])
  if (run_dir == ".") {
    run_dir <- getwd()
  }
  cli::cat_line(paste("In", run_dir), col = "green")

  # format and print status string
  if (length(x[[PROC_STDOUT]]) > 1) {
    cli::cat_line("Process finished.", col = "green")
  } else if (x[[PROC_STDOUT]] == "DRY_RUN") {
    cli::cat_line("DRY RUN! Process not actually run.", col = "red")
  } else if (str_detect(x[[PROC_STDOUT]], ".wait = FALSE")) {
    cli::cat_line("Not waiting for process to finish.", col = "blue")
  }

}


#' @describeIn print_bbi Prints a high level summary of a model from a bbi_nonmem_summary object
#' @importFrom purrr map_chr
print.bbi_nonmem_summary <- function(x, ...) {
  print_str <- character()

  .d <- .x[[SUMMARY_DETAILS]]
  print_str <- c(print_str, glue("Dataset: {.d$data_set}"))
  print_str <- c(print_str, glue("Records: {.d$number_of_data_records}\t Observations: {.d$number_of_obs}\t Patients: {.d$number_of_patients}"))
  print_str <- c(print_str, "Estimation Method(s):", map_chr(.d$estimation_method, ~glue("  - {.x}")))

  .h <- unlist(.x[[SUMMARY_HEURISTICS]])
  if (any(.h)) {
    h_str <- c("Heuristic Problem(s) Detected:", map_chr(names(which(.h)), ~glue("  - {.x}")))
  } else {
    h_str <- "No Heuristic Problems Detected"
  }
  print_str <- c(print_str, h_str)

  param_str <- .x %>%
    param_estimates() %>%
    select(parameter_names, estimate, stderr, shrinkage) %>%
    knitr::kable()
  print_str <- c(print_str, "", param_str)

  cat(print_str, sep = "\n")
}
