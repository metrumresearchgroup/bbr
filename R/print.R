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

#' @describeIn print_bbi Prints the information contained in the model object and whether the model has been run
#' @importFrom cli cli_h1 cli_h2 cat_bullet style_italic col_blue col_green col_red
#' @importFrom purrr iwalk walk
print.bbi_nonmem_model <- function(x, ...) {
  is_valid_print <- function(.x) {
    if (!is.null(.x)) {
      length(.x) != 0
    } else {
      FALSE
    }
  }

  status <- col_red("Not Run")
  output_dir <- get_output_dir(x, .check_exists = FALSE)

  if (dir.exists(output_dir)) {
    status <- col_green("Finished Running")
    json_file <- file.path(output_dir, "bbi_config.json")

    if (!fs::file_exists(json_file)) {
      status <- col_red("Incomplete Run")
    }

  }

  cli_h1('Status')
  cli_h2(status)

  cli_h1("Absolute Model Path")
  cat_bullet(x[[ABS_MOD_PATH]])

  cli_h1("YAML & Model Files")
  cat_bullet(get_yaml_path(x, .check_exists = FALSE))
  cat_bullet(get_model_path(x, .check_exists = FALSE))

  if (is_valid_print(x[[YAML_DESCRIPTION]])) {
    cli_h1('Description')
    cat_bullet(style_italic(x[[YAML_DESCRIPTION]]))
  }

  if (is_valid_print(x[[YAML_TAGS]])) {
    cli_h1('Tags')
    walk(x[[YAML_TAGS]], cat_bullet)
  }

  if (is_valid_print(x[[YAML_NOTES]])) {
    cli_h1('Notes')
    iwalk(x[[YAML_NOTES]],
          ~cat_bullet(paste0(.y, ": ", style_italic(.x))))
  }

  if (is_valid_print(x[[YAML_BBI_ARGS]])) {
    cli_h1("BBI Args")
    iwalk(x[[YAML_BBI_ARGS]],
          ~cat_bullet(paste0(.y, ": ", col_blue(.x))))
  }
}
