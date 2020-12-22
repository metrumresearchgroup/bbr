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
#' @importFrom crayon bold italic magenta red green blue
#' @importFrom purrr imap_chr
print.bbi_nonmem_model <- function(x, ...) {
  bullet <- function(.x) {
    bold(magenta(paste("\n*", .x)))
  }

  sub_bullet <- function(.x) {
    paste(paste("\n    *", .x),
          collapse = ' ')
  }

  is_valid_print <- function(.x) {
    if (!is.null(.x)) {
      length(.x) != 0
    } else {
      FALSE
    }
  }

  status <- red("Not Run")
  output_dir <- get_output_dir(x, .check_exists = FALSE)

  if (dir.exists(output_dir)) {
    status <- green("Finished Running")
    json_file <- file.path(output_dir, "bbi_config.json")

    if (!fs::file_exists(json_file)) {
      status <- red("Incomplete Run")
    }

  }

  cat(paste(bullet('Status:'),
            status))

  cat(paste(bullet("Absolute Model Path:"),
            sub_bullet(x[[ABS_MOD_PATH]])))

  cat(paste(
    bullet("YAML & Model Files:"),
    sub_bullet(get_yaml_path(x, .check_exists = FALSE)),
    sub_bullet(get_model_path(x, .check_exists = FALSE))
  ))

  if (is_valid_print(x[[YAML_DESCRIPTION]])) {
    cat(paste(bullet("Description:"),
              sub_bullet(x[[YAML_DESCRIPTION]])))
  }

  if (is_valid_print(x[[YAML_TAGS]])) {
    cat(paste(bullet("Tags:"),
              sub_bullet(x[[YAML_TAGS]])))
  }

  if (is_valid_print(x[[YAML_NOTES]])) {
    cat(paste(bullet("Notes:"),
              sub_bullet(imap_chr(
                x[[YAML_NOTES]],
                ~ paste0(.y,
                         ": ",
                         italic(.x))
              ))))
  }

  if (is_valid_print(x[[YAML_BBI_ARGS]])) {
    cat(paste(bullet("BBI Args:"),
              sub_bullet(imap_chr(
                x[[YAML_BBI_ARGS]],
                ~ paste0(.y,
                         ": ",
                         blue(.x))
              ))))
  }
}
