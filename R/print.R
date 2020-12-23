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
  cat_line("Running:", col = "green")
  cat(paste0("  ", call_str, "\n"))

  # format and print run directory string
  run_dir <- fs::path_norm(x[[PROC_WD]])
  if (run_dir == ".") {
    run_dir <- getwd()
  }
  cat_line(paste("In", run_dir), col = "green")

  # format and print status string
  if (length(x[[PROC_STDOUT]]) > 1) {
    cat_line("Process finished.", col = "green")
  } else if (x[[PROC_STDOUT]] == "DRY_RUN") {
    cat_line("DRY RUN! Process not actually run.", col = "red")
  } else if (str_detect(x[[PROC_STDOUT]], ".wait = FALSE")) {
    cat_line("Not waiting for process to finish.", col = "blue")
  }

}


#' @describeIn print_bbi Prints a high level summary of a model from a bbi_nonmem_summary object
#' @importFrom purrr map_chr
#' @importFrom cli cat_line
#' @importFrom dplyr mutate_if
#' @importFrom checkmate assert_number
#'
#' @param .digits Number of significant digits to use for parameter table. Defaults to 3.
#' @param .fixed If `FALSE`, the default, omits fixed parameters from the parameter table.
#' @param .off_diag If `FALSE`, the default, omits off-diagonals of OMEGA and SIGMA matrices from the parameter table.
#' @param .nrow If `NULL`, the default, print all rows of the parameter table.
#'   Otherwise, prints only `.nrow` rows.
#' @export
print.bbi_nonmem_summary <- function(x, .digits = 3, .fixed = FALSE, .off_diag = FALSE, .nrow = NULL, ...) {

  # print top line info
  .d <- x[[SUMMARY_DETAILS]]
  cat_line(glue("Dataset: {.d$data_set}\n\n"))
  cat_line(glue("Records: {.d$number_of_data_records}\t Observations: {.d$number_of_obs}\t Patients: {.d$number_of_patients}\n\n"))
  cat_line(glue("Objective Function Value (final est. method): {extract_ofv(list(x))}\n\n"))
  cli::cat_line("Estimation Method(s):\n")
  purrr::walk(paste(.d$estimation_method, "\n"), cli::cat_bullet, bullet = "en_dash")

  # check heuristics
  .h <- unlist(x[[SUMMARY_HEURISTICS]])
  if (any(.h)) {
    # h_str <- c("**Heuristic Problem(s) Detected:**\n", map_chr(names(which(.h)), ~glue("  - {.x}\n\n")))
    # cat_line(h_str, col = "red")
    #
    cli::cat_line("**Heuristic Problem(s) Detected:**\n", col = "red")
    purrr::walk(paste(names(which(.h)), "\n"), cli::cat_bullet, bullet = "en_dash", col = "red")
  } else {
    cat_line("No Heuristic Problems Detected\n\n")
  }

  # build parameter table (catch Bayesian error)
  param_df <- tryCatch(
    param_estimates(x),
    error = function(.e) {
      .error_msg <- paste(as.character(.e$message), collapse = " -- ")
      if (grepl(PARAM_BAYES_ERR_MSG, .error_msg, fixed = TRUE)) {
        cat_line(glue("**{PARAM_BAYES_ERR_MSG}**"), col = "red")
        return(NULL)
      } else {
        stop(.e)
      }
    }
  )
  if(is.null(param_df)) {
    return(invisible(NULL))
  }

  if (isFALSE(.fixed)) {
    param_df <- filter(param_df, !.data$fixed)
  }

  if (isFALSE(.off_diag)) {
    param_df <- filter(param_df, is.na(.data$diag) | .data$diag)
  }

  if (!is.null(.nrow)) {
    checkmate::assert_number(.nrow)
    orig_rows <- nrow(param_df)
    param_df <- param_df[1:.nrow, ]
  }

  param_df <- param_df %>%
    select(.data$parameter_names, .data$estimate, .data$stderr, .data$shrinkage) %>%
    mutate_if(is.numeric, sig, .digits = .digits)

  if (requireNamespace("knitr", quietly = TRUE)) {
    param_str <- param_df %>%
      knitr::kable() %>%
      as.character()

    # add color for shrinkage
    param_str <- map_chr(param_str, highlight_cell, .i = 5, .threshold = 30)
  } else {
    param_str <- param_df %>%
      print() %>%
      capture.output()
  }

  cat_line(param_str)
  if (!is.null(.nrow)) cat_line(glue("... {orig_rows - .nrow} more rows"), col = "grey")
}


#' Format digits
#'
#' Simplified version of `pmtables::sig()` for formatting numbers
#' to a specific number of significant digits.
#'
#' @param x numeric, value to manipulate
#' @param digits numeric, number of significant digits
#'
#' @return character vector of formatted values
#'
#' @keywords internal
sig <- function(.x, .digits) {
  namez <- names(.x)

  .x <- .x %>%
    as.numeric() %>%
    formatC(digits = .digits, format = 'g', flag = '#')
  .x <- gsub("\\.$", "", .x)
  .x <- gsub("NA", "", .x)

  names(.x) <- namez

  return(.x)
}


#' Highlight cell in kable table
#'
#' Highlights in red numeric cells that are above the specified threshold.
#'
#' @param .l character scalar of the line to be formatted (a row of a kable table)
#' @param .i the index of the column to check
#' @param .threshold the threshold to check against. If value is greater than
#'   .threshold then it is formatted as red.
#'
#' @return character vector of formatted values
#'
#' @keywords internal
highlight_cell <- function(.l, .i, .threshold) {
  split_l <- unlist(str_split(.l, "\\|"))
  ie1 <- .i-1
  is2 <- .i+1
  ie2 <- length(split_l)

  to_check <- split_l[[.i]]
  check_pad <- stringr::str_extract_all(to_check, " ") %>%
    unlist() %>%
    paste(collapse = "")

  to_check <- to_check %>%
    as.numeric %>%
    suppressSpecificWarning("NAs introduced by coercion")

  if (is.na(to_check) || to_check <= .threshold) {
    return(.l)
  }

  paste(
    paste(split_l[1:ie1], collapse = '|'),
    paste0(col_red(to_check), check_pad),
    paste(split_l[is2:ie2], collapse = '|'),
    sep = "|"
  )
}

