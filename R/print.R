#' Print methods for bbr objects
#'
#' The various objects defined by `bbr` have their own print methods to
#' allow users to get a quick view of the contents of the object.
#' **When printing a `bbi` object in an `.Rmd` file** that is intended to be
#' knit, consider setting `results = 'asis'` in the chunk options. This
#' will make for prettier formatting, especially of table outputs.
#'
#' @param x Object to format or print.
#' @param ... Other arguments passed on to individual methods.
#'
#' @name print_bbi
NULL

#' @describeIn print_bbi Prints the call made to bbi and whether the process is still running or has finished.
#' @param .call_limit Integer scalar for the max number of characters to print
#'   before truncating the call string. This is compared with the entire length,
#'   but only the positional arguments between the executable path and the first
#'   long option will be truncated.
#' @importFrom stringr str_split str_detect
#' @importFrom fs path_norm
#' @importFrom cli cat_line
#' @export
print.bbi_process <- function(x, ..., .call_limit = 250) {
  exec_path <- x[[PROC_BBI]]
  call_str <- paste(x[[PROC_CMD_ARGS]], collapse = " ")
  len_call <- nchar(exec_path) + nchar(call_str) + 1  # +1 for space

  trunc_marker <- " ... [truncated]"
  # Adjust the .call_limit so that we don't "truncate" just to end up at the
  # same length but with less information.
  .call_limit <- .call_limit - nchar(trunc_marker)

  # truncate model list if too long, keeping flags at the end (if any present)
  if (len_call > .call_limit) {
    split_call_str <- str_split(call_str, " \\-\\-", n = 2)
    mod_str <- split_call_str[[1]][1]
    flag_str <- split_call_str[[1]][2]

    # The length check above looked at unsplit call string. Do a second length
    # check to avoid marking an untruncated string as truncated.
    if (nchar(mod_str) > .call_limit) {
      call_str <- paste0(substr(mod_str, 1, .call_limit), trunc_marker)
      if(!is.na(flag_str)) {
        call_str <- paste0(call_str, " --", flag_str)
      }
    }
  }
  # ... and keeping the executable path at the beginning.
  call_str <- paste(exec_path, call_str)

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

#' Print a list of files.
#'
#' [print.bbi_model()] calls this after printing the YAML and model path to
#' allow specific model types to display additional files.
#'
#' @param .mod Model object.
#' @param print_fn A function that this method should call to print each file.
#' @keywords internal
#' @export
print_model_files <- function(.mod, print_fn) {
  UseMethod("print_model_files")
}

#' @export
print_model_files.default <- function(.mod, print_fn) {
  return(invisible(NULL))
}

#' @describeIn print_bbi Prints the information contained in the model object and whether the model has been run
#' @importFrom cli cli_h1 cli_h2 cat_bullet style_italic col_blue col_green col_red cat_rule
#' @importFrom purrr iwalk walk
#' @export
print.bbi_model <- function(x, ...) {

  # make sure a summary object doesn't slip through
  tryCatch(
    check_model_object(x),
    error = function(.e) {
      .error_msg <- paste(as.character(.e$message), collapse = " -- ")
      if (grepl("Must pass a model object", .error_msg, fixed = TRUE)) {
        dev_error(paste("print.bbi_model:", .error_msg))
      } else {
        stop(.e)
      }
    }
  )

  is_valid_print <- function(.x) {
    if (!is.null(.x)) {
      length(.x) != 0
    } else {
      FALSE
    }
  }

  heading <- cli_h1
  subheading <- cli_h2
  bullet_list <- cat_bullet

  if (isTRUE(getOption('knitr.in.progress'))) {
    is_asis <- knitr::opts_current$get("results") == 'asis'

    heading <- function(x) {
      # alphanumeric width = 1, line width = 2
      # w = ncharacters in output to width = 80
      cat('\n')
      if (is_asis) {
        w <- ceiling(nchar(x) + (80 - nchar(x)) / 2)
        cat_rule(x, width = w)
      } else {
        cat_rule(x)
      }
    }

    bullet_list <- subheading <- function(x) {
      if (is_asis) {
        cat("\n")
      }
      cat_bullet(x)
    }
  }

  status <- bbi_nonmem_model_status(x)
  if (status == "Finished Running") {
    status <- col_green(status)
  } else {
    status <- col_red(status)
  }

  heading('Status')
  subheading(status)

  heading("Absolute Model Path")
  bullet_list(x[[ABS_MOD_PATH]])

  heading("YAML & Model Files")
  bullet_list(get_yaml_path(x, .check_exists = FALSE))
  bullet_list(get_model_path(x, .check_exists = FALSE))
  print_model_files(x, bullet_list)

  if (is_valid_print(x[[YAML_DESCRIPTION]])) {
    heading('Description')
    bullet_list(style_italic(x[[YAML_DESCRIPTION]]))
  }

  if (is_valid_print(x[[YAML_TAGS]])) {
    heading('Tags')
    walk(x[[YAML_TAGS]], bullet_list)
  }

  if (is_valid_print(x[[YAML_NOTES]])) {
    heading('Notes')
    iwalk(x[[YAML_NOTES]],
          ~ bullet_list(paste0(.y, ": ", style_italic(.x))))
  }

  if (is_valid_print(x[[YAML_BBI_ARGS]])) {
    heading("BBI Args")
    iwalk(x[[YAML_BBI_ARGS]],
          ~ bullet_list(paste0(.y, ": ", col_blue(.x))))
  }
}


#' @describeIn print_bbi Prints a high level summary of a model from a `bbi_nonmem_summary` object
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
  cat_line(glue("Records: {.d$number_of_data_records}\t Observations: {.d$number_of_obs}\t Subjects: {.d$number_of_subjects}\n\n"))

  only_sim <- isTRUE(.d$only_sim)
  if (only_sim) {
    cat_line("No Estimation Methods (ONLYSIM)\n")
  } else {
    cat_line(glue("Objective Function Value (final est. method): {extract_ofv(list(x))}\n\n"))
    cli::cat_line("Estimation Method(s):\n")
    purrr::walk(paste(.d$estimation_method, "\n"), cli::cat_bullet, bullet = "en_dash")
  }

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

  if (only_sim) {
    return(invisible(NULL))
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
    select("parameter_names", "estimate", "stderr", "shrinkage") %>%
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


#' @describeIn print_bbi Prints a high level summary of a model from a `bbi_nmboot_summary` object
#' @importFrom purrr map_chr
#' @importFrom cli cat_line
#' @importFrom dplyr mutate_if
#' @importFrom checkmate assert_number
#'
#' @export
print.bbi_nmboot_summary <- function(x, .digits = 3, .nrow = 10, ...) {

  # print top line info
  .d <- x[[SUMMARY_DETAILS]]
  cli_h1("Based on")
  cat_line(paste("Model:", col_blue(x$based_on_model_path)))
  cat_line(paste("Dataset:", col_blue(x$based_on_data_set)))

  # Run specifications (seed, stratification columns, cleaned_up)
  cli_h1("Run Specifications")

  strat_cols <- if(is.null(x$strat_cols)) "None" else paste(x$strat_cols, collapse = ", ")
  names(strat_cols) <- "Stratification Columns"

  seed <- if(is.null(x$seed)) "None" else x$seed
  names(seed) <- "Seed"

  run_specs <- c(strat_cols, seed)
  iwalk(run_specs, ~ cat_bullet(paste0(.y, ": ", col_blue(.x))))

  # Add bullet if cleaned up
  if(isTRUE(bootstrap_is_cleaned_up(x))){
    cli::cat_bullet(paste("Cleaned up:", col_green(TRUE)))
  }

  # Bootstrap run summary (n_samples, any heuristics)
  cli_h1("Bootstrap Run Summary")

  # TODO: confirm this is appropriate for only_sim (unsure where this comes from)
  only_sim <- isTRUE("only_sim" %in% names(.d))
  if (only_sim) {
    cat_line("No Estimation Methods (ONLYSIM)\n")
  } else {
    cli::cat_line("Estimation Method(s):\n")
    purrr::walk(paste(x$estimation_method, "\n"), cli::cat_bullet, bullet = "en_dash")
  }

  cli::cat_line("Run Status:\n")
  n_samples <- c("Number of runs" = x$n_samples)
  cli::cat_bullet(paste("Number of runs:", col_blue(x$n_samples)), bullet = "en_dash")

  # check heuristics
  .h <- x[[SUMMARY_HEURISTICS]]
  heuristics_cols <- names(.h)[!grepl(ABS_MOD_PATH, names(.h))]
  heuristics <- purrr::map_dfr(heuristics_cols, function(col){
    tibble(heuristic = col, any_found = any(.h[[col]]), n_found = sum(.h[[col]]))
  })

  if (any(heuristics$any_found)) {
    heuristics_found <- heuristics$heuristic[which(heuristics$any_found)]
    heuristics_n <- heuristics$n_found[which(heuristics$any_found)]
    heuristics_perc <- round((heuristics_n/n_samples) * 100, 2)
    purrr::walk(
      paste0(heuristics_found, ": ", col_red(heuristics_n)," (", col_red(heuristics_perc), " %)"),
      cli::cat_bullet, bullet = "en_dash"
    )
    cat("\n")
  } else {
    cat_line("\n")
  }

  if (only_sim) {
    return(invisible(NULL))
  }


  # Build parameter comparison table if it exists
  # To avoid printing issues before the comparison is added to the summary object
  # see summarize_bootstrap_run() for details.
  if(!is.null(x$boot_compare)){
    param_df <- x$boot_compare %>% mutate_if(is.numeric, sig, .digits = .digits)

    if (!is.null(.nrow)) {
      checkmate::assert_number(.nrow)
      orig_rows <- nrow(param_df)
      .nrow <- min(.nrow, nrow(param_df))
      param_df <- param_df[1:.nrow, ]
    }

    if (requireNamespace("knitr", quietly = TRUE)) {
      param_str <- param_df %>%
        knitr::kable() %>%
        as.character()
    } else {
      param_str <- param_df %>%
        print() %>%
        capture.output()
    }

    cat_line(param_str)
    if (!is.null(.nrow)) cat_line(glue("... {orig_rows - .nrow} more rows"), col = "grey")
  }


}

#####################
# INTERNAL HELPERS
#####################


# Function to create ASCII box plot
ascii_boxplot <- function(data) {
  q <- quantile(data, c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

  # Overwrite min and max values with non-outliers
  whisker_len <- 1.5*stats::IQR(data, na.rm = TRUE)
  min_q <- unname(q[2] - whisker_len)
  max_q <- unname(q[4] + whisker_len)
  if(min_q > q[1]) q[1] <- min_q
  if(max_q < q[5]) q[5] <- max_q


  # Normalize distances between quartiles to minimum value
  rel_diffs <- diff(q)/min(diff(q))
  checkmate::assert_numeric(rel_diffs, lower = 0, finite = TRUE)

  # Attempt to normalize relative widths based on sum (min value must be at least 1)
  # - similar to fitting a plot to screen size
  if((15/sum(rel_diffs)) >= 1){
    rel_diffs <- rel_diffs * (15/sum(rel_diffs))
  }

  # Unicode handling
  down_arrow <- stringi::stri_unescape_unicode("\\u2193")
  box_border <- stringi::stri_unescape_unicode("\\u2500")
  box_corner_top_left <- stringi::stri_unescape_unicode("\\u250c")
  box_corner_bottom_left <- stringi::stri_unescape_unicode("\\u2514")
  box_corner_top_right <- stringi::stri_unescape_unicode("\\u2510")
  box_corner_bottom_right <- stringi::stri_unescape_unicode("\\u2518")

  # units - minimum 6 characters when normalized to 1 (see min of `rel_diffs`)
  # This is mainly to ensure there is enough space to print values underneath
  space_unit <- paste0(rep(" ", 6), collapse = "")
  dash_unit <- paste0(rep("-", 6), collapse = "")
  bar_unit <- paste0(rep(box_border, 6), collapse = "")

  # whisker lines
  dash1 <- paste0("|", paste0(rep(dash_unit, rel_diffs[1]), collapse = ""), "|")
  dash2 <- paste0("|", paste0(rep(dash_unit, rel_diffs[4]), collapse = ""), "|")

  # space box by length of left-whisker (space_end is just for consistency in length)
  # - space_pre gets an extra space to account for the minimum vertical line
  space_pre <- paste0(c(rep(paste0(space_unit), rel_diffs[1]), " "), collapse = "")
  space_end <- paste0(rep(paste0(space_unit), rel_diffs[4]), collapse = "")

  # Box for Q2
  bar1_lines <- paste0(rep(bar_unit, rel_diffs[2]), collapse = "")
  bar1_top <- paste0(box_corner_top_left, bar1_lines, "|")
  bar1_space <- paste0(rep(space_unit, rel_diffs[2]), collapse = "")
  bar1_bottom <- paste0(box_corner_bottom_left, bar1_lines, "|")

  # Box for Q3
  bar2_lines <- paste0(rep(bar_unit, rel_diffs[3]), collapse = "")
  bar2_top <- paste0(bar2_lines, box_corner_top_right)
  bar2_space <- paste(rep(space_unit, rel_diffs[3]), collapse = "")
  bar2_bottom <- paste0(bar2_lines, box_corner_bottom_right)

  # Build full box & whisker plot
  bar_top <- paste0(space_unit, space_pre, bar1_top, bar2_top, space_end)
  lines_middle <- paste0(space_unit, dash1,  bar1_space, "|", bar2_space, dash2)
  bar_bottom <- paste0(space_unit, space_pre, bar1_bottom, bar2_bottom, space_end)
  pointers <- paste0(
    space_unit, down_arrow,
    paste0(rep(space_unit, rel_diffs[1]), collapse = ""), down_arrow,
    paste0(rep(space_unit, rel_diffs[2]), collapse = ""), down_arrow,
    paste0(rep(space_unit, rel_diffs[3]), collapse = ""), down_arrow,
    paste0(rep(space_unit, rel_diffs[4]), collapse = ""), down_arrow,
    collapse = "")

  # Format and space values to align with vertical lines ('|') in `lines_middle`
  q_round <- round(q, 2)

  space_values <- function(len_quartile, len_value){
    start_space <- paste0(rep(space_unit, len_quartile), collapse = "")
    space_size <- max(nchar(start_space) - len_value, 1)
    paste0(rep(" ", space_size), collapse = "")
  }


  box_vals <- paste0(
    space_unit, q_round[1],
    space_values(rel_diffs[1], nchar(q_round[2])), q_round[2],
    space_values(rel_diffs[2], nchar(q_round[3])), q_round[3],
    space_values(rel_diffs[3], nchar(q_round[4])), q_round[4],
    space_values(rel_diffs[4], nchar(q_round[5])), q_round[5],
    collapse = "")

  cat_line(bar_top, col = "blue")
  cat_line(lines_middle, col = "blue")
  cat_line(bar_bottom, col = "blue")
  cat_line(pointers, col = "red")
  cat_line(box_vals, col = "blue")
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
    suppressSpecificWarning("NAs introduced by coercion") %>%
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
    as.numeric() %>%
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

