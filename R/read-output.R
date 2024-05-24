# Helper functions for reading output files

#' Check an output file
#'
#' Reads the head and tail of specified file and prints it the console and/or returns it as a character vector.
#' This is called internally in several S3 methods described below.
#' @param .file Character scalar of path to file to read
#' @param .head Integer for number of lines to read from the top of the file
#' @param .tail Integer for number of lines to read from the bottom of the file
#' @param .print If `TRUE`, the default, print resulting head and tail to console.
#' @param .return If `FALSE`, the default, returns `NULL` invisibly. If `TRUE` returns resulting head and tail as a character vector.
#' @importFrom readr read_lines
#' @export
check_file <- function(.file, .head = 3, .tail = 5, .print = TRUE, .return = FALSE) {
  checkmate::assert_string(.file)
  l <- read_lines(.file)

  l_len <- length(l)

  if (.head + .tail >= l_len) {
    # return full file if head + tail >= total length
    res_vec <- l
  } else {
    # fetch head vector
    if (.head > 0) {
      head_vec <- l[1:.head]
    } else {
      head_vec <- NULL
    }

    # fetch tail vector
    if (.tail <= 0) {
      tail_start <- NULL
    } else {
      tail_start <- 1 + l_len - .tail
    }

    if (!is.null(tail_start)) {
      tail_vec <- l[tail_start:l_len]
    } else {
      tail_vec <- NULL
    }

    # concatenate
    dot_dot <- ifelse((tail_start > .head + 1) || is.null(tail_start), "...", NULL)
    res_vec <- c(head_vec, dot_dot, tail_vec)
  }

  # return and/or print
  if (.print) {
    cat(paste(res_vec, collapse="\n"), "\n")
  }
  if (.return) {
    return(res_vec)
  } else {
    return(invisible())
  }
}


# wrappers to interact easily with OUTPUT file

#' @rdname check_file
#' @param .mod Model object or path associated with file.
#' @param ... arguments passed through to `check_file()`
#' @export
tail_output <- function(.mod, ...) {
  UseMethod("tail_output")
}

#' @describeIn check_file Tail the OUTPUT file from a file path to a model.
#' @export
tail_output.character <- function(.mod, .head = 3, .tail = 5, .print = TRUE, .return = FALSE, ...) {
  checkmate::assert_string(.mod)
  # If model path passed, rely on model method to construct path.
  if (basename(.mod) != "OUTPUT") {
    return(
      tail_output(
        read_model(.mod),
        .head = .head, .tail = .tail, .print = .print, .return = .return,
        ...
      )
    )
  }

  check_file(.mod, .head, .tail, .print, .return, ...)
}

#' @describeIn check_file Tail the OUTPUT file from a `bbi_base_model` object.
#' @export
tail_output.bbi_base_model <- function(.mod, .head = 3, .tail = 5, .print = TRUE, .return = FALSE, ...) {
  .file <- file.path(get_output_dir(.mod), "OUTPUT")
  tryCatch(
    check_file(.file, .head, .tail, .print, .return, ...),
    error = function(err) {
      if (!stringr::str_detect(err$message, "does not exist")) {
        stop(err)
      }

      status <- bbi_nonmem_model_status(.mod)
      if (status == "Finished Running") {
        message("Model already finished running")
      } else if (status == "Incomplete Run") {
        message("Model running but OUTPUT file doesn't exist (check back)")
      } else {
        # Given the get_output_dir() call upstream, "Not Run" state should be
        # inaccessible. Treat it as a bug.
        stop(err)
      }
    }
  )
}


# wrappers to interact easily with .lst file

#' @rdname check_file
#' @param .mod Model object or path associated with file.
#' @param ... arguments passed through to `check_file()`
#' @export
tail_lst <- function(.mod, ...) {
  UseMethod("tail_lst")
}

#' @describeIn check_file Tail the .lst file from a file path to a model.
#' @export
tail_lst.character <- function(.mod, .head = 3, .tail = 5, .print = TRUE, .return = FALSE, ...) {
  checkmate::assert_string(.mod)
  # If model path passed, rely on model method to construct path.
  if (tools::file_ext(.mod) != "lst") {
    return(
      tail_lst(
        read_model(.mod),
        .head = .head, .tail = .tail, .print = .print, .return = .return,
        ...
      )
    )
  }

  check_file(.mod, .head, .tail, .print, .return, ...)
}

#' @describeIn check_file Tail the .lst file from a `bbi_base_model` object.
#' @export
tail_lst.bbi_base_model <- function(.mod, .head = 3, .tail = 5, .print = TRUE, .return = FALSE, ...) {
  .file <- build_path_from_model(.mod, ".lst")
  check_file(.file, .head, .tail, .print, .return, ...)
}


#' Check output directory
#'
#' List files in the output directory to glance at where the process is
#' @param .mod Either a `bbi_{.model_type}_model` object or a file path to an output directory
#' @param ... values to pass to `fs::dir_ls`
#' @export
check_output_dir <- function(.mod, ...) {
  UseMethod("check_output_dir")
}

#' @describeIn check_output_dir Takes a file path to the output directory.
#' @export
check_output_dir.character <- function(.mod, ...) {
  .out_files <- fs::dir_ls(.mod, ...)
  return(.out_files)
}

#' @describeIn check_output_dir Takes `bbi_{.model_type}_model` or `bbi_{.model_type}_summary` object
#' @export
check_output_dir.bbi_model <- function(.mod, ...) {
  .output_dir <- .mod %>% get_output_dir()
  .out_files <- check_output_dir(.output_dir, ...)
  return(.out_files)
}


#' Check NONMEM output files
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Checks a NONMEM output file that's a whitespace-delimited file (for instance .grd or .ext)
#' @param .path Character scalar path to the file
#' @param .x_var name of variable to filter with `.x_floor`
#' @param .x_floor Filters file to only rows with `.x_var` GREATER THAN this value.
#' @importFrom readr read_table2 cols
#' @importFrom dplyr filter
#' @export
check_nonmem_table_output <- function(
  .path,
  .x_var = NULL,
  .x_floor = NULL) {

  deprecate_warn(
    "1.5.0",
    "check_nonmem_table_output()",
    with = "nm_file()",
    details = "All functions calling `check_nonmem_table_output()` are being replaced from `nm_*()` functions. See ?nm_file for details."
  )

  # read file
  df <- read_table2(.path, skip=1, col_types = cols())

  # filter out early iterations (and final)
  if(!is.null(.x_var) && !is.null(.x_floor)) {
    df <- df %>% filter(.data[[.x_var]] > .x_floor)
  }

  # return result
  return(df)
}


#' Plot NONMEM output files
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Creates a line plot of the wide-format tibble output from `check_nonmem_table_output()`
#' @param .df the wide-format tibble output from `check_nonmem_table_output()`
#' @param .x_var String of the variable name to use on the X-axis
#' @param .stat_name String of the name of the stat the other columns represents (like "gradient" or "theta").
#' @export
plot_nonmem_table_df <- function(.df, .x_var, .stat_name) {
  deprecate_warn(
    "1.5.0",
    "plot_nonmem_table_df()",
    details = paste(
      "All functions calling `plot_nonmem_table_df()` are being deprecated to focus the scope of bbr.",
      "Consider using https://github.com/metrumresearchgroup/pmplots instead."
    )
  )

  if (!requireNamespace("ggplot2", quietly = TRUE) || !requireNamespace("forcats", quietly = TRUE)) {
    stop(paste("must have both ggplot2 and forcats to use plot_nonmem_table_df"))
  }

  p <- .df %>% tidyr::gather("stat", "value", -.data[[.x_var]]) %>%
    mutate(stat = forcats::fct_inorder(.data$stat)) %>%
    ggplot2::ggplot(ggplot2::aes(x=.data[[.x_var]], y=.data$value, colour=.data$stat)) +
    ggplot2::geom_line() +
    ggplot2::xlab(.x_var) +
    ggplot2::ylab(paste(.stat_name, "value")) +
    ggplot2::scale_colour_discrete(name = .stat_name) +
    ggplot2::ggtitle(paste(.x_var, "x", .stat_name))
  return(p)
}


# wrappers to interact with .grd files easily

#' @rdname check_nonmem_table_output
#' @param .mod Model to check. Either a `bbi_nonmem_model` object or a file path.
#' @param .iter_floor Filters file to only rows with `ITERATION` GREATER THAN this value.
#' @export
check_grd <- function(.mod, .iter_floor = 0) {
  UseMethod("check_grd")
}

#' @describeIn check_nonmem_table_output Checks .grd file from a file path
#' @export
check_grd.character <- function(.mod, .iter_floor = 0) {
  checkmate::assert_string(.mod)
  # if model path passed, construct path
  if (tools::file_ext(.mod) != "grd") {
    .mod = as.character(file.path(.mod, paste0(get_model_id(.mod), ".grd")))
  }

  df <- check_nonmem_table_output(.mod, .x_var = "ITERATION", .x_floor = .iter_floor)
  return(df)
}

#' @describeIn check_nonmem_table_output Checks .grd file from a `bbi_nonmem_model`
#' @export
check_grd.bbi_nonmem_model <- function(.mod, .iter_floor = 0) {
  check_nonmem_table_bbi(.mod, .iter_floor, .extension = ".grd")
}

#' @describeIn check_nonmem_table_output Checks .grd file from a `bbi_nonmem_summary`
#' @export
check_grd.bbi_nonmem_summary <- function(.mod, .iter_floor = 0) {
  check_nonmem_table_bbi(.mod, .iter_floor, .extension = ".grd")
}

#' @describeIn plot_nonmem_table_df Plot the .grd file
#' @export
plot_grd <- function(.df) {
  plot_nonmem_table_df(.df, .x_var = "ITERATION", .stat_name = "GRADIENT")
}


# wrappers to interact with .ext files easily

#' @rdname check_nonmem_table_output
#' @param .mod Model to check. Either a `bbi_nonmem_model` object or a file path.
#' @param .iter_floor Filters file to only rows with `ITERATION` GREATER THAN this value.
#' @export
check_ext <- function(.mod, .iter_floor = 0) {
  UseMethod("check_ext")
}

#' @describeIn check_nonmem_table_output Checks .ext file from a file path
#' @export
check_ext.character <- function(.mod, .iter_floor = 0) {
  checkmate::assert_string(.mod)
  # if model path passed, construct path
  if (tools::file_ext(.mod) != "ext") {
    .mod = as.character(file.path(.mod, paste0(get_model_id(.mod), ".ext")))
  }

  df <- check_nonmem_table_output(.mod, .x_var = "ITERATION", .x_floor = .iter_floor)
  return(df)
}

#' @describeIn check_nonmem_table_output Checks .ext file from a `bbi_nonmem_model` object
#' @export
check_ext.bbi_nonmem_model <- function(.mod, .iter_floor = 0) {
  check_nonmem_table_bbi(.mod, .iter_floor, .extension = ".ext")
}

#' @describeIn check_nonmem_table_output Checks .ext file from a `bbi_nonmem_summary` object
#' @export
check_ext.bbi_nonmem_summary <- function(.mod, .iter_floor = 0) {
  check_nonmem_table_bbi(.mod, .iter_floor, .extension = ".ext")
}

#' @describeIn plot_nonmem_table_df Plot the .ext file
#' @export
plot_ext <- function(.df) {
  plot_nonmem_table_df(.df, .x_var = "ITERATION", .stat_name = "PARAMETER")
}


#' Private helper to pull a NONMEM table from a model
#' @keywords internal
check_nonmem_table_bbi <- function(.mod, .iter_floor, .extension) {
  ext_path <- build_path_from_model(.mod, .extension)
  df <- check_nonmem_table_output(ext_path, .x_var = "ITERATION", .x_floor = .iter_floor)
  return(df)
}

