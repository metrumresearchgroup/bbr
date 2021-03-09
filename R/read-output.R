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
    cat(paste(res_vec, collapse="\n"))
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
  # if model path passed, construct path
  if (basename(.mod) != "OUTPUT") {
    .mod = as.character(file.path(.mod, "OUTPUT"))
  }

  check_file(.mod, .head, .tail, .print, .return, ...)
}

#' @describeIn check_file Tail the OUTPUT file from a `bbi_nonmem_model` object.
#' @export
tail_output.bbi_nonmem_model <- function(.mod, .head = 3, .tail = 5, .print = TRUE, .return = FALSE, ...) {
  .file <- file.path(get_output_dir(.mod), "OUTPUT")
  check_file(.file, .head, .tail, .print, .return, ...)
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
  # if model path passed, construct path
  if (tools::file_ext(.mod) != "lst") {
    .mod = as.character(file.path(.mod, paste0(get_model_id(.mod), ".lst")))
  }

  check_file(.mod, .head, .tail, .print, .return, ...)
}

#' @describeIn check_file Tail the .lst file from a `bbi_nonmem_model` object.
#' @export
tail_lst.bbi_nonmem_model <- function(.mod, .head = 3, .tail = 5, .print = TRUE, .return = FALSE, ...) {
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

#' @describeIn check_output_dir Takes a `bbi_model` object.
#' @export
check_output_dir.bbi_model <- function(.mod, ...) {
  .output_dir <- .mod %>% get_output_dir()
  .out_files <- check_output_dir(.output_dir, ...)
  return(.out_files)
}


#' Check NONMEM output files
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
#' Creates a line plot of the wide-format tibble output from `check_nonmem_table_output()`
#' @param .df the wide-format tibble output from `check_nonmem_table_output()`
#' @param .x_var String of the variable name to use on the X-axis
#' @param .stat_name String of the name of the stat the other columns represents (like "gradient" or "theta").
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot aes geom_line xlab ylab scale_colour_discrete ggtitle
#' @importFrom forcats fct_inorder
#' @export
plot_nonmem_table_df <- function(.df, .x_var, .stat_name) {
  p <- .df %>% gather("stat", "value", -.data[[.x_var]]) %>%
    mutate(stat = forcats::fct_inorder(.data$stat)) %>%
    ggplot(aes(x=.data[[.x_var]], y=.data$value, colour=.data$stat)) + geom_line() +
    xlab(.x_var) + ylab(paste(.stat_name, "value")) + scale_colour_discrete(name = .stat_name) + ggtitle(paste(.x_var, "x", .stat_name))
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

