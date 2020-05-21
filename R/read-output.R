# Helper functions for reading output files

#' Builds the absolute path to a file in the output directory from components of the `bbi_{.model_type}_model` object
#' @param .mod `bbi_{.model_type}_model` object
#' @param .extension file extension to append (for example `lst`, `ext`, `grd`, etc.)
build_path_from_mod_obj <- function(.mod, .extension) {
  out_file_path <- file.path(.mod[[WORKING_DIR]],
                        .mod[[YAML_OUT_DIR]],
                        paste0(get_model_id(.mod[[YAML_MOD_PATH]]), ".", .extension))
  return(out_file_path)
}

#' Reads the head and tail of specified file and prints it the console and/or returns it as a character vector.
#' @param .file Character scaler of path to file to read
#' @param .head Integer for number of lines to read from the top of the file
#' @param .tail Integer for number of lines to read from the bottom of the file
#' @param .print Boolean for whether to print resulting head and tail to console. Defaults to TRUE.
#' @param .return Boolean for whether to return resulting head and tail as a character vector. Defaults to FALSE.
#' @importFrom readr read_lines
#' @rdname check_file
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

#' S3 generic for tailing the OUTPUT file
#' @param .mod generic input
#' @param ... args passed through
#' @export
#' @rdname check_file
tail_output <- function(.mod, ...) {
  UseMethod("tail_output")
}

#' S3 dispatch for tailing the OUTPUT file
#' @param .mod Path to OUTPUT file
#' @param .head Integer for number of lines to read from the top of the file
#' @param .tail Integer for number of lines to read from the bottom of the file
#' @param .print Boolean for whether to print resulting head and tail to console. Defaults to TRUE.
#' @param .return Boolean for whether to return resulting head and tail as a character vector. Defaults to FALSE.
#' @param ... arguments passed through to check_file
#' @export
#' @rdname check_file
tail_output.character <- function(.mod, .head = 3, .tail = 5, .print = TRUE, .return = FALSE, ...) {
  # if model path passed, construct path
  if (basename(.mod) != "OUTPUT") {
    .mod = as.character(file.path(.mod, "OUTPUT"))
  }

  check_file(.mod, .head, .tail, .print, .return, ...)
}

#' S3 dispatch for tailing the OUTPUT file
#' @param .mod bbi_nonmem_model object
#' @param .head Integer for number of lines to read from the top of the file
#' @param .tail Integer for number of lines to read from the bottom of the file
#' @param .print Boolean for whether to print resulting head and tail to console. Defaults to TRUE.
#' @param .return Boolean for whether to return resulting head and tail as a character vector. Defaults to FALSE.
#' @param ... arguments passed through to check_file
#' @export
#' @rdname check_file
tail_output.bbi_nonmem_model <- function(.mod, .head = 3, .tail = 5, .print = TRUE, .return = FALSE, ...) {
  .file <- file.path(.mod[[WORKING_DIR]], .mod[[YAML_OUT_DIR]], "OUTPUT")
  check_file(.file, .head, .tail, .print, .return, ...)
}


# wrappers to interact easily with .lst file

#' S3 generic for tailing the lst file
#' @param .mod generic input
#' @param ... args passed through
#' @export
#' @rdname check_file
tail_lst <- function(.mod, ...) {
  UseMethod("tail_lst")
}

#' S3 dispatch for tailing the lst file
#' @param .mod Path to lst file
#' @param .head Integer for number of lines to read from the top of the file
#' @param .tail Integer for number of lines to read from the bottom of the file
#' @param .print Boolean for whether to print resulting head and tail to console. Defaults to TRUE.
#' @param .return Boolean for whether to return resulting head and tail as a character vector. Defaults to FALSE.
#' @export
#' @rdname check_file
tail_lst.character <- function(.mod, .head = 3, .tail = 5, .print = TRUE, .return = FALSE, ...) {
  # if model path passed, construct path
  if (tools::file_ext(.mod) != "lst") {
    .mod = as.character(file.path(.mod, paste0(get_model_id(.mod), ".lst")))
  }

  check_file(.mod, .head, .tail, .print, .return, ...)
}

#' S3 dispatch for tailing the lst file
#' @param .mod bbi_nonmem_model object
#' @param .head Integer for number of lines to read from the top of the file
#' @param .tail Integer for number of lines to read from the bottom of the file
#' @param .print Boolean for whether to print resulting head and tail to console. Defaults to TRUE.
#' @param .return Boolean for whether to return resulting head and tail as a character vector. Defaults to FALSE.
#' @export
#' @rdname check_file
tail_lst.bbi_nonmem_model <- function(.mod, .head = 3, .tail = 5, .print = TRUE, .return = FALSE, ...) {
  .file <- build_path_from_mod_obj(.mod, "lst")
  check_file(.file, .head, .tail, .print, .return, ...)
}


#' List files in the output directory to glance at where the process is
#' @param .mod generic res
#' @param ... values to pass to `fs::dir_ls`
#' @rdname check_output_dir
#' @export
check_output_dir <- function(.mod, ...) {
  UseMethod("check_output_dir")
}

#' S3 dispatch to list output directory files from a directory path
#' @param .mod Character scaler of path to output directory
#' @rdname check_output_dir
#' @export
check_output_dir.character <- function(.mod, ...) {
  .out_files <- fs::dir_ls(.mod, ...)
  return(.out_files)
}

#' S3 dispatch to list output directory files from a `bbi_{.model_type}_model` object
#' @param .mod The `bbi_{.model_type}_model` object
#' @rdname check_output_dir
#' @export
check_output_dir.bbi_nonmem_model <- function(.mod, ...) {
  .output_dir <- .mod %>% get_output_dir()
  .out_files <- check_output_dir(.output_dir, ...)
  return(.out_files)
}


#' Checks nonmem output file that's a whitespace-delimited file (for instance .grd or .ext)
#' @param .path Character scalar path to the gradient file
#' @param .x_var name of variable to filter with `.x_floor`
#' @param .x_floor Filters file to only rows with `.x_var` GREATER THAN this value.
#' @importFrom readr read_table2 cols
#' @importFrom dplyr filter
#' @rdname check_nonmem_table_output
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


#' Creates a line plot of the wide-format tibble output from check_nonmem_table_output
#' @param .df the wide-format tibble output from check_nonmem_table_output
#' @param .x_var Character scaler for the variable name to use on the X-axis
#' @param .stat_name Character scaler for the name of the stat the other columns represents (like "gradient" or "theta").
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot aes geom_line xlab ylab scale_colour_discrete ggtitle
#' @importFrom forcats fct_inorder
#' @rdname plot_nonmem_table_df
#' @export
plot_nonmem_table_df <- function(.df, .x_var, .stat_name) {
  p <- .df %>% gather("stat", "value", -.data[[.x_var]]) %>%
    mutate(stat = forcats::fct_inorder(.data$stat)) %>%
    ggplot(aes(x=.data[[.x_var]], y=.data$value, colour=.data$stat)) + geom_line() +
    xlab(.x_var) + ylab(paste(.stat_name, "value")) + scale_colour_discrete(name = .stat_name) + ggtitle(paste(.x_var, "x", .stat_name))
  return(p)
}


# wrappers to interact with .grd files easily

#' S3 generic for checking grd file
#' @param .mod generic input
#' @param .iter_floor Filters file to only rows with `ITERATION` GREATER THAN this value.
#' @export
#' @rdname check_nonmem_table_output
check_grd <- function(.mod, .iter_floor = 0) {
  UseMethod("check_grd")
}

#' S3 dispatch for checking grd file
#' @param .mod Path to file
#' @export
#' @rdname check_nonmem_table_output
check_grd.character <- function(.mod, .iter_floor = 0) {
  # if model path passed, construct path
  if (tools::file_ext(.mod) != "grd") {
    .mod = as.character(file.path(.mod, paste0(get_model_id(.mod), ".grd")))
  }

  df <- check_nonmem_table_output(.mod, .x_var = "ITERATION", .x_floor = .iter_floor)
  return(df)
}

#' S3 dispatch for checking grd file
#' @param .mod `bbi_nonmem_model` object
#' @export
#' @rdname check_nonmem_table_output
check_grd.bbi_nonmem_model <- function(.mod, .iter_floor = 0) {
  grd_path <- build_path_from_mod_obj(.mod, "grd")
  df <- check_nonmem_table_output(grd_path, .x_var = "ITERATION", .x_floor = .iter_floor)
  return(df)
}

#' S3 dispatch for plotting grd file
#' @param .df tibble from from check_grd() output
#' @export
#' @rdname plot_nonmem_table_df
plot_grd <- function(.df) {
  plot_nonmem_table_df(.df, .x_var = "ITERATION", .stat_name = "GRADIENT")
}


# wrappers to interact with .ext files easily

#' S3 generic for checking ext file
#' @param .mod generic input
#' @param .iter_floor Filters file to only rows with `ITERATION` GREATER THAN this value.
#' @export
#' @rdname check_nonmem_table_output
check_ext <- function(.mod, .iter_floor = 0) {
  UseMethod("check_ext")
}

#' S3 dispatch for checking ext file
#' @param .mod path to ext file
#' @export
#' @rdname check_nonmem_table_output
check_ext.character <- function(.mod, .iter_floor = 0) {
  # if model path passed, construct path
  if (tools::file_ext(.mod) != "ext") {
    .mod = as.character(file.path(.mod, paste0(get_model_id(.mod), ".ext")))
  }

  df <- check_nonmem_table_output(.mod, .x_var = "ITERATION", .x_floor = .iter_floor)
  return(df)
}

#' S3 dispatch for checking ext file
#' @param .mod `bbi_nonmem_model` object
#' @export
#' @rdname check_nonmem_table_output
check_ext.bbi_nonmem_model <- function(.mod, .iter_floor = 0) {
  ext_path <- build_path_from_mod_obj(.mod, "ext")
  df <- check_nonmem_table_output(ext_path, .x_var = "ITERATION", .x_floor = .iter_floor)
  return(df)
}

#' S3 dispatch for plotting ext file
#' @param .df tibble from from check_ext() output
#' @export
#' @rdname plot_nonmem_table_df
plot_ext <- function(.df) {
  plot_nonmem_table_df(.df, .x_var = "ITERATION", .stat_name = "GRADIENT")
}


