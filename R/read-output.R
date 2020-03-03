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
    invisible()
  }
}


# wrappers to interact easily with OUTPUT file

#' S3 generic for tailing the OUTPUT file
#' @param .f generic .f
#' @param ... args passed through
#' @export
#' @rdname check_file
tail_output <- function(.f, ...) {
  UseMethod("tail_output", .f)
}

#' S3 dispatch for tailing the OUTPUT file
#' @param .file Path to OUTPUT file
#' @export
#' @rdname check_file
tail_output.character <- function(.file, .head = 3, .tail = 5, .print = TRUE, .return = FALSE) {
  # if model path passed, construct path
  if (basename(.file) != "OUTPUT") {
    .file = as.character(file.path(.file, "OUTPUT"))
  }

  check_file(.file, .head, .tail, .print, .return)
}

#' S3 dispatch for tailing the OUTPUT file
#' @param .mod bbi_nonmem_model object
#' @export
#' @rdname check_file
tail_output.bbi_nonmem_model <- function(.mod, .head = 3, .tail = 5, .print = TRUE, .return = FALSE) {
  .file <- file.path(.mod[[WORKING_DIR]], .mod[[YAML_OUT_DIR]], "OUTPUT")
  check_file(.file, .head, .tail, .print, .return)
}


# wrappers to interact easily with .lst file

#' S3 generic for tailing the lst file
#' @param .f generic .f
#' @param ... args passed through
#' @export
#' @rdname check_file
tail_lst <- function(.f, ...) {
  UseMethod("tail_lst", .f)
}

#' S3 dispatch for tailing the lst file
#' @param .file Path to lst file
#' @export
#' @rdname check_file
tail_lst.character <- function(.file, .head = 3, .tail = 5, .print = TRUE, .return = FALSE) {
  # if model path passed, construct path
  if (tools::file_ext(.file) != "lst") {
    .file = as.character(file.path(.file, paste0(get_mod_id(.file), ".lst")))
  }

  check_file(.file, .head, .tail, .print, .return)
}

#' S3 dispatch for tailing the lst file
#' @param .mod bbi_nonmem_model object
#' @export
#' @rdname check_file
tail_lst.bbi_nonmem_model <- function(.mod, .head = 3, .tail = 5, .print = TRUE, .return = FALSE) {
  .file <- build_path_from_mod_obj(.mod, "lst")
  check_file(.file, .head, .tail, .print, .return)
}


#' List files in the output directory to glance at where the process is
#' @param .mod generic res
#' @rdname check_output_dir
#' @export
check_output_dir <- function(.mod, ...) {
  UseMethod("check_output_dir", .mod)
}

#' S3 dispatch to list output directory files from a directory path
#' @param .output_dir Character scaler of path to output directory
#' @param .filter Optional Character scaler of regex to filter filenames on and only return matches
#' @rdname check_output_dir
#' @export
check_output_dir.character <- function(.output_dir, .filter = NULL) {
  .out_files <- fs::dir_ls(.output_dir)

  # optionally filter results
  if(!is.null(.filter)) {
    .out_files <- str_subset(.out_files, .filter)
  }

  return(.out_files)
}

#' S3 dispatch to list output directory files from a `bbi_{.model_type}_model` object
#' @param .mod The `bbi_{.model_type}_model` object
#' @param .filter Optional Character scaler of regex to filter filenames on and only return matches
#' @rdname check_output_dir
#' @export
check_output_dir.bbi_nonmem_model <- function(.mod, .filter = NULL) {
  .output_dir <- .mod %>% get_output_dir()
  .out_files <- check_output_dir(.output_dir, .filter)
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
#' @rdname plot_nonmem_table_df
#' @export
plot_nonmem_table_df <- function(.df, .x_var, .stat_name) {
  p <- .df %>% gather("stat", "value", -.data[[.x_var]]) %>%
    ggplot(aes(x=.data[[.x_var]], y=.data$value, colour=.data$stat)) + geom_line() +
    xlab(.x_var) + ylab(paste(.stat_name, "value")) + scale_colour_discrete(name = .stat_name) + ggtitle(paste(.x_var, "x", .stat_name))
  return(p)
}


# wrappers to interact with .grd files easily

#' S3 generic for checking grd file
#' @param .x generic .x
#' @param ... args passed through
#' @export
#' @rdname check_nonmem_table_output
check_grd <- function(.x, ...) {
  UseMethod("check_grd", .x)
}

#' S3 dispatch for checking grd file
#' @param .path Path to file
#' @param .iter_floor Filters file to only rows with `ITERATION` GREATER THAN this value.
#' @export
#' @rdname check_nonmem_table_output
check_grd.character <- function(.path, .iter_floor = 0) {
  # if model path passed, construct path
  if (tools::file_ext(.path) != "grd") {
    .path = as.character(file.path(.path, paste0(get_mod_id(.path), ".grd")))
  }

  df <- check_nonmem_table_output(.path, .x_var = "ITERATION", .x_floor = .iter_floor)
  return(df)
}

#' S3 dispatch for checking grd file
#' @param .mod `bbi_nonmem_model` object
#' @param .iter_floor Filters file to only rows with `ITERATION` GREATER THAN this value.
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
#' @param .x generic .x
#' @param ... args passed through
#' @export
#' @rdname check_nonmem_table_output
check_ext <- function(.x, ...) {
  UseMethod("check_ext", .x)
}

#' S3 dispatch for checking ext file
#' @param .path path to ext file
#' @param .iter_floor Filters file to only rows with `ITERATION` GREATER THAN this value.
#' @export
#' @rdname check_nonmem_table_output
check_ext.character <- function(.path, .iter_floor = 0) {
  # if model path passed, construct path
  if (tools::file_ext(.path) != "ext") {
    .path = as.character(file.path(.path, paste0(get_mod_id(.path), ".ext")))
  }

  df <- check_nonmem_table_output(.path, .x_var = "ITERATION", .x_floor = .iter_floor)
  return(df)
}

#' S3 dispatch for checking ext file
#' @param .mod `bbi_nonmem_model` object
#' @param .iter_floor Filters file to only rows with `ITERATION` GREATER THAN this value.
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

#' Check the progress of a NONMEM run from the bbi_nonmem_model object
#' @param .mod `bbi_nonmem_summary` object to check on
#' @param .ext_wait Integer number of seconds to wait for an .ext file to be there before exiting with FALSE
#' @importFrom fs file_exists
#' @export
check_nonmem_progress <- function(.mod, .ext_wait = 30) {
  # look for ext file
  SLEEP = 1
  ext_path <- build_path_from_mod_obj(.mod, "ext")
  if (!fs::file_exists(ext_path)) {
    while (.ext_wait > 0) {
      if (fs::file_exists(ext_path)) {
        break
      } else {
        cat(as.character(glue("Can't find {ext_path}. Waiting {.ext_wait} more seconds...")), sep = "\n")
        Sys.sleep(SLEEP)
        .ext_wait <- .ext_wait - SLEEP
      }
    }
    if (.ext_wait <= 0) {
      warning(glue("Can't find {ext_path}. Your run may still be queued or it may have failed. Try increasing `.wait_ext` if you want to continue checking."))
      return(FALSE)
    }
  }

  # if found, check if ext file has rows with negative iterations (which means it finished)
  done_rows <- check_ext(.mod, .iter_floor = NULL) %>% filter(.data$ITERATION < 0) %>% nrow()
  if (done_rows > 0) {
    return(TRUE)
  } else {
    # try to get tail of OUTPUT file
    out_tail <- tryCatch(
      tail_output(.mod, .tail = 10, .head = 0, .print = FALSE, .return = TRUE),
      error = function(e) {
        warning(glue("{ext_path} file does not look finished but there is also no `{.mod[[YAML_OUT_DIR]]}/OUTPUT` file. Your run may have failed."))
        return(FALSE)
      }
    )
    cat(paste(
      glue("\n\n---\nModel is still running. Tail of `{.mod[[YAML_OUT_DIR]]}/OUTPUT` file:"), "\n---\n",
      paste(out_tail, collapse = "\n")
    ))
    return(FALSE)
  }
}
