#' Reads the head and tail of specified file and prints it the console and/or returns it as a character vector.
#' @param .file Character scaler of path to file to read
#' @param .head Integer for number of lines to read from the top of the file
#' @param .tail Integer for number of lines to read from the bottom of the file
#' @importFrom readr read_lines
check_file <- function(.file, .head = 3, .tail = 5, .print = TRUE, .return = FALSE) {
  l <- read_lines(.file)

  l_len <- length(l)

  if (.head >= l_len) {
    .head <- l_len
    .tail <- 0
  }

  if (.tail <= 0) {
    tail_start <- NULL
  } else {
    tail_start <- 1 + l_len - .tail

    if(tail_start <= .head) {
      tail_start <- .head + 1
    }
  }

  if (!is.null(tail_start)) {
    tail_vec <- l[tail_start:l_len]

    if (tail_start > .head + 1) {
      tail_vec <- c("...", tail_vec)
    }
  } else {
    tail_vec <- ""
  }

  res_vec <- c(l[1:.head], tail_vec)
  cat(paste(res_vec, collapse="\n"))
  invisible()
}


# wrappers to interact easily with OUTPUT file
tail_output <- function(.f, ...) {
  UseMethod("tail_output", .f)
}

tail_output.character <- function(.file, .head = 3, .tail = 5, .print = TRUE, .return = FALSE) {
  # if model id passed, construct path
  if (get_mod_id(.file) == .file) {
    .file = as.character(glue("{.file}/OUTPUT"))
  }

  check_file(.file, .head, .tail, .print, .return)
}

tail_output.bbi_nonmem_result <- function(.res, .head = 3, .tail = 5, .print = TRUE, .return = FALSE) {
  .file <- file.path(.res$output_dir, "OUTPUT")
  check_file(.file, .head, .tail, .print, .return)
}


# wrappers to interact easily with .lst file
tail_lst <- function(.f, ...) {
  UseMethod("tail_lst", .f)
}

tail_lst.character <- function(.file, .head = 3, .tail = 5, .print = TRUE, .return = FALSE) {
  # if model id passed, construct path
  if (get_mod_id(.file) == .file) {
    .file = as.character(glue("{.file}/{.file}.lst"))
  }

  check_file(.file, .head, .tail, .print, .return)
}

tail_lst.bbi_nonmem_result <- function(.res, .head = 3, .tail = 5, .print = TRUE, .return = FALSE) {
  .file <- file.path(.res$output_dir, paste0(get_mod_id(.res$model_path), ".lst"))
  check_file(.file, .head, .tail, .print, .return)
}

#' Checks nonmem output file that's a whitespace-delimited file (for instance .grd or .ext)
#' @param .path Character scalar path to the gradient file
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


#' Creates a line plot of the wide-format tibble output from check_nonmem_table_output
#' @param .df the wide-format tibble output from check_nonmem_table_output
#' @param .x_var Character scaler for the variable name to use on the X-axis
#' @param .stat_name Character scaler for the name of the stat the other columns represents (like "gradient" or "theta").
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot aes geom_line xlab ylab scale_colour_discrete ggtitle
#' @export
plot_nonmem_table_df <- function(.df, .x_var, .stat_name) {
  p <- .df %>% gather("stat", "value", -.data[[.x_var]]) %>%
    ggplot(aes(x=.data[[.x_var]], y=.data$value, colour=.data$stat)) + geom_line() +
    xlab(.x_var) + ylab(paste(.stat_name, "value")) + scale_colour_discrete(name = .stat_name) + ggtitle(paste(.x_var, "x", .stat_name))
  return(p)
}


# wrappers to interact with .grd files easily
check_grd <- function(.x, ...) {
  UseMethod("check_grd", .x)
}

check_grd.character <- function(.path, .iter_floor = 0) {
  # if model id passed, construct path
  if (get_mod_id(.path) == .path) {
    .path = as.character(glue("{.path}/{.path}.grd"))
  }

  df <- check_nonmem_table_output(.path, .x_var = "ITERATION", .x_floor = .iter_floor)
  return(df)
}

check_grd.bbi_nonmem_result <- function(.res, .iter_floor = 0) {
  grd_path <- file.path(.res$output_dir, paste0(get_mod_id(.res$model_path), ".grd"))
  df <- check_nonmem_table_output(grd_path, .x_var = "ITERATION", .x_floor = .iter_floor)
  return(df)
}

plot_grd <- function(.df, ...) {
  plot_nonmem_table_df(.df, .x_var = "ITERATION", .stat_name = "GRADIENT")
}



# wrappers to interact with .grd files easily
check_ext <- function(.x, ...) {
  UseMethod("check_ext", .x)
}

check_ext.character <- function(.path, .iter_floor = 0) {
  # if model id passed, construct path
  if (get_mod_id(.path) == .path) {
    .path = as.character(glue("{.path}/{.path}.ext"))
  }

  df <- check_nonmem_table_output(.path, .x_var = "ITERATION", .x_floor = .iter_floor)
  return(df)
}

check_ext.bbi_nonmem_result <- function(.res, .iter_floor = 0) {
  ext_path <- file.path(.res$output_dir, paste0(get_mod_id(.res$model_path), ".ext"))
  df <- check_nonmem_table_output(ext_path, .x_var = "ITERATION", .x_floor = .iter_floor)
  return(df)
}

plot_ext <- function(.df, ...) {
  plot_nonmem_table_df(.df, .x_var = "ITERATION", .stat_name = "GRADIENT")
}


