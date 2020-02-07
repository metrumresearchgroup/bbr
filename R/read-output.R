#options(readr.num_columns = 0) # turn off readr parsing message
#options(crayon.enabled = FALSE) # turn off color tibble printing


#' Checks nonmem output file that's a whitespace-delimited file (for instance .grd or .ext)
#' @param .path Character scalar path to the gradient file
#' @param .plot Boolean for whether to return a plot instead of a tibble
#' @param .iter_floor Filters gradient file to only iterations GREATER THAN this value. Default is 0.
#' @importFrom readr read_table2
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot
#' @export
check_nonmem_output_file <- function(.path, .plot = FALSE, .iter_floor = 0) {
  df <- read_table2(.path, skip=1) %>% filter(ITERATION > .iter_floor)
  if (!.plot) {
    return(df)
  } else {
    p <- df %>% gather("gradient", "value", -ITERATION) %>%
      ggplot(aes(x=ITERATION, y=value, colour=gradient)) + geom_line()
    return(p)
  }
}


##### make a read_nonmem_table_file() for the .tab and par.tab ?


#' Prints the head and tail of a file to the console
#' @param .file The file to look at
#' @param .head number of lines to look at for the head of the file (default 3)
#' @param .iter_floor number of lines to look at for the head of the file (default 10)
#' @importFrom readr read_lines
#' @export
check_file <- function(.file, .head = 3, .tail = 10) {
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

  cat(paste(c(l[1:.head], tail_vec), collapse="\n"))
  invisible()
}

# check_file helper for OUTPUT file
tail_OUTPUT <- function(.x, ...) {
  UseMethod("tail_OUTPUT", .x)
}

tail_OUTPUT.babylon_result <- function(.x, ...) {
  check_file(paste0(.x$output_dir, "OUTPUT", sep="/"), ...)
}

tail_OUTPUT.character <- function(.x, ...) {
  check_file(paste0(.x, "OUTPUT", sep="/"), ...)
}

tail_OUTPUT.numeric <- function(.x, ...) {
  check_file(paste0(as.character(.x), "OUTPUT", sep="/"), ...)
}

# check_file helper for .lst file
tail_lst <- function(.x, ...) {
  UseMethod("tail_lst", .x)
}

tail_lst.babylon_result <- function(.x, ...) {
  check_file(glue("{.x$output_dir}/{basename(.x$output_dir)}.lst"), ...)
}

tail_lst.character <- function(.x, ...) {
  check_file(glue("{.x}/{.x}.lst"), ...)
}

tail_lst.numeric <- function(.x, ...) {
  check_file(glue("{.x}/{.x}.lst"), ...)
}

