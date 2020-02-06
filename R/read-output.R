#options(readr.num_columns = 0) # turn off readr parsing message
#options(crayon.enabled = FALSE) # turn off color tibble printing


#' Checks nonmem output file that's a whitespace-delimited file (for instance .grd or .ext)
#' @param .path Character scalar path to the gradient file
#' @param .plot Boolean for whether to return a plot instead of a tibble
#' @param .iter_floor Filters gradient file to only iterations GREATER THAN this value. Default is 0.
#' @importFrom readr read_table2
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot
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

# grd <- "inst/nonmem/acop/acop.grd"
# check_nonmem_output_file(grd)
# check_nonmem_output_file(grd, .plot=T)
#
# ext <- "inst/nonmem/acop/acop.ext"
# check_nonmem_output_file(ext)
# check_nonmem_output_file(ext, .plot=T)


##### make a read_nonmem_table_file() for the .tab and par.tab ?



check_file <- function(.file, .head = 3, .tail = 5) {
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

# check_file(ext)
# check_file(ext, .tail=2)
# check_file(ext, .tail=10)
# check_file(ext, .tail=20)

tail_OUTPUT <- function(.x, ...) {
  UseMethod("tail_OUTPUT", .x)
}

tail_OUTPUT.babylon_result <- function(.x) {
  check_file(paste0(.x$output_dir, "OUTPUT", sep="/"))
}

tail_OUTPUT.character <- function(.x) {
  check_file(paste0(.x, "OUTPUT", sep="/"))
}

tail_OUTPUT.numeric <- function(.x) {
  check_file(paste0(as.character(.x), "OUTPUT", sep="/"))
}


# tail_lst <- function(.x, ...) {
#   UseMethod("tail_lst", .x)
# }
#
# tail_lst.babylon_result <- function(.x) {
#   check_file(paste0(.x$output_dir, "lst", sep="/"))
# }
#
# tail_lst.character <- function(.x) {
#   check_file(paste0(.x, "lst", sep="/"))
# }
#
# tail_lst.numeric <- function(.x) {
#   check_file(paste0(as.character(.x), "lst", sep="/"))
# }



#' Execute `bbi summary` to generate json output of model results
#' @param .path Full path to a model that has been run that user wants to summarize
#' @param .model_type Type of model to summarize. Currently only supports "nonmem"
#' @export
model_summary <- function(.path, .model_type = c("nonmem")) {
  .model_type <- match.arg(.model_type)

  # execute summary command
  cmd_args <- c(.model_type, "summary", "--json", .path)
  output <- bbi_exec(cmd_args)

  # parse json output
  return(parse_model_results(output$stdout))
}

#' Reads model results to a list by parsing json output from bbi summary
#' @param .x summary output string
#' @param ... params to pass to jsonlite::fromJSON
#' @param file model json summary output file path
#' @export
#' @importFrom jsonlite fromJSON read_json
parse_model_results <- function(.x, ..., file = NULL) {
  if (!is.null(file)) {
    return(jsonlite::read_json(file = file, simplifyDataFrame = FALSE))
  }
  return(jsonlite::fromJSON(.x, ..., simplifyDataFrame = FALSE))
}
