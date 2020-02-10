#' Checks nonmem output file that's a whitespace-delimited file (for instance .grd or .ext)
#' @param .path Character scalar path to the gradient file
#' @param .plot Boolean for whether to return a plot instead of a tibble
#' @param .iter_floor Filters gradient file to only iterations GREATER THAN this value. Default is 0.
#' @importFrom readr read_table2
#' @importFrom tidyr gather
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot geom_line
#' @export
check_nonmem_output_file <- function(.path, .plot = FALSE, .iter_floor = 0) {
  # read file
  df <- read_table2(.path, skip=1, col_types = cols())

  # filter out early iterations (and final)
  if("ITERATION" %in% names(df) && !is.null(.iter_floor)) {
    df <- df %>% filter(ITERATION > .iter_floor)
  }

  # return result
  if (!.plot) {
    return(df)
  } else {
    p <- df %>% gather("gradient", "value", -ITERATION) %>%
      ggplot(aes(x=ITERATION, y=value, colour=gradient)) + geom_line()
    return(p)
  }
}
