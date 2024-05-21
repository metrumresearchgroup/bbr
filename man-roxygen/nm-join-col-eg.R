#' @section Join column:
#'
#' #### Example: creating a row number `.join_col`
#'
#' Take the following `$INPUT` and `$DATA` records:
#' ```
#' $INPUT ID TIME MDV EVID DV AMT SEX WT ETN
#' $DATA ../../../../extdata/acop.csv
#' ```
#'
#' Before submitting a model, read in the data and add a row number column;
#' ```
#' library(dplyr)
#' data <- nm_data(.mod) %>% mutate(NUM = 1:n())
#' readr::write_csv(data, get_data_path(.mod))
#' ```
#'
#' Then add `'NUM'` to the list of input columns:
#' ```
#' $INPUT ID TIME MDV EVID DV AMT SEX WT ETN *NUM*
#' ```
