
#' Read all tables and input data from NONMEM run
#' @name nm_tables
NULL

#' @describeIn nm_tables Extract paths to table output files from NONMEM control
#'   stream, and optionally check if the files exist.
#' @param .check_exists If `TRUE` check if files exist.
#' @importFrom stringr str_extract str_replace
#' @importFrom purrr map_chr
#' @export
nm_table_files <- function(.mod, .check_exists = TRUE) {
  .p <- get_model_path(.mod)
  .l <- parse_ctl_to_list(.p)

  # get file names from table statements and construct paths
  .f <- .l[names(.l) == "TABLE"] %>%
    map_chr(~paste(.x, collapse = " ")) %>%
    str_extract("\\bFILE\\s*=\\s*([^ ]+)") %>%
    str_replace("\\bFILE\\s*=\\s*", "") %>%
    str_replace("^\\Q./\\E", "") %>%
    file.path(get_output_dir(.mod, .check_exists = .check_exists), .)

  if(isTRUE(.check_exists)) {
    .fe <- fs::file_exists(.f)
    if (any(!.fe)) {
      stop(paste(
        glue("Parsed {length(.f)} table files from {.p} but the following files do not exist:"),
        paste(names(.fe)[!.fe], collapse = ", ")
      ))
    }
  }
  return(.f)
}


#####################################
# PRIVATE HELPERS AND IMPLEMENTATION


#' Parses NONMEM control stream to a list of $ blocks.
#' Adapted from mrgsolve.
#' @param .path File path to control stream
#' @importFrom readr read_lines
#' @keywords internal
parse_ctl_to_list <- function(.path) {
  checkmate::assert_string(.path)
  txt <- read_lines(.path)

  block_re <- "^\\s*\\$[A-Za-z]\\w*"

  # Look for block lines
  m <- regexec(block_re, txt)

  # Where the block starts
  start <- which(sapply(m, "[", 1L) > 0)

  if(length(start)==0) {
    stop("No model specification file blocks were found.", call.=FALSE)
  }

  # Get the matches
  mm <- regmatches(txt[start], m[start])

  # Block labels
  labs <- gsub("[$ ]", "", sapply(mm, "[", 1L))

  # Remove block label text
  txt[start] <- trimws(substr(txt[start], nchar(unlist(mm,use.names=FALSE))+1, nchar(txt[start])))

  # Where the block ends
  end <- c((start-1),length(txt))[-1]

  # Create the list
  spec <- lapply(seq_along(start), function(i) {
    y <- txt[start[i]:end[i]]
  })

  names(spec) <- labs

  return(spec)
}
