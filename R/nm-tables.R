
#' Read all tables and input data
#'
#' Reads in the input data set and all table output files from a NONMEM run.
#' This function will return a named list with all the relevant tibbles. To return
#' a _single tibble_ with the input data joined to the relevant table outputs, use
#' the related [nm_join()] function. This function will print the number of rows
#' and columns when each file is loaded. This **printing can be suppressed** by
#' setting `options(bbr.verbose = FALSE)`.
#'
#' @details
#' As described in Value, `nm_tables()` returns a named list of tibble(s). To
#' return a single tibble will all of this data joined together, see
#' [nm_join()].
#'
#' Because `nm_tables()` calls [nm_file()] internally, it is _not_ compatible
#' with multiple tables written to a single file. See "Details" in [nm_file()]
#' for alternatives.
#'
#' @return A named list of tibbles. The first element will always be named
#'   `data` and will contain the input data set. Subsequent elements will be
#'   named for the file from which they were loaded, with `get_model_id(.mod)`
#'   (and `.`) removed from the beginning and end, if present. For example, a
#'   model named `001.ctl` that generated a table named `001.tab` will have the
#'   relevant element named `tab`. Column names in all tibbles will be converted
#'   to uppercase.
#' @param .mod A `bbi_nonmem_model` or `bbi_nonmem_summary` object, or a path to
#'   a NONMEM run.
#' @param .files Character vector of file paths to table files to read in.
#'   Defaults to calling [nm_table_files()] on `.mod`, which will parse all file
#'   names from `$TABLE` blocks in the control stream. If passing manually,
#'   paths should be either absolute, or relative to `get_output_dir(.mod)`.
#' @importFrom purrr compact map_chr
#' @importFrom stringr str_replace
#' @seealso [nm_join()], [nm_file()]
#' @export
nm_tables <- function(
  .mod,
  .files = nm_table_files(.mod)
) {
  if (inherits(.mod, "character")) {
    checkmate::assert_string(.mod)
    .mod <- read_model(.mod)
  }
  check_model_object(.mod, c(NM_MOD_CLASS, NM_SUM_CLASS))
  checkmate::assert_character(.files)

  # make paths absolute
  .files <- map_chr(.files, ~{
    if (fs::is_absolute_path(.x)) {
      return(.x)
    } else {
      return(file.path(get_output_dir(.mod), .x))
    }
  })

  # read in input data
  res <- list(
    data = nm_data(.mod)
  )

  # build names for table elements
  mod_id <- get_model_id(.mod)
  .n <- .files %>%
    basename() %>%
    str_replace(glue("^{mod_id}"), "") %>%
    str_replace(glue("{mod_id}$"), "") %>%
    make.names() %>%
    str_replace(glue("^\\."), "") %>%
    str_replace(glue("\\.$"), "")

  # read in each table file
  for (.i in 1:length(.files)) {
    res[[.n[.i]]] <- nm_file(.files[.i])
  }
  return(compact(res))
}

#' @describeIn nm_tables Extract paths to table output files from NONMEM control
#'   stream, and optionally check if the files exist.
#' @param .check_exists If `TRUE` check if files exist.
#' @importFrom stringr str_extract str_replace
#' @importFrom purrr map_chr
#' @export
nm_table_files <- function(.mod, .check_exists = TRUE) {
  .p <- get_model_path(.mod)
  .l <- parse_ctl_to_list(.p)
  out_dir <- get_output_dir(.mod, .check_exists = .check_exists)

  # get file names from table statements and construct paths
  .f <- .l[grep("^TAB", names(.l))] %>%
    map_chr(~paste(.x, collapse = " ")) %>%
    str_extract("\\bFILE\\s*=\\s*([^ ]+)") %>%
    str_replace("\\bFILE\\s*=\\s*", "") %>%
    str_replace("^\\.\\/", "") %>%
    file.path(out_dir, .)

  if(rlang::is_empty(.f)){
    stop(glue("No table files were found in {.p}"))
  }

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
