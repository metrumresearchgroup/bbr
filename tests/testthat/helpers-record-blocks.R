


# Helper Functions --------------------------------------------------------


#' Function for making an example matrix for replacement
#'
#' @param n size of the matrix (n x n)
#' @param values vector of values to set. All others will be 0.
#' @param block_loc vector indicating the location of the values. Should be the
#'        same length as `values` and created with `block()`
#'
#' @example
#' \dontrun{
#' make_matrix(
#'  n = 30,
#'  values = c(c(0.1, 0.01, 0.1), c(0, 0), c(0.1, 0.01, 0.12), rep(0.1, 12), rep(0, 12)) + 1,
#'  block_loc = c(block(2), rep(block(1), 2), block(2), rep(block(1), 12), rep(block(1), 12))
#' )
#' }
#'
make_matrix <- function(n, values, block_loc) {

  if(length(values) != length(block_loc)){
    stop("`values` and `block_loc` should have the same length")
  }

  index_strings <- build_matrix_indices(block_loc)

  # matrix setup and spec
  mat <- matrix(0, n, n)
  mat_spec <- purrr::map2_dfr(index_strings, values, function(index_str, val) {
    indices <- as.numeric(unlist(strsplit(gsub("\\(|\\)", "", index_str), ",")))
    return(tibble::tibble(row=indices[1], col=indices[2], value = val))
  })

  # Iterate through the rows of mat_spec and assign values to the matrix
  for (i in 1:nrow(mat_spec)) {
    mat[mat_spec$row[i], mat_spec$col[i]] <- mat_spec$value[i]
  }

  # Make upper-triangular matrix
  mat <- t(mat)

  return(mat)
}


# Function to read the content of a file and extract the list object
read_and_extract_list <- function(file_path) {

  # Example Directory
  record_example_dir <- system.file(
    file.path("test-refs", "inherit-estimates"), package = "bbr", mustWork = TRUE
  )

  file_content <- readLines(file.path(record_example_dir, file_path))
  list_object <- eval(parse(text = paste(file_content, collapse = "\n")))

  # Remove leading newlines from strings (if any)
  # (trailing new lines would be part of the nmrec record)
  list_object$result_ctl <- gsub("^\n+", "", list_object$result_ctl)
  list_object$input_ctl <- gsub("^\n+", "", list_object$input_ctl)

  # Function to create nmrec object
  make_fake_ctl <- function(case = NULL, input_ctl = NULL){

    template_lines <- glue::glue("$PROBLEM {case}\n\n{input_ctl}") %>%
      as.character() %>% strsplit("\n") %>% unlist()

    ctl <- nmrec::parse_ctl(template_lines)
    return(ctl)
  }

  # Create nmrec object
  list_object$input_nmrec <- make_fake_ctl(
    case = list_object$case,
    input_ctl = list_object$input_ctl
  )
  # Sort list
  list_object <- list_object[sort(names(list_object))]

  # *parse_ctl() adds newline to record* - add newline to expected output
  list_object$result_ctl <- paste0(list_object$result_ctl, "\n")

  return(list_object)
}


#' Format `nmrec_ctl_records`
#'
#' Removes PROBLEM record and returns a formatted string
format_record <- function(input_nmrec){
  # Remove problem statement
  input_nmrec$records <- Filter(
    function(x) !grepl("^\\$PROBLEM", x$format()), input_nmrec$records
  )
  format(input_nmrec)
}


diff_record <- function(test_case) {
  diffobj::diffChr(format_record(test_case$input_nmrec), test_case$result_ctl)
}

# Compile Examples --------------------------------------------------------

# Example Directory
record_example_dir <- system.file(
  file.path("test-refs", "inherit-estimates"), package = "bbr", mustWork = TRUE
)

# Group and set up examples
example_paths <- list.files(record_example_dir)
example_spec <- example_paths %>%
  purrr::map_chr(\(.x) str_extract(.x, "^[^-]+")) %>%
  unique() %>% rlang::set_names() %>%
  purrr::map(\(.x) example_paths[str_detect(example_paths, paste0("^", .x, "-"))]) %>%
  tibble::enframe(name = "record_type", value = "file_path") %>%
  tidyr::unnest(cols = "file_path") %>%
  dplyr::mutate(record_name = fs::path_ext_remove(file_path))


get_example_record <- function(
    .case = NULL,
    .record_type = c("any", "theta", "omega", "sigma"),
    .pull_record = TRUE,
    .example_spec = example_spec
){

  .record_type <- match.arg(.record_type)

  # load examples - this should be done every time `get_example_record` is
  # called to refresh the example. Otherwise changes to the example will persist
  .example_spec <- .example_spec %>%
    dplyr::rowwise() %>%
    dplyr::mutate(list_object = list(read_and_extract_list(file_path))) %>%
    dplyr::ungroup() %>% dplyr::relocate("record_type", "record_name")

  records <- .example_spec

  if(.record_type != "any"){
    records <- records %>% dplyr::filter(record_type == .record_type)
  }

  if(!is.null(.case)){
    records <- records %>% dplyr::filter(grepl(.case, record_name, fixed = TRUE))
  }

  cli::cli_div(theme = list(span.emph = list(color = "red"), span.code = list(color = "blue")))

  if(isTRUE(.pull_record) && nrow(records) == 1){
    rec <- records %>% dplyr::pull("list_object")
    return(rec[[1]])
  }else if(isTRUE(.pull_record) && nrow(records) < 1 && !is.null(.case)){
    msg <- glue::glue("No records matching {.emph '{{.case}}'} for {.code {{.record_type}}} records",
                      .open = "{{", .close = "}}")
    cli::cli_abort(c("x" = msg))
  }else if(isTRUE(.pull_record) && nrow(records) > 1 && !is.null(.case)){
    msg <- glue::glue("Multiple records matching {.emph '{{.case}}'} for {.code {{.record_type}}} records",
                      .open = "{{", .close = "}}")
    cli::cli_warn(c("!" = msg))
    return(records)
  }else{
    return(records)
  }
}

