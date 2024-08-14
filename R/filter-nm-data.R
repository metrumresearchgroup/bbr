#' Helper for reading in and parsing the `$DATA` record.
#'
#' @note This cannot be used for _modifying_ a `$DATA` record.
#' @noRd
#' @keywords internal
read_data_record <- function(.mod){
  data_recs <- get_records(.mod, "data")
  n_data <- length(data_recs)
  if(n_data !=1){
    recs_fmt <- purrr::map_chr(data_recs, function(rec) rec$format())
    rlang::abort(
      c(
        glue::glue("Expected a single data record, but found {n_data}:\n\n"),
        recs_fmt
      )
    )
  }
  data_recs[[1]]$parse()
  return(data_recs[[1]])
}


#' Extract IGNORE or ACCEPT options from a data record
#' @inheritParams filter_nm_data
#' @noRd
get_data_filter_exprs <- function(.mod){

  data_rec <- read_data_record(.mod)

  # Extract & format IGNORE/ACCEPT options
  ignore_opts <- purrr::keep(data_rec$values, function(val){
    inherits(val, "nmrec_option_value") && identical(val[["name"]], "ignore")
  })

  accept_opts <- purrr::keep(data_rec$values, function(val){
    inherits(val, "nmrec_option_value") && identical(val[["name"]], "accept")
  })

  has_ignore <- !rlang::is_empty(ignore_opts)
  has_accept <- !rlang::is_empty(accept_opts)

  if(has_ignore && has_accept){
    # Identical to NMTRAN error message if both are used
    rlang::abort("ACCEPT list and IGNORE list may not both be used")
  }else{
    type <- dplyr::case_when(
      has_ignore ~ "ignore",
      has_accept ~ "accept",
      # NA type will escape filtering
      TRUE ~ NA_character_
    )
  }

  # Pull out filters (remove parentheses and any quoting)
  #  - can concatenate options since one or both are assumed to be empty
  exprs <- purrr::map_chr(c(ignore_opts, accept_opts), function(val){
    gsub("\\(|\\)", "", unquote_filename(val$value))
  })

  # Separate any (list) type expressions
  exprs <- unlist(stringr::str_split(exprs, ",")) %>% stringr::str_trim()

  return(
    list(
      type = type,
      exprs = exprs
    )
  )
}

#' Function to translate `NONMEM` operators to `R` operators
#' @param expr String. A `NONMEM` ignore/accept expression
#' @note `.EQN.` and `.NEN.` are available after `NONMEM 7.3`
#' @return A [dplyr::filter()] expression
#'
#' @examples
#' \dontrun{
#'
#' translate_nm_operator(c("AGE.NE.30", "ID.EQ.2", "WT/=70"))
#' #> [1] "AGE!=30" "ID==2" "WT!=70"
#' }
#' @keywords internal
#' @seealso [invert_operator()], [translate_nm_expr()]
translate_nm_operator <- function(expr) {
  # Check for unsupported operators
  bad_ops <- c(".OR.",".AND", ".NOT.")
  bad_ops_pat <- paste(bad_ops, collapse = "|")
  if(any(grepl(bad_ops_pat, expr))){
    cli::cli_abort(
      c(
        "The following logical operators are not supported {.var {bad_ops}}",
        "i" = "See NONMEM documentation for more details"
      )
    )
  }

  # Equal
  expr <- gsub(".EQ.", "==", expr, fixed = TRUE)
  expr <- gsub(".EQN.", "==", expr, fixed = TRUE)
  # Not equal
  expr <- gsub(".NE.", "!=", expr, fixed = TRUE)
  expr <- gsub(".NEN.", "!=", expr, fixed = TRUE)
  expr <- gsub("/=", "!=", expr, fixed = TRUE)
  # Less than | Less than or equal to
  expr <- gsub(".LT.", "<", expr, fixed = TRUE)
  expr <- gsub(".LE.", "<=", expr, fixed = TRUE)
  # Greater than | Greater than or equal to
  expr <- gsub(".GT.", ">", expr, fixed = TRUE)
  expr <- gsub(".GE.", ">=", expr, fixed = TRUE)

  # Handle single `=` only when it's not part of `==`, `!=`, `<=`, `>=`
  expr <- gsub("(?<=[^=!<>])=(?=[^=!<>])", "==", expr, perl = TRUE)
  return(expr)
}

#' Function to invert `R` operators in filter expressions
#' @param expr A [dplyr::filter()] expression
#' @return the inverted expression
#'
#' @examples
#' \dontrun{
#'
#' invert_operator(c('A==2', 'B >= 4'))
#' #> [1] "A!=2"   "B <= 4"
#' }
#' @keywords internal
#' @seealso [translate_nm_operator()], [translate_nm_expr()]
invert_operator <- function(expr) {
  expr <- dplyr::case_when(
    grepl("==", expr, fixed = TRUE) ~ gsub("==", "!=", expr, fixed = TRUE),
    grepl("!=", expr, fixed = TRUE) ~ gsub("!=", "==", expr, fixed = TRUE),
    grepl("<=", expr, fixed = TRUE) ~ gsub("<=", ">=", expr, fixed = TRUE),
    grepl(">=", expr, fixed = TRUE) ~ gsub(">=", "<=", expr, fixed = TRUE),
    grepl("<", expr, fixed = TRUE) ~ gsub("<", ">", expr, fixed = TRUE),
    grepl(">", expr, fixed = TRUE) ~ gsub(">", "<", expr, fixed = TRUE),
    TRUE ~ expr
  )
  return(expr)
}

#' Translate `NONMEM` `IGNORE` and `ACCEPT` expressions into [dplyr::filter()]
#'  expressions.
#'
#' @param nm_expr A `NONMEM` filter expression. e.g., `'ID.EQ.2, BLQ=1'`.
#' @param type Either `'ignore'` or `'accept'`. Denotes which type of `NONMEM`
#'  filtering the expression corresponds to.
#' @param data_cols Column names associated with the input data. Used for
#'  `'ignore'` expressions.
#'
#' @examples
#' \dontrun{
#'
#' test_exprs <- c("SEX==1", "ID.EQ.2", "WT/=70", "AGE.NE.30", "A=1", "WT.GT.40")
#'
#' translate_nm_expr(test_exprs, type = 'ignore')
#'
#' translate_nm_expr(test_exprs, type = 'accept')
#'
#'
#' # Use of `@`, `#`, or form `IGNORE=C2` require `data_cols` to be specified,
#' # though only the first column is used
#' data_cols <- c("C", "ID", "TIME", "EVID", "DV", "BLQ")
#'
#' translate_nm_expr("#", data_cols = data_cols)
#'
#' translate_nm_expr("c2", data_cols = data_cols)
#'
#' translate_nm_expr("@", data_cols = data_cols)
#'
#' }
#' @keywords internal
#' @seealso [translate_nm_operator()], [invert_operator()]
translate_nm_expr <- function(
    nm_expr,
    type = c("ignore", "accept"),
    data_cols = NULL
){
  type <- match.arg(type)
  checkmate::assert_character(data_cols, min.len = 1, null.ok = TRUE)

  # Translate NM operators
  exprs <- translate_nm_operator(nm_expr)

  r_exprs <- purrr::map_chr(exprs, function(expr){
    if(type == "ignore"){
      # `IGNORE=#`, `IGNORE=@`, `IGNORE=c1`, `IGNORE=(list)`
      if(expr == "#"){
        # IGNORE=# is the default.  That is, in the absence of IGNORE option, any
        # record whose first character is '#' is treated as a comment record.
        paste0("!grepl('^#', ", data_cols[1], ")")
      }else if(expr == "@"){
        # IGNORE=@ signifies that any data record having an alphabetic character
        # or `@` as its first non-blank character in column one should be ignored.
        #  - This permits a table file having header lines to be used as an
        #    NM-TRAN data set.
        #  - add extra `\\` for later parse()
        paste0("!grepl('^\\\\s*[A-Za-z@]', ", data_cols[1], ")")
      }else if(grepl('^[a-zA-Z]$', expr)){
        # This is for `IGNORE=C` columns. Meaning ignore rows if the _first_ column
        # contains 'C' (this form always points to the _first_ column)
        # - the above regex looks for characters of length>=1, and no symbols
        paste0(data_cols[1], "!=", "'", expr, "'")
      }else{
        # Invert list form expressions
        invert_operator(expr)
      }
    }else{
      # ACCEPT option only supports `ACCEPT=(list)` form --> no formatting needed
      expr
    }
  })

  return(r_exprs)
}

#' Filter `NONMEM` input data based on `IGNORE` and `ACCEPT` record options
#'
#' @param .mod A `bbi_nonmem_model` object
#' @param data A starting dataset
#' @keywords internal
filter_nm_data <- function(.mod, data = nm_data(.mod)){

  # Extract & format IGNORE/ACCEPT options into expressions
  nm_exprs <- get_data_filter_exprs(.mod)

  # Return starting data if no IGNORE/ACCEPT options are found
  if(is.na(nm_exprs$type)){
    attr(data, "n_records_dropped") <- 0
    return(data)
  }

  # Translate NONMEM syntax into `dplyr::filter` logic
  r_filters <- translate_nm_expr(
    nm_expr = nm_exprs$exprs, type = nm_exprs$type, data_cols = names(data)
  )

  # Create the final dplyr::filter expression
  filter_expression <- paste(r_filters, collapse = " & ")

  # Apply filters
  filtered_data <- tryCatch({
    data %>% dplyr::filter(eval(parse(text = filter_expression)))
  }, error = function(cond){
    cli::cli_abort(
      c(
        "ignore/accept list could not be converted to filters",
        "i" = "The following errors occurred:",
        "x" = cond$parent$message
      )
    )
  })

  perc_retained <- round(100*(nrow(filtered_data)/nrow(data)), 2)
  attr(filtered_data, "perc_retained") <- perc_retained
  attr(filtered_data, "n_records_dropped") <- nrow(data) - nrow(filtered_data)

  return(filtered_data)
}
