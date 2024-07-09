#' Helper for reading in and parsing the `$DATA` record.
#'
#' Note that this cannot be used for _modifying_ a record.
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


#' Function to translate NONMEM operators to R operators
#' @noRd
#' @keywords internal
translate_nm_operator <- function(expr) {
  expr <- gsub("\\.EQ\\.", "==", expr)
  expr <- gsub("\\.NE\\.|/=", "!=", expr)
  expr <- gsub("\\.LT\\.", "<", expr)
  expr <- gsub("\\.LE\\.", "<=", expr)
  expr <- gsub("\\.GT\\.", ">", expr)
  expr <- gsub("\\.GE\\.", ">=", expr)

  # Handle single `=` only when it's not part of `==`, `!=`, `<=`, `>=`
  expr <- gsub("(?<=[^=!<>])=(?=[^=!<>])", "==", expr, perl = TRUE)
  return(expr)
}

#' Function to invert ignore expressions
#' @noRd
#' @keywords internal
invert_operator <- function(expr) {
  expr <- dplyr::case_when(
    grepl("==", expr) ~ gsub("==", "!=", expr),
    grepl("!=", expr) ~ gsub("!=", "==", expr),
    grepl("<=", expr) ~ gsub("<=", ">=", expr),
    grepl(">=", expr) ~ gsub(">=", "<=", expr),
    grepl("<", expr) ~ gsub("<", ">", expr),
    grepl(">", expr) ~ gsub(">", "<", expr),
    TRUE ~ expr
  )
  return(expr)
}

#' Translate `IGNORE` and `ACCEPT` filter expressions into [dplyr::filter()]
#'  expressions.
#' @param nm_expr a `NONMEM` filter expression. e.g., `'ID.EQ.2, BLQ=1'`.
#' @param type Either `'ignore'` or `'accept'`. Denotes which type of `NONMEM`
#'  filtering the expression corresponds to.
#' @param data_cols column names associated with the input data. Used for
#'  `'ignore'` expressions.
#'
#' @examples
#' \dontrun{
#' test_exprs <- "SEX==1, ID.EQ.2, WT/=70, AGE.NE.30, A=1, WT.GT.40"
#'
#' translate_nm_expr(test_exprs, type = 'ignore')
#' #> [1] "SEX!=1 & ID!=2 & WT==70 & AGE==30 & A!=1 & WT<40"
#'
#' translate_nm_expr(test_exprs, type = 'accept')
#' #> [1] "SEX==1 & ID==2 & WT!=70 & AGE!=30 & A==1 & WT>40"
#'
#'
#' # Use of `@`, `#`, or form `IGNORE=C2` require `data_cols` to be specified
#' data_cols <- c("C", "ID", "TIME", "EVID", "DV", "BLQ")
#'
#' translate_nm_expr("#", data_cols = data_cols)
#' #> [1] "!grepl('^#', C)"
#'
#' translate_nm_expr("c2", data_cols = data_cols)
#' #> [1] "C!='c2'"
#'
#' translate_nm_expr("@", data_cols = data_cols)
#' #> [1] "!grepl('^[A-Za-z@]', C) & !grepl('^[A-Za-z@]', ID) &
#' #> !grepl('^[A-Za-z@]', TIME) & !grepl('^[A-Za-z@]', EVID) &
#' #> !grepl('^[A-Za-z@]', DV) & !grepl('^[A-Za-z@]', BLQ)"
#' }
#' @keywords internal
translate_nm_expr <- function(
    nm_expr,
    type = c("ignore", "accept"),
    data_cols = NULL
){
  type <- match.arg(type)
  checkmate::assert_character(data_cols, min.len = 1, null.ok = TRUE)

  # Translate NM operators and separate any (list) type expressions
  exprs <- translate_nm_operator(nm_expr)
  exprs <- stringr::str_split(exprs, ",")[[1]] %>% stringr::str_trim()

  r_exprs <- purrr::map_chr(exprs, function(expr){
    if(type == "ignore"){
      # `IGNORE=#`, `IGNORE=@`, `IGNORE=c1`, `IGNORE=(list)`
      if(expr == "#"){
        # IGNORE=# is the default.  That is, in the absence of IGNORE option, any
        # record whose first character is '#' is treated as a comment record.
        paste0("!grepl('^#', ", data_cols[1], ")")
      }else if(expr == "@"){
        # IGNORE=@ signifies that any data record having an alphabetic character
        # or `@` as its first non-blank character (not just in column 1)
        # should be ignored. This permits a table file having header lines to be
        # used as an NM-TRAN data set.
        col_filters <- purrr::map_chr(data_cols, function(col) {
          paste0("!grepl('^[A-Za-z@]', ", col, ")")
        })
        paste(col_filters, collapse = " & ")
      }else if(grepl('^[a-zA-Z0-9]{1,}$', expr)){
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
  r_expr <- paste(r_exprs, collapse = " & ")
  return(r_expr)
}

#' Filter `NONMEM` input data based on `IGNORE` and `ACCEPT` record options
#'
#' @param .mod a `bbi_nonmem_model` object
#' @param data a starting dataset
#' @keywords internal
filter_nm_data <- function(.mod, data = nm_data(.mod)){

  data_rec <- read_data_record(.mod)

  # Extract & format IGNORE options
  ignore_opts <- purrr::keep(data_rec$values, function(val){
    inherits(val, "nmrec_option_value") && identical(val[["name"]], "ignore")
  })
  ignore_exprs <- purrr::map_chr(ignore_opts, function(ign_val){
    gsub("\\(|\\)", "", unquote_filename(ign_val$value))
  })

  # Extract & format ACCEPT options
  accept_opts <- purrr::keep(data_rec$values, function(val){
    inherits(val, "nmrec_option_value") && identical(val[["name"]], "accept")
  })
  accept_exprs <- purrr::map_chr(accept_opts, function(acc_val){
    gsub("\\(|\\)", "", unquote_filename(acc_val$value))
  })

  # Translate the ignore and accept expressions
  ignore_filters <- purrr::map_chr(ignore_exprs, translate_nm_expr, data_cols = names(data))
  accept_filters <- purrr::map_chr(accept_exprs, translate_nm_expr, type = "accept")

  # Combine all filter expressions
  all_filters <- c(ignore_filters, accept_filters)

  # Create the final dplyr::filter expression
  filter_expression <- paste(all_filters, collapse = " & ")

  # Apply filters
  filtered_data <- tryCatch({
    data %>% dplyr::filter(eval(parse(text = filter_expression)))
  }, error = function(cond){
    rlang::inform(
      c(
        "ignore and/or accept statements could not be converted to filters",
        "The following errors occurred:",
        cond$parent$message
      )
    )
    return(NULL)
  })

  if(!is.null(filtered_data)){
    attr(filtered_data, "n_records_dropped") <- nrow(data) - nrow(filtered_data)
  }

  return(filtered_data)
}
