#' Modify or retrieve options and records from a `NONMEM` control stream file
#' @param .mod a bbr model object
#' @name modify_records
#'
#' @details
#'  - **`get_model_ctl()`** is called internally within the other functions, though
#'  it can also be used outside of that context.
#'  - **`modify_prob_statement()`**, **`modify_data_path_ctl()`**,
#'    **`remove_records()`**, and **`add_new_record()`** read in the control stream,
#'    make any modifications, and then save out the updated control stream.
#'      - `modify_prob_statement()` also returns a character string defining the
#'        `$PROBLEM` text (see `prob_text` argument).
#' - **`mod_has_record()`** will return a logical value denoting whether a `bbr`
#'   model has a given record type.
#' - **`get_records()`** extracts all records of a given type. Note that it is
#'   not meant to be used to modify existing records.
#' - **get_input_columns()** and **get_table_columns()** extract the column names
#'   of the input data and all table files respectively. They can either be parsed
#'   from a `NONMEM` control stream file, or determined from reading in the
#'   `csvs`/table files. See `from_data` argument for more details.
#'
#' @seealso [assert_record_type()]
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' # Adding a new record
#' seed <- 1234; n <- 100
#' sim_lines <- glue("({seed}) SUBPROBLEMS={n} TRUE=FINAL ONLYSIMULATION")
#' add_new_record(.mod, "simulation", lines = sim_lines, after = "pred")
#'
#'
#' # Remove all records of a given type
#' if(mod_has_record(.mod, "table")) remove_records(.mod, "table")
#'
#'
#' # Modify or extract a problem statement
#' modify_prob_statement(.mod, prob_text = NULL)
#' #> [1] "PK model 1 cmt base"
#' modify_prob_statement(.mod, prob_text = "new problem")
#' #> [1] "new problem"
#'
#' }
#' @note
#' Run the following command to see what record types are available/supported by
#' `nmrec`:
#'  ```
#'  `ls(nmrec:::record_names)`
#'  ```
NULL


#' @describeIn modify_records Read in a `NONMEM` control stream file, parsed via
#' `nmrec`
#' @keywords internal
get_model_ctl <- function(.mod){
  check_model_object(.mod, c(NM_MOD_CLASS, NMBOOT_MOD_CLASS, NMSIM_MOD_CLASS))
  mod_path <- get_model_path(.mod)
  ctl <- nmrec::read_ctl(mod_path)
  return(ctl)
}


#' @describeIn modify_records Pull all records of a given record type from a
#' `bbr` model
#' @param get_lines Logical (T/F). If `TRUE`, return the lines as character
#'  vectors (based on how they were defined in the control stream).
#' @keywords internal
get_records <- function(.mod, type, get_lines = FALSE){
  assert_record_type(.mod, type)
  ctl <- get_model_ctl(.mod)
  recs <- nmrec::select_records(ctl, type)
  if(rlang::is_empty(recs)){
    return(NULL)
  }else{
    if(isTRUE(get_lines)){
      recs <- purrr::map(recs, function(rec){
        rec$get_lines()
      })
    }
    return(recs)
  }
}


#' @describeIn modify_records Check if a `bbr` model has a given record type
#' @keywords internal
mod_has_record <- function(.mod, type){
  assert_record_type(.mod, type)
  ctl <- get_model_ctl(.mod)
  recs <- nmrec::select_records(ctl, type)
  has_rec <- !rlang::is_empty(recs)

  return(has_rec)
}


#' @describeIn modify_records Remove _all records_ of a given type from a `NONMEM`
#'  control stream file
#' @param type record type. This may be spelled any way that's accepted in a
#'  `NONMEM` control stream.
#' @keywords internal
remove_records <- function(.mod, type){
  if(mod_has_record(.mod, type)){
    ctl <- get_model_ctl(.mod)

    # Get appropriate names and records to remove (if any)
    type_name <- get_records(.mod, type)[[1]]$name
    indices_remove <- purrr::map_lgl(ctl$records, function(rec){
      identical(rec[["name"]], type_name)
    })

    # Remove records
    ctl$records[indices_remove] <- NULL

    # Write out modified ctl
    mod_path <- get_model_path(.mod)
    nmrec::write_ctl(ctl, mod_path)
  }else{
    verbose_msg(glue("record type {type} not found"))
  }

  return(invisible(TRUE))
}


#' @describeIn modify_records Add a new record of a given type from a `NONMEM`
#'  control stream file
#' @param rec_name a character string defining the record name (e.g., `$THETA`)
#' @param lines a character string or vector of lines to append to the new
#'   record. If passing a vector, creates a new line per index. Defaults to `NULL`.
#' @param after add new record after _this record type_. This may be spelled
#'   any way that's accepted in a `NONMEM` control stream. If `NULL`, append to
#'   the end of the control stream. If multiple records are found, uses the last
#'   index
#' @keywords internal
add_new_record <- function(
    .mod,
    type,
    rec_name = paste0("$", toupper(type), " "),
    lines = NULL,
    after = NULL
){
  checkmate::assert_character(lines, null.ok = TRUE)
  checkmate::assert_character(rec_name, null.ok = TRUE)
  assert_record_type(.mod, type)

  if(!is.null(rec_name)){
    rec_name <- paste0(stringr::str_trim(rec_name), " ")
  }

  ctl <- get_model_ctl(.mod)

  # Define new record
  if(!is.null(lines)){
    lines_txt <- paste0(lines, collapse = "\n")
    # Starting on same line as record by default
    new_rec <- paste0(rec_name, lines_txt, "\n\n")
  }else{
    new_rec <- paste0(rec_name, "\n\n")
  }

  # Determine where to put the new record
  if(is.null(after)){
    # If NULL, add record to the end
    num_recs <- length(ctl$records)
    ctl$records <- append(ctl$records, new_rec, after = num_recs)
  }else{
    assert_record_type(.mod, after)
    # This is the name encoded in the R6 object. This is needed for matching types
    # - use the last record
    after_name <- get_records(.mod, after)[[1]]$name

    rec_indices <- seq_along(ctl$records)
    indices_after <- purrr::keep(rec_indices, function(index){
      ctl$records[[index]]$name == after_name
    })

    # Add to the last discovered record of a given type
    ctl$records <- append(ctl$records, new_rec, after = max(indices_after))
  }

  # Write out modified ctl
  mod_path <- get_model_path(.mod)
  nmrec::write_ctl(ctl, mod_path)
  return(invisible(TRUE))
}


#' @describeIn modify_records Modify the specified data path in a `NONMEM`
#' control stream file
#' @param data_path Data path to set in a `$DATA` record.
#' @keywords internal
modify_data_path_ctl <- function(.mod, data_path){
  ctl <- get_model_ctl(.mod)

  # Get data record
  data_rec <- read_data_record(.mod)

  # Overwrite 'filename' option
  filename_opt <- nmrec::get_record_option(data_rec, "filename")
  if (is.null(filename_opt)) {
    dev_error(glue("No filename option found in `$DATA` record: \n{data_rec$format()}"))
  }
  filename_opt$value <- data_path

  # Write out modified ctl
  mod_path <- get_model_path(.mod)
  nmrec::write_ctl(ctl, mod_path)
  return(invisible(TRUE))
}


#' @describeIn modify_records Modify or retrieve the `$PROBLEM` statement from
#' a `NONMEM` control stream file.
#' @param prob_text If `NULL` return the current `$PROBLEM` statement. If a
#'  character string, set the problem statement to that value.
#' @keywords internal
modify_prob_statement <- function(.mod, prob_text = NULL){
  checkmate::assert_character(prob_text, len = 1, null.ok = TRUE)
  mod_path <- get_model_path(.mod)
  ctl <- get_model_ctl(.mod)

  prob_recs <- nmrec::select_records(ctl, "prob")
  if (length(prob_recs) != 1) {
    dev_error("select_records should always return a single $PROBLEM record")
  }

  prob_rec <- prob_recs[[1]]
  prob_str <- nmrec::get_record_option(prob_rec, "text")
  if(is.null(prob_str)){
    # NONMEM permits a bare "$PROB{newline}", in which case there won't be a text option.
    prob_rec$parse()
    idx <- purrr::detect_index(
      prob_rec$values,
      function(x) inherits(x, "nmrec_option_record_name")
    )
    if(idx == 0){
      dev_error("Unsupported $PROBLEM record format")
    }
    if(!is.null(prob_text)){
      prob_rec$values <- append(prob_rec$values, paste0(" ", prob_text), idx)
      prob_str_return <- prob_text
    }else{
      prob_str_return <- prob_rec$values[[idx]]$format()
    }
  }else{
    if(!is.null(prob_text)){
      prob_str$value <- prob_text
    }
    prob_str_return <- prob_str$value
  }

  # Write control stream and return problem statement
  if(!is.null(prob_text)){
    nmrec::write_ctl(ctl, mod_path)
  }
  return(prob_str_return)
}

#' @describeIn modify_records Retrieve input data column names from either the
#'  `$INPUT` record in a `NONMEM` control stream file or the input dataset.
#' @param from_data Logical (T/F). If `TRUE`, the default, get the column names
#'  from the first line of the referenced dataset (input data or table file). If
#'  `FALSE`, parse the control stream and retrieve from the relevant record type
#'  (`$INPUT` or `$TABLE`).
#' @keywords internal
get_input_columns <- function(.mod, from_data = TRUE){
  if(isTRUE(from_data)){
    data_path <- get_data_path(.mod)
    input_data <- fread(data_path, na.strings = ".", verbose = FALSE, nrows = 1)
    input_cols <- toupper(names(input_data))
  }else{
    inputs <- get_records(.mod, "input")[[1]]
    inputs$parse()

    input_col_opts <- purrr::keep(inputs$values, function(val){
      inherits(val, c("nmrec_option_flag", "nmrec_option_value")) &&
        !inherits(val, "nmrec_option_record_name")
    })

    input_col_names <- purrr::map_chr(input_col_opts, function(val) val$name)
    input_cols <- purrr::map_chr(input_col_opts, function(val){
      ifelse(inherits(val, "nmrec_option_flag"), val$name, as.character(val$value))
    }) %>% stats::setNames(input_col_names)
  }
  return(input_cols)
}

#' @describeIn modify_records Retrieve table columns names from either `$TABLE`
#'  record(s) in a `NONMEM` control stream file or the tabled out files
#' @keywords internal
get_table_columns <- function(.mod, from_data = TRUE){
  if(isTRUE(from_data)){
    tab_files <- nm_table_files(.mod)
    table_cols <- purrr::map(tab_files, function(tab_file){
      tab <- fread(tab_file, na.strings = ".", verbose = FALSE, nrows = 1, skip = 1)
      table_cols <- toupper(names(tab))
    })
  }else{
    tables <- get_records(.mod, "table")

    table_cols <- purrr::map(tables, function(table){
      table$parse()
      table_col_opts <- purrr::keep(table$values, function(val){
        inherits(val, "nmrec_option_pos")
      })
      table_cols <- purrr::map(table_col_opts, function(col){
        str_split(col$format(), " ")[[1]]
      }) %>% purrr::list_c()
    })
  }
  return(table_cols)
}


#' Helper for reading in and parsing the `$DATA` record.
#' @noRd
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



#' Filter `NONMEM` input data based on `IGNORE` and `ACCEPT` record options
#'
#' @param .mod a `bbi_nonmem_model` object
#' @param data a starting dataset
#' @keywords internal
nm_data_filter <- function(.mod, data = nm_data(.mod)){

  # Function to translate NONMEM operators to R operators
  translate_nm_operator <- function(expr) {
    expr <- gsub("\\.EQ\\.|=", "==", expr)
    expr <- gsub("\\.NE\\.", "!=", expr)
    expr <- gsub("\\.LT\\.", "<", expr)
    expr <- gsub("\\.LE\\.", "<=", expr)
    expr <- gsub("\\.GT\\.", ">", expr)
    expr <- gsub("\\.GE\\.", ">=", expr)
    return(expr)
  }

  # Function to invert an expression (for ignore statements)
  invert_expression <- function(expr) {
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

  data_rec <- read_data_record(.mod)

  # Extract & format IGNORE options
  ignore_opts <- purrr::keep(data_rec$values, function(val){
    inherits(val, "nmrec_option_value") && identical(val[["name"]], "ignore")
  })
  ignore_vals <- purrr::map_chr(ignore_opts, function(ign_val){
    gsub("\\(|\\)", "", unquote_filename(ign_val$value))
  })

  # Extract & format ACCEPT options
  accept_opts <- purrr::keep(data_rec$values, function(val){
    inherits(val, "nmrec_option_value") && identical(val[["name"]], "accept")
  })
  accept_vals <- purrr::map_chr(accept_opts, function(acc_val){
    gsub("\\(|\\)", "", unquote_filename(acc_val$value))
  })


  # Translate the ignore and accept expressions
  ignore_exprs <- lapply(ignore_vals, translate_nm_operator) %>% gsub(",", " &", .)
  accept_exprs <- lapply(accept_vals, translate_nm_operator) %>% gsub(",", " &", .)

  ## Combine the expressions into filter expressions ##
  data_cols <- names(data)

  # `IGNORE=#`, `IGNORE=@`, `IGNORE=c1`, `IGNORE=(list)`
  ignore_filters <- purrr::map_chr(ignore_exprs, function(expr) {
    if(expr == "#"){
      # IGNORE=# is the default.  That is, in the absence of IGNORE option, any
      # record whose first character is '#' is treated as a comment record.
      col_filters <- purrr::map_chr(data_cols, function(col) {
        paste0("!grepl('^#', ", col, ")")
      })
      return(paste(col_filters, collapse = " & "))
    }else if(expr == "@"){
      # IGNORE=@ signifies that any data record having an alphabetic character
      # or `@` as its first non-blank character (not just in column 1)
      # should be ignored. This permits a table file having header lines to be
      # used as an NM-TRAN data set.
      col_filters <- purrr::map_chr(data_cols, function(col) {
        paste0("!grepl('^[A-Za-z@]', ", col, ")")
      })
      return(paste(col_filters, collapse = " & "))
    }else if(grepl('^[a-zA-Z0-9]{1,}$', expr)){
      # This is for `IGNORE=C` columns. Meaning ignore rows if the _first_ column
      # contains 'C' (this form always points to the _first_ column)
      # - the above regex looks for characters of length>=1, and no symbols
      paste0(data_cols[1], "!=", "'", expr, "'")
    }else{
      # Invert list form expressions
      return(invert_expression(expr))
    }
  })

  # ACCEPT option only supports `ACCEPT=(list)` form --> no formatting needed

  # Combine all filter expressions
  all_filters <- c(ignore_filters, accept_exprs)

  # Create the final dplyr::filter expression
  filter_expression <- paste(all_filters, collapse = " & ")

  # Apply filters
  filtered_data <- tryCatch({
    data %>% dplyr::filter(eval(parse(text = filter_expression)))
  }, error = function(cond){
    rlang::abort(
      c(
        "ignore and/or accept statements could not be converted to filters",
        "The following errors occurred:",
        cond$parent$message
      )
    )
  })

  attr(filtered_data, "n_records_dropped") <- nrow(data) - nrow(filtered_data)
  return(filtered_data)
}


#' Apply `null` mapping
#' @inheritParams nm_data_filter
#' @keywords internal
nm_data_null_map <- function(.mod, data = nm_data(.mod)){
  # Get data record
  data_rec <- read_data_record(.mod)

  # Extract & format NULL options (not used for filtering)
  null_opts <- purrr::keep(data_rec$values, function(val){
    inherits(val, "nmrec_option_value") && identical(val[["name"]], "null")
  })
  null_val <- purrr::map_chr(null_opts, function(ign_val){
    unquote_filename(ign_val$value)
  })

  # Replace null values with `null_val`
  null_valid <- !rlang::is_empty(null_val) && length(null_val) == 1
  if (isTRUE(null_valid)) {
    # Null data items consist of a single dot (.), consecutive commas or
    # consecutive tab characters
    is_nm_null <- function(val){
      val == "." | grepl("^[,]{2,}$", val) | grepl("^[ \t]{2,}$", val)
    }
    data <- data %>% dplyr::mutate(
      across(everything(), ~ ifelse(is_nm_null(.x), null_val, .x))
    )
    attr(data, "null_value") <- null_val
  }
  return(data)
}

#' Drop or skip records based on `$INPUT` record options
#' @inheritParams nm_data_filter
#' @keywords internal
nm_data_drop_skip_records <- function(.mod, data = nm_data(.mod)){
  # Get INPUT record
  input_records <- get_records(.mod, "input")
  if (length(input_records) != 1) {
    rlang::abort("Expected a single $INPUT record.")
  }

  input_rec <- input_records[[1]]
  input_rec$parse()

  # Extract columns that should be dropped
  drop_cols <- purrr::map_chr(input_rec$values, function(val){
    if(inherits(val, c("nmrec_option_flag", "nmrec_option_value")) &&
       (identical(val[["name"]], "DROP") | identical(val[["name"]], "SKIP"))){
      ifelse(isTRUE(val$value), val[["name"]], val[["value"]])
    }else{
      return(NA)
    }
  })

  cols_to_remove <- drop_cols[!is.na(drop_cols)]

  if (length(cols_to_remove) > 0) {
    data <- data %>% dplyr::select(-all_of(cols_to_remove))
    attr(data, "dropped_cols") <- cols_to_remove
  }

  return(data)
}


#' Format input data as an `NM-TRAN` dataset
#' @inheritParams nm_data_filter
#' @keywords internal
nm_data_as_nmtran <- function(.mod, data = nm_data(.mod)){

  # Filter the data based on IGNORE and ACCEPT statements
  filtered_data <- nm_data_filter(.mod, data = data)

  # Map null values
  null_mapped_data <- nm_data_null_map(.mod, data = filtered_data)

  # DROP/SKIP records
  final_data <- nm_data_drop_skip_records(.mod, data = null_mapped_data)

  # Rename columns if data item labels were used
  # TODO: confirm if renaming should happen before/after IGNORE options
  col_names_ctl <- get_input_columns(.mod, from_data = FALSE)

  # Filter out column names that were dropped
  dropped_cols <- attributes(final_data)$dropped_cols
  if(!is.null(dropped_cols)){
    cols_rename <- col_names_ctl[!(col_names_ctl %in% dropped_cols)]
  }

  if(!all(cols_rename %in% names(final_data))){
    rlang::abort(
      c(
        "At least one column specified in the control stream was not found in the data",
        paste("Diff: ", setdiff(cols_rename, names(final_data)))
      )
    )
  }

  final_data <- final_data %>% setNames(names(cols_rename))

  return(final_data)
}

#' Helper for checking if a specified record type is valid.
#'
#' Checks if the specified record type is valid. Note that this does _not_ check
#' if the record is present in the control stream, just whether `nmrec` recognizes
#' the spelling.
#'
#' @inheritParams remove_records
#' @details
#' This function is basically meant to ensure that `type` is an available
#' record name within `ls(nmrec:::record_names)`. This function should search
#' there (and no longer require a model object) if those names become exported.
#'
#' Run the following command to see what record types are available/supported by
#' `nmrec`:
#'  ```
#'  `ls(nmrec:::record_names)`
#'  ```
#' @keywords internal
assert_record_type <- function(.mod, type){
  checkmate::assert_character(type)
  ctl <- get_model_ctl(.mod)
  recs <- tryCatch(
    nmrec::select_records(ctl, type),
    error = function(cond) return(cond)
  )

  if(inherits(recs, "error")){
    rlang::abort(
      c(
        "Issue when looking for record type. Failed with the following message:",
        recs$message
      )
    )
  }

  return(invisible(TRUE))
}

