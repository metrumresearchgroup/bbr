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
NULL


#' @describeIn modify_records Safely read in a `NONMEM` control stream file via
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
#'
#' @note
#' Run the following command To see what record types are available/supported by
#' `nmrec`:
#'  ```
#'  `ls(nmrec:::record_names)`
#'  ```
#'
#' @keywords internal
remove_records <- function(.mod, type){
  if(mod_has_record(.mod, type)){
    ctl <- get_model_ctl(.mod)
    type_name <- get_records(.mod, type)[[1]]$name
    rec_indices <- seq_along(ctl$records)
    indices_remove <- purrr::keep(rec_indices, function(index){
      ctl$records[[index]]$name == type_name
    })

    # Remove records
    if(length(indices_remove) >= 1){
      ctl$records[indices_remove] <- NULL
    }

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
#'
#' @param data_path Data path to set in a `$DATA` record.
#'
#' @keywords internal
modify_data_path_ctl <- function(.mod, data_path){
  ctl <- get_model_ctl(.mod)

  # Get data record
  data_recs <- nmrec::select_records(ctl, "data")
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

  # Overwrite 'filename' option
  filename_opt <- nmrec::get_record_option(data_recs[[1]], "filename")
  if (is.null(filename_opt)) {
    dev_error(glue("No filename option found in `$DATA` record: \n{data_recs[[1]]$format()}"))
  }
  filename_opt$value <- data_path

  # Write out modified ctl
  mod_path <- get_model_path(.mod)
  nmrec::write_ctl(ctl, mod_path)
  return(invisible(TRUE))
}


#' @describeIn modify_records Modify or retrieve the `$PROBLEM` statement from
#' a `NONMEM` control stream file.
#'
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

#' Helper for checking if a specified record type is valid.
#'
#' @inheritParams remove_records
#' @details
#' This function is basically meant to ensure that `type` is an available
#' record name within `ls(nmrec:::record_names)`. This function should search
#' there (and no longer require a model object) if those names become exported.
#' @noRd
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

