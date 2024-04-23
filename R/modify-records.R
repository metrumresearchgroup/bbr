#' Modify options and records from a `NONMEM` control stream file
#' @param mod a bbr model object
#' @name modify_records
#'
#' @details
#'  - `safe_read_ctl()` is called internally within the other functions, though
#'  it can also be used outside of that context.
#'  - The other functions will read in the control stream, make any modifications,
#'  and then save out the updated control stream. `modify_prob_statement()` is
#'  the only function that can _also_ optionally return a record option (see
#'  `prob_text` argument).
NULL

#' @describeIn modify_records Safely read in a `NONMEM` control stream file via
#' `nmrec`
#' @keywords internal
safe_read_ctl <- function(mod){
  mod_path <- get_model_path(mod)
  ctl <- tryCatch(nmrec::read_ctl(mod_path), error = identity)
  if(inherits(ctl, "error")){
    warning(
      glue::glue("Could not read control stream file. Reason: {ctl$message}")
    )
  }
  return(ctl)
}


#' @describeIn modify_records Modify or retrieve the `$PROBLEM` statement from
#' a `NONMEM` control stream file.
#'
#' @param prob_text If `NULL` return the current `$PROB` statement. If a
#'  character string, set the problem statement to that value.
#'
#' @keywords internal
modify_prob_statement <- function(mod, prob_text = NULL){
  mod_path <- get_model_path(mod)
  ctl <- safe_read_ctl(mod)

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
      prob_str_return <- prob_rec$values[[idx + 1]]$format()
    }else{
      prob_str_return <- prob_rec$values[[idx]]$format()
    }
  }else{
    if(!is.null(prob_text)){
      prob_str$value <- prob_text
    }
    prob_str_return <- prob_str$value
  }

  # write control stream and return problem statement
  nmrec::write_ctl(ctl, mod_path)
  return(prob_str_return)
}


#' @describeIn modify_records Modify the specified data path in a `NONMEM`
#' control stream file
#'
#' @param data_path Data path to set in a `$DATA` record.
#'
#' @keywords internal
modify_data_path_ctl <- function(mod, data_path){
  ctl <- safe_read_ctl(mod)

  # Get data record
  data_rec <- nmrec::select_records(ctl, "data")[[1]]
  data_rec$parse()

  # Overwrite 'filename' option
  data_rec$values <- purrr::map(data_rec$values, function(data_opt){
    if(inherits(data_opt, "nmrec_option_pos") && data_opt$name == "filename"){
      data_opt$value <- data_path
    }
    data_opt
  })

  # Write out modified ctl
  mod_path <- get_model_path(mod)
  nmrec::write_ctl(ctl, mod_path)
}



#' @describeIn modify_records Remove _all records_ of a given type from a `NONMEM`
#' control stream file
#' @param type type of record to remove. Only `'covariance'` and `'table'` are
#' currently supported
#'
#' @note
#' To add support for more record types to `remove_records()`, run the following
#' command to see the record types of a given model supported by `nmrec`:
#'  ```
#'  purrr::map_chr(ctl$records, "name")
#'  ```
#'
#' @keywords internal
remove_records <- function(mod, type = c("covariance", "table")){
  type <- match.arg(type)
  ctl <- safe_read_ctl(mod)

  rec_indices <- seq_along(ctl$records)
  indices_remove <- purrr::keep(rec_indices, function(index){
    ctl$records[[index]]$name == type
  })

  # Remove records
  if(length(indices_remove) >= 1){
    ctl$records[indices_remove] <- NULL
  }

  # Write out modified ctl
  mod_path <- get_model_path(mod)
  nmrec::write_ctl(ctl, mod_path)
}

