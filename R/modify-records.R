
#' Modify or retrieve the `$PROBLEM` statement from a `NONMEM` control stream
#'  file.
#'
#' @param mod a bbr model object
#' @param prob_text If `NULL` return the current `$PROB` statement. If a character
#'  string, set the problem statement to that value.
#'
#' @keywords internal
modify_prob_statement <- function(mod, prob_text = NULL){
  mod_path <- get_model_path(mod)
  ctl <- tryCatch(nmrec::read_ctl(mod_path), error = identity)
  if(inherits(ctl, "error")){
    warning(
      glue::glue("Could not read control stream file. Reason: {ctl$message}")
    )
  }

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




#' Modify the specified data path in a control stream file
#'
#' @param mod a bbr model object
#' @param data_path Data path to set in a `$DATA` record.
#'
#' @keywords internal
modify_data_path_ctl <- function(mod, data_path){
  mod_path <- get_model_path(mod)
  ctl <- tryCatch(nmrec::read_ctl(mod_path), error = identity)
  if(inherits(ctl, "error")){
    warning(
      glue::glue("Could not read control stream file. Reason: {ctl$message}")
    )
  }

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
  nmrec::write_ctl(ctl, mod_path)
}
