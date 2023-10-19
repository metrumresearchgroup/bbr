
#' Supported estimates to inherit from parent model
BBR_ESTIMATES_INHERIT <- c("theta", "sigma", "omega")


#' Inherit parameter estimates
#'
#' @param .mod new model object to overwrite.
#' @param .mod_inherit_path model path to inherit properties from.
#' @param .inherit type of estimates to inherit from parent model.
#'
#' @export
inherit_param_estimates <- function(
    .mod,
    .mod_inherit_path = get_based_on(.mod),
    .inherit = c("theta", "sigma", "omega"),
    .bounds_opts = c("maintain_bounds", "single_value"),
    .digits = 3
){

  .bounds_opts <- match.arg(.bounds_opts)

  checkmate::assert_true(all(.inherit %in% BBR_ESTIMATES_INHERIT))

  # Confirm .mod_inherit_path is valid
  if(is.null(.mod_inherit_path) || !fs::file_exists(.mod_inherit_path)){
    msg_prefix <- if(is.null(.mod_inherit_path)){
      "`get_based_on(.mod)` returned `NULL`. Please specify `.mod_inherit_path` directly."
    }else{
      glue::glue("Parent model does not exist at: {.mod_inherit_path}")
    }
    msg <- glue::glue("{msg_prefix}
      To inherit parameter estimates from a parent model, this must be a valid file path.")
    stop(msg)
  }

  # Inherit model objects
  .mod_path <- .mod$absolute_model_path

  # Parent model objects
  based_on_mod <- read_model(.mod_inherit_path)
  based_on_sum <- model_summary(based_on_mod)
  inherit_mod_lines <- nmrec::read_ctl(ctl_ext(.mod_inherit_path))

  # TODO: new_thetas, new_omegas, and new_sigmas will have to be formatted
  # if more than one record is being replaced (list)
  # `setup_param_records` will ensure the replacement is of the required length


  # Update THETA Block
  if("theta" %in% .inherit){
    new_thetas <- based_on_sum %>% get_theta() %>% signif(digits = .digits) %>% unname()
    copy_thetas(inherit_mod_lines, .new_thetas = new_thetas, .bounds_opts = .bounds_opts)
  }

  # Update OMEGA Block
  if("omega" %in% .inherit){
    new_omegas <- based_on_sum %>% get_omega() %>% signif(digits = .digits)
    copy_omegas(inherit_mod_lines, .new_omegas = new_omegas, .bounds_opts = .bounds_opts)
  }

  # Update SIGMA Block
  if("sigma" %in% .inherit){
    new_sigmas <- based_on_sum %>% get_sigma() %>% signif(digits = .digits)
    copy_sigmas(inherit_mod_lines, .new_sigmas = new_sigmas, .bounds_opts = .bounds_opts)
  }

  # Write out updated model
  nmrec::write_ctl(inherit_mod_lines, ctl_ext(.mod_path))

  return(invisible(.mod_inherit_path))
}


#' Copy theta records from one model to another
#'
#' @param .mod_lines lines of ctl file. Must be an `nmrec` `nmrec_ctl_records` object.
#' @param .new_thetas list of new theta vectors, or a single theta vector
#'
#' @keywords internal
copy_thetas <- function(.mod_lines, .new_thetas, .bounds_opts){

  # Pull records and format replacement values
  param_setup <- setup_param_records(
    .mod_lines, .new_params = .new_thetas, .rec_type = "theta"
  )
  theta_recs <- param_setup$param_recs
  new_thetas <- param_setup$new_params

  # Base case (copy over values, no unique blocks)
  purrr::walk2(theta_recs, new_thetas, function(theta_rec, new_thetas_i){
    theta_rec$parse()

    # inspect each record - filter to value options
    val_recs <- extract_record_values(theta_rec)

    # Ensure replacement lengths are the same
    check_record_replacements(val_recs, new_thetas_i)
    copy_record_opt(val_recs, new_thetas_i, .bounds_opts)
  })
}


#' Copy sigma records from one model to another
#'
#' @inheritParams copy_thetas
#' @param .new_sigmas list of new sigma matrices, or a single sigma matrix
#'
#' @keywords internal
copy_sigmas <- function(.mod_lines, .new_sigmas, .bounds_opts){

  # Pull records and format replacement values
  param_setup <- setup_param_records(
    .mod_lines, .new_params = .new_sigmas, .rec_type = "sigma"
  )
  sigma_recs <- param_setup$param_recs
  new_sigmas <- param_setup$new_params

  # Base case (copy over values, no unique blocks)
  purrr::walk2(sigma_recs, new_sigmas, function(sigma_rec, new_sigmas_i){
    sigma_rec$parse()

    # inspect each record - filter to value options
    val_recs <- extract_record_values(sigma_rec)

    # Ensure replacement lengths are the same
    check_record_replacements(val_recs, new_sigmas_i)
    copy_record_opt(val_recs, new_sigmas_i, .bounds_opts)
  })
}


#' Copy omega records from one model to another
#'
#' @inheritParams copy_thetas
#' @param .new_omegas list of new omega matrices, or a single omega matrix
#'
#' @keywords internal
copy_omegas <- function(.mod_lines, .new_omegas, .bounds_opts){

  # Pull records and format replacement values
  param_setup <- setup_param_records(
    .mod_lines, .new_params = .new_omegas, .rec_type = "omega"
  )
  omega_recs <- param_setup$param_recs
  new_omegas <- param_setup$new_params

  # Base case (copy over values, no unique blocks)
  purrr::walk2(omega_recs, new_omegas, function(omega_rec, new_omegas_i){
    omega_rec$parse()

    # inspect each record - filter to value options
    val_recs <- extract_record_values(omega_rec)

    # Ensure replacement lengths are the same
    check_record_replacements(val_recs, new_omegas_i)
    copy_record_opt(val_recs, new_omegas_i, .bounds_opts)
  })
}



#' Extract the values from a record
extract_record_values <- function(.record){
  .record$parse()
  val_recs <- purrr::keep(.record$values, function(rec_opt){
    inherits(rec_opt, "nmrec_option") && !inherits(rec_opt, "nmrec_option_record_name") &&
      !inherits(rec_opt, c("nmrec_option_value")) && !inherits(rec_opt, c("nmrec_option_flag"))
  })

  return(val_recs)
}


#' Set up parameter records and replacement values
#'
#' Extract records of a given type and filter out prior blocks. Replacement values
#' and extracted records are formatted to be lists of equal length
#'
#' @param .mod_lines lines of ctl file. Must be an `nmrec` `nmrec_ctl_records` object
#' @param .new_params Either a list for multiple replacements, or one of the following:
#'        \describe{
#'        \item{`.rec_type = 'theta'`}{a `vector` of replacement values}
#'        \item{`.rec_type = 'sigma'` or `.rec_type = 'omega'`}{a `matrix` of replacement values}
#'        }
#' @param .rec_type Record type. One of `c("theta", "sigma", "omega")`
#'
#' @keywords internal
setup_param_records <- function(.mod_lines, .new_params, .rec_type = BBR_ESTIMATES_INHERIT){

  .rec_type <- match.arg(.rec_type)

  # coerce .new_params to list if not already
  # vectors/matricies are allowed for a replacement of 1:1 (n_theta_blocks:n_replacements)
  # if(!inherits(.new_params, "list")) .new_params <- list(.new_params)

  # Get parameter records
  param_recs <- nmrec::select_records(.mod_lines, .rec_type)

  # Get attributes for each record
  param_recs_spec <- get_record_attr(param_recs)

  # Filter out prior records specified without P/PV/PD
  # Priors specified with P/PV/PD are auto filtered out via `select_records`
  prior_rec <- nmrec::select_records(.mod_lines, "prior")
  param_recs <- filter_prior_records(param_recs, prior_rec, param_recs_spec)
  # Remove any filtered out records from spec
  param_recs_spec <- param_recs_spec[1:length(param_recs)]


  # Matrix handling
  if(.rec_type %in% c("sigma", "omega")){
    new_params_lst <- purrr::map(param_recs_spec, function(rec_spec){
      if(isTRUE(rec_spec$diag_matrix)){
        # Get diagonals if no covariance was specified
        rec_replacement <- unname(diag(.new_params))[rec_spec$index]
      }else if(isTRUE(rec_spec$cov_matrix)){
        # Get values of upper triangular matrix if covariance is specified
        mat_subset <- .new_params[rec_spec$index, rec_spec$index]
        rec_replacement <- mat_subset[upper.tri(mat_subset, diag = TRUE)]
      }else if(isTRUE(rec_spec$is_same)){
        # Ignore SAME records
        rec_replacement <- NULL
      }else{
        stop("add support")
      }
      return(rec_replacement)
    })
  }else{
    # Theta handling
    new_params_lst <- purrr::map(param_recs_spec, function(rec_spec){
      .new_params[rec_spec$index]
    })
  }


  # `param_recs` and `new_params_lst` should both be lists of the same length when replacing
  if(length(new_params_lst) != length(param_recs)){
    msg <- paste(
      glue::glue("Found {length(param_recs)} {.rec_type} records, which does not"),
      glue::glue("match up with the assumed length of `new_params_lst` ({length(new_params_lst)})")
    )
    stop(msg)
  }

  return(
    list(
      param_recs = param_recs,
      new_params = new_params_lst,
      param_recs_spec = param_recs_spec
    )
  )
}


#' Overwrite an `nmrec` record option
#'
#' @param val_recs list of `nmrec` record options.
#' @param new_values vector of replacement values. Should be the same length as
#'        `val_recs`
#' @inheritParams inherit_param_estimates
#'
#' @keywords internal
copy_record_opt <- function(val_recs, new_values, .bounds_opts){

  # Iterate over a single record object (e.g., a THETA block)
  purrr::walk2(val_recs, new_values, function(rec_opt_i, replacement_i){
    # Get bound type
    bound_type <- get_param_bound_type(rec_opt_i)

    # Get location of values
    val_locs <- inspect_option_class(rec_opt_i, c("nmrec_option_pos", "nmrec_option"), "index")

    # Replace single value
    if(bound_type == "fixed"){
      rec_opt_i$values[[val_locs]] <- replacement_i
    }else{
      if(.bounds_opts == "single_value"){
        # Remove bounds, replace with single value
        index_keep <- c(
          val_locs[1], # keep first value
          # keep parentheses if present
          inspect_option_class(
            rec_opt_i, .class = c("nmrec_paren_open", "nmrec_paren_close"),
            .operation = "index", .inherits = "any"
          )
        ) %>% sort()

        rec_opt_i$values <- rec_opt_i$values[index_keep]
        # Get new location of value and replace
        val_loc_new <- inspect_option_class(rec_opt_i, c("nmrec_option_pos", "nmrec_option"), "index")
        rec_opt_i$values[[val_loc_new]] <- replacement_i
      }else if(.bounds_opts == "maintain_bounds"){
        # Add starting value to bounds
        if(bound_type == "bounds_with_starting"){
          # If starting value already exists, overwrite it (middle value)
          checkmate::assert_true(length(val_locs) == 3)
          rec_opt_i$values[[val_locs[2]]] <- replacement_i
        }else if(bound_type =="bounds"){
          # If starting value does not exist, append it
          checkmate::assert_true(length(val_locs) == 2)
          # Create template position option
          # TODO: much of this code may move to nmrec:
          # It doesn't really matter too much, but ideally `rec_opt_i$values[[val_locs[1]]]`
          # should be replaced with actual `nmrec` building blocks
          new_opt_lst <- list(
            nmrec:::elem_comma(), nmrec:::elem_whitespace(" "),
            rec_opt_i$values[[val_locs[1]]]
          )
          rec_opt_i$values <- append(rec_opt_i$values, new_opt_lst, after = val_locs[1])
          val_loc_new <- inspect_option_class(rec_opt_i, c("nmrec_option_pos", "nmrec_option"), "index")
          rec_opt_i$values[[val_locs[2]]] <- replacement_i
        }
      }
    }
  })
}



#' Parse values per block
#'
#' @param .param_recs list of `nmrec` record objects specific to the parameter
#'
#' @keywords internal
get_record_attr <- function(.param_recs){
  checkmate::assert_list(.param_recs)
  # Get record labels
  record_labels <- purrr::imap(.param_recs, function(.record, .index){
    .record$parse()

    # Get value and flag options
    # SAME with no number is an nmrec_option_flag
    # SAME(value) is an nmrec_option_value
    rec_label <- purrr::keep(.record$values, function(rec_opt){
      inspect_option_class(
        rec_opt, c("nmrec_option_value", "nmrec_option_flag"), .inherits = "any"
      ) && !inherits(rec_opt, "nmrec_option_record_name") # drop record name
    })

    if(rlang::is_empty(rec_label)){
      tibble::tibble(index = .index, name = NA_character_, value = NA_real_)
    }else{
      purrr::map_dfr(rec_label, \(.x){
        if(inherits(.x, "nmrec_option_nested")){
          # If nested option (e.g, $THETA 4 FIX), the value is irrelevant
          tibble::tibble(index = .index, name = .x$format(), value = NA_real_)
        }else{
          # $OMEGA BLOCK(1) SAME(3) --> name = c('block', 'same'); value = c(1, 3)
          tibble::tibble(
            index = .index, name = .x$name, value = as.numeric(gsub("[^0-9]+", "", .x$value))
          )
        }
      })
    }
  })

  param_recs_spec <- purrr::map2(.param_recs, record_labels, \(.record, .label_df){
    labels <- .label_df$name

    # Get basic record attributes
    is_block <- nzchar(labels) && any(grepl("(?i)block", labels))
    is_same <- nzchar(labels) && any(grepl("(?i)same", labels))

    # Determine length of each record
    if(isTRUE(is_block)){
      if(isTRUE(is_same)){
        # Block handling with SAME(value)
        record_length <- .label_df$value[.label_df$name=="same"]
        # NA means a number of repeats wasn't defined, which means repeat once
        if(is.na(record_length)) record_length <- 1
      }else{
        # Default Block handling
        mat_size <- .label_df$value[.label_df$name=="block"]
        record_length <- mat_size #length(block(mat_size))
      }
    }else{
      # Single/diagonal values
      record_length <- length(extract_record_values(.record))
    }

    # Determine if covariance was specified for matrices - Must be at least `BLOCK(2)`
    cov_matrix <- isTRUE(is_block) && record_length >= 2 && isFALSE(is_same)
    diag_matrix <- (isFALSE(is_block) || record_length == 1) && isFALSE(is_same)

    list(
      record_type = .record$values[[1]]$name,
      record_length = record_length,
      cov_matrix = cov_matrix,
      diag_matrix = diag_matrix,
      is_same = is_same
    )
  })

  # Determine index for each record
  purrr::imap(param_recs_spec, \(.x, .y){
    record_length <- .x$record_length
    if (.y > 1) {
      end <- sum(purrr::map_dbl(param_recs_spec[1:.y], "record_length"))
      prev_end <- sum(purrr::map_dbl(param_recs_spec[1:(.y-1)], "record_length"))
      rec_range <- unique(c((prev_end + 1), end))
    }else{
      rec_range <- c(1, .x$record_length)
    }

    index <- if(length(rec_range) > 1){
      c(rec_range[1] : rec_range[2])
    }else{
      rec_range
    }

    c(.x, list(index = index))
  })
}




#' Determine if record option contains a specific class (count if desired)
#' @param .record_opt an `nmrec_option_pos` object to check. *Can be* nested
#'        (inherit class `nmrec_option_nested`).
#' @param .class class or vector of classes to look for.
#' @param .operation which operation to perform (i.e. what to return)
#'     \describe{
#'      \item{`'any'`}{Returns Logical (`TRUE`/`FALSE`): whether the `.class` is found in the record at all}
#'      \item{`'count'`}{Returns Numeric: frequency(ies) of `.class`}
#'      \item{`'index'`}{Returns Numeric: index(es) of `.class`}
#'      }
#' @param .inherits Either `'all'` or `'any'`. Only relevant if `class` is a vector.
#'        If `.inherits = 'all'`, the classes must correspond to the same element.
#'        If `.inherits = 'any'`, the classes can correspond to different elements.
#'
#' @keywords internal
inspect_option_class <- function(
    .record_opt,
    .class,
    .operation = c("any", "count", "index"),
    .inherits = c("all", "any")
){

  .operation <- match.arg(.operation)
  .inherits <- match.arg(.inherits)
  is_nested <- inherits(.record_opt, "nmrec_option_nested")

  check_class <- if(isTRUE(is_nested)){
    purrr::map_lgl(.record_opt$values, \(.x){
      checks <- purrr::map_lgl(.class, \(class_i) inherits(.x, class_i))
      if(.inherits == "all") all(checks) else any(checks)
    })
  }else{
    purrr::map_lgl(.class, \(class_i) inherits(.record_opt, class_i))
  }


  if(.operation == "any"){
    any(check_class)
  }else if(.operation == "count"){
    length(check_class[check_class])
  }else if(.operation == "index"){
    which(check_class)
  }
}





#' Determine the type of bounds (or fixed parameter) for model parameters
#'
#' This function determines the type of bounds specified for model parameters,
#' limited to THETA, OMEGA, and SIGMA records in a NONMEM control stream file.
#'
#' @param .record_opt an `nmrec_option_nested` object to check
#'
#' @keywords internal
get_param_bound_type <- function(.record_opt){

  correct_str <- inherits(.record_opt, "nmrec_option_nested") &&
    inherits(.record_opt, "nmrec_option_pos")
  if(!correct_str){
    cli::cli_abort(c("x" = "{.code .record_opt} is not in the correct format",
                     "i" = "Record option should inherit {.code nmrec_option_nested}
                     and {.code nmrec_option_pos}"))
  }

  # Check if option is bounded: in format (low, hi), or (low, start, hi)
  # TODO: confirm that this approach is valid
  is_bounded <- inspect_option_class(.record_opt, "nmrec_paren_open") &&
    inspect_option_class(.record_opt, "nmrec_paren_close") &&
    inspect_option_class(.record_opt, "nmrec_comma")

  if(isFALSE(is_bounded)){
    type <- "fixed"
  }else{
    num_vals <- inspect_option_class(.record_opt, "nmrec_option_pos", "count")
    type <- dplyr::case_when(
      num_vals == 2 ~ "bounds",
      num_vals == 3 ~ "bounds_with_starting",
      TRUE ~ NA_character_
    )
    if(is.na(type)) dev_error("unexpected record format")
  }

  return(type)
}


#' Filter out prior records
#'
#' @param .param_recs list of `nmrec` record objects specific to the parameter
#' @param .prior_rec list of `nmrec` record objects specifying priors
#'        (should be of length 1).
#' @param .spec list of record specifications for each element of `.param_recs`
#'
#' @details
#' `bbr` does not support the inheritance of priors. To simplify
#' the inheritance of previous parameter estimates, filter prior blocks out,
#' and ensure the returned object is still a list of records.
#'
#' multiple blocks with -same name- is older method for specifying priors.
#' We only copy over main blocks, so we have to check the $PRIOR record to
#' determine the expected number of initial values
#'
#' @keywords internal
filter_prior_records <- function(.param_recs, .prior_rec, .spec){
  checkmate::assert_list(.param_recs)

  if(rlang::is_empty(.prior_rec)){
    # Handling if no prior record is found (skip)
    return(.param_recs)
  }else{
    # Number of parameters found across all records of a given type
    n_params_found <- max(.spec[[length(.spec)]]$index)

    # Determine total number of expected records using the prior record
    .prior_rec <- .prior_rec[[1]]

    # Get relevant PRIOR option
    rec_type <- purrr::map_chr(.spec, \(.x) .x$record_type) %>% unique()
    prior_opt <- switch (rec_type,
      "theta" = "NTHETA",
      "omega" = "NETA",
      "sigma" = "NEPS"
    )
    prior_opt_rec <- nmrec::get_record_option(.prior_rec, prior_opt)

    if(!is.null(prior_opt_rec)){
      n_params_expected <- as.numeric(prior_opt_rec$value)

      # Filter out priors
      if(n_params_expected < n_params_found){
        recs_include <- purrr::map_lgl(.spec, \(.x) max(.x$index) <= n_params_expected)
        .param_recs <- .param_recs[recs_include]
      }else if(n_params_expected != n_params_found){
        dev_error("Unable to filter out prior records")
      }
    }

    return(.param_recs)
  }
}



#' Check that the replacement values are the same length as the record
#'
#' @inheritParams copy_record_opt
#'
#' @keywords internal
check_record_replacements <- function(val_recs, new_values){
  # This should never return FALSE. FALSE likely indicates a bug or lack of
  # support for a particular record format
  if(length(new_values) != length(val_recs)){
    val_recs_fmt <- purrr::map(val_recs, \(.x) .x$format())
    msg <- glue::glue("Record and/or replacement is in an unsupported format:
                      \nRecord:\n{glue_collapse(val_recs_fmt,  sep = ', ')}
                      \nReplacement:\n{glue_collapse(new_values,  sep = ', ')}\n\n")
    dev_error(msg)
  }else{
    return(invisible(TRUE))
  }
}
