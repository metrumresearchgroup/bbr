
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
    .inherit = c("theta", "sigma"),
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
    copy_omegas(inherit_mod_lines, .new_omegas = new_omegas)
  }

  # Update SIGMA Block
  if("sigma" %in% .inherit){
    new_sigmas <- based_on_sum %>% get_sigma() %>% signif(digits = .digits)
    copy_sigmas(inherit_mod_lines, .new_sigmas = new_sigmas)
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
    val_recs <- purrr::keep(theta_rec$values, function(rec_opt){
      inherits(rec_opt, "nmrec_option") && !inherits(rec_opt, "nmrec_option_record_name")
    })

    # Ensure replacement lengths are the same (TODO: handle this differently)
    checkmate::assert_true(length(new_thetas_i) == length(val_recs))
    modify_record_opt(val_recs, new_thetas_i, .bounds_opts)
  })
}


#' Copy sigma records from one model to another
#'
#' @inheritParams copy_thetas
#' @param .new_sigmas list of new sigma matrices, or a single sigma matrix
#'
#' @keywords internal
copy_sigmas <- function(.mod_lines, .new_sigmas){

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
    val_recs <- purrr::keep(sigma_rec$values, function(rec_opt){
      inherits(rec_opt, "nmrec_option") && !inherits(rec_opt, "nmrec_option_record_name")
    })

    # Ensure replacement lengths are the same (TODO: handle this differently)
    checkmate::assert_true(length(new_sigmas_i) == length(val_recs))
    modify_record_opt(val_recs, new_sigmas_i, .bounds_opts)
  })
}


#' Copy omega records from one model to another
#'
#' @inheritParams copy_thetas
#' @param .new_omegas list of new omega matrices, or a single omega matrix
#'
#' @keywords internal
copy_omegas <- function(.mod_lines, .new_omegas){

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
    val_recs <- purrr::keep(omega_rec$values, function(rec_opt){
      inherits(rec_opt, "nmrec_option") && !inherits(rec_opt, c("nmrec_option_record_name")) &&
        !inherits(rec_opt, c("nmrec_option_value"))
    })

    # Ensure replacement lengths are the same (TODO: handle this differently)
    checkmate::assert_true(length(new_omegas_i) == length(val_recs))

    purrr::walk2(val_recs, new_omegas_i, function(rec_opt_i, omega_i){
      # Inspect the position of each record, replace the value
      purrr::walk(rec_opt_i$values, function(rec_opt_values){
        if(inherits(rec_opt_values, "nmrec_option") && inherits(rec_opt_values, "nmrec_option_pos")){
          rec_opt_values$value <- omega_i
        }
      })
    })
  })
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

  # Get parameter records
  param_recs <- nmrec::select_records(.mod_lines, .rec_type)

  # Filter out prior records
  param_recs <- filter_prior_records(param_recs)

  # coerce .new_params to list if not already
  # vectors/matricies are allowed for a replacement of 1:1 (n_theta_blocks:n_replacements)
  if(!inherits(.new_params, "list")) .new_params <- list(.new_params)

  # TODO: determine relevant specs of for each type that set the inheritance procedure

  # Matrix handling
  cov_specified <- FALSE # TODO: determine if covariance is specified in ctl
  if(.rec_type %in% c("sigma", "omega")){
    matrix_types <- get_matrix_types(param_recs)
    .new_params <- purrr::map2(.new_params, matrix_types, function(new_params_i, mat_type){
      if(is.null(mat_type) || (mat_type == "block" && isFALSE(cov_specified))){
        # Grab diagonals for block matrices
        new_params_i <- unname(diag(new_params_i))
      }else if(mat_type == "block" & isTRUE(cov_specified)){
        new_params_i <- new_params_i[upper.tri(new_params_i, diag = TRUE)]
      }else{
        stop("add support")
      }
    })
  }


  # `param_recs` and `.new_params` should both be lists of the same length when replacing
  if(length(.new_params) != length(param_recs)){
    msg <- paste(
      glue::glue("Found {length(param_recs)} {.rec_type} records, which does not"),
      glue::glue("match up with the assumed length of `.new_params` ({length(.new_params)})")
    )
    stop(msg)
  }

  return(
    list(
      param_recs = param_recs,
      new_params = .new_params
    )
  )
}




#' Filter out prior records
#'
#' @param .records list of `nmrec` record objects
#'
#' @details
#' `bbr` does not support the inheritance of priors at this time. To simplify
#' the inheritance of previous parameter estimates, filter prior blocks out,
#' and ensure the returned object is still a list of records.
#'
#' @keywords internal
filter_prior_records <- function(.records){
  # Get record labels
  block_labels <- get_block_labels(.records) %>% unlist()

  ### Remove any priors, as these dont need to be copied over ###
  # multiple blocks with -same name- is old method for specifying priors. Only copy over main block
  # this logic is likely not enough and does not properly discern whether the other matrix is a prior or not
  if(length(block_labels) > 1 & dplyr::n_distinct(block_labels) == 1){
    .records <- list(.records[[1]])
  }

  # new method for priors uses a 'P' or 'PV' at the end of the block label. Filter those out
  has_P_or_PV <- grepl("P$|PV$", block_labels) # search for "P$" or "PV$"
  if(any(has_P_or_PV)){
    .records <- .records[has_P_or_PV]
  }

  return(.records)
}



#' Get block label from parsed control stream file
#'
#' @param .block character vector or list of vectors containing the lines of the control stream file.
#'         Must contain a block header (e.g. $OMEGA)
#'
#' @keywords internal
get_block_labels <- function(.blocks){
  if(!inherits(.blocks, "list")) .blocks <- list(.blocks)

  purrr::map(.blocks, function(.block){
    .block$parse()
    block_label <- purrr::keep(.block$values, function(rec_opt){
      inherits(rec_opt, "nmrec_option_record_name")
    })

    if(rlang::is_empty(block_label)){
      return(NULL)
    }else{
      gsub("\\$", "", block_label[[1]]$format())
    }
  })
}


#' Get matrix type for omega and sigma records
#'
#' @inheritParams get_block_labels
#'
#' @keywords internal
get_matrix_types <- function(.blocks){
  if(!inherits(.blocks, "list")) .blocks <- list(.blocks)

  purrr::map(.blocks, function(.block){
    .block$parse()
    block_label <- purrr::keep(.block$values, function(rec_opt){
      inherits(rec_opt, "nmrec_option_value")
    })

    if(rlang::is_empty(block_label)){
      return(NULL)
    }else{
      block_label[[1]]$name
    }
  })
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



#' Modify an `nmrec` record option
#'
#' @inheritParams get_param_bound_type
#'
#' @keywords internal
modify_record_opt <- function(val_recs, new_values, .bounds_opts){

  # Iterate over a single record object (e.g., a THETA block)
  purrr::walk2(val_recs, new_thetas_i, function(rec_opt_i, replacement_i){
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
          # TODO: much of this code may move to nmrec
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


