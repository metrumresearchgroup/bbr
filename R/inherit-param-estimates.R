
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
    .bounds_opts = c("starting_val"),
    .digits = 3
){

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
    copy_thetas(inherit_mod_lines, .new_thetas = new_thetas)
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
copy_thetas <- function(.mod_lines, .new_thetas){

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

    purrr::walk2(val_recs, new_thetas_i, function(rec_opt_i, theta_i){
      # Inspect the position of each record, replace the value
      purrr::walk(rec_opt_i$values, function(rec_opt_values){
        if(inherits(rec_opt_values, "nmrec_option")){
          rec_opt_values$value <- theta_i
        }
      })
    })
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

    purrr::walk2(val_recs, new_sigmas_i, function(rec_opt_i, sigma_i){
      # Inspect the position of each record, replace the value
      purrr::walk(rec_opt_i$values, function(rec_opt_values){
        if(inherits(rec_opt_values, "nmrec_option") && inherits(rec_opt_values, "nmrec_option_pos")){
          rec_opt_values$value <- sigma_i
        }
      })
    })
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
      inherits(rec_opt, "nmrec_option") && !inherits(rec_opt, c("nmrec_option_record_name", "nmrec_option_value"))
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
