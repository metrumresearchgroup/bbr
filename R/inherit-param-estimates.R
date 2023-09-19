
#' Supported estimates to inherit from parent model
BBR_ESTIMATES_INHERIT <- c("theta", "sigma", "omega")


#' Inherit parameter estimates
#'
#' @param .mod new model object to overwrite.
#' @param .parent_mod model path to inherit properties from.
#' @param .inherit vector of estimates to inherit from parent model.
#'
#' @export
inherit_param_estimates <- function(.mod, .parent_mod = get_based_on(.mod), .inherit = c("theta", "sigma")){

  checkmate::assert_true(all(.inherit %in% BBR_ESTIMATES_INHERIT))

  # Confirm .parent_mod is valid
  based_on_path <- .parent_mod

  if(is.null(based_on_path) || !fs::file_exists(based_on_path)){
    msg_prefix <- if(is.null(based_on_path)){
      ".mod %>% get_based_on() returned `NULL`"
    }else{
      glue::glue("Parent model does not exist at: {based_on_path}")
    }
    msg <- glue::glue("{msg_prefix}
      To inherit parameter estimates from a parent model, this must be a valid file path.")
    stop(msg)
  }

  # Parent model objects
  based_on_mod <- read_model(based_on_path)
  based_on_sum <- model_summary(based_on_mod)

  # Update THETA Block
  if("theta" %in% .inherit){
    new_thetas <- based_on_sum %>% get_theta() %>% unname()
    copy_thetas(.parent_mod, .new_thetas = new_thetas)
  }

  # Update OMEGA Block
  if("omega" %in% .inherit){
    new_omegas <- based_on_sum %>% get_omega()
    # copy_omegas(.parent_mod, .new_omegas = new_omegas)
  }

  # Update SIGMA Block
  if("sigma" %in% .inherit){
    new_sigmas <- based_on_sum %>% get_sigma()
    # copy_sigmas(.parent_mod, .new_sigmas = new_sigmas)
  }

  return(invisible(.parent_mod))
}


copy_thetas <- function(.parent_mod, .new_thetas){

  parent_mod_lines <- ctl_ext(.parent_mod) %>% nmrec::read_ctl()
  theta_recs <- nmrec::select_records(parent_mod_lines, "theta")

  # coerce .new_thetas to list if not already
  # vectors are allowed for a replacement of 1:1 (n_theta_blocks:n_replacements)
  if(!inherits(.new_thetas, "list")) .new_thetas <- list(.new_thetas)

  # TODO: determine relevant specs of theta blocks that set the inheritance procedure

  # Filter out prior records
  # TODO: this actually removes them - we dont want this behavior
  theta_recs <- filter_prior_records(theta_recs)


  # `theta_recs` and `.new_thetas` should both be lists of the same length when replacing
  if(length(.new_thetas) != length(theta_recs)){
    msg <- paste(
      glue::glue("Found {length(theta_recs)} theta records, which does not"),
      glue::glue("match up with the assumed length of `.new_thetas` ({length(.new_thetas)})")
    )
    stop(msg)
  }else{
    # Base case (copy over values, no unique blocks)
    purrr::walk2(theta_recs, .new_thetas, function(theta_rec, thetas){
      theta_rec$parse()

      # inspect each record - filter to value options
      theta_val_recs <- purrr::keep(theta_rec$values, function(rec_opt){
        inherits(rec_opt, "nmrec_option") && !inherits(rec_opt, "nmrec_option_record_name")
      })

      # Ensure replacement lengths are the same (TODO: handle this differently)
      checkmate::assert_true(length(thetas) == length(theta_val_recs))

      purrr::walk2(theta_val_recs, thetas, function(rec_opt_i, theta_i){
        # Inspect the position of each record, replace the value
        purrr::walk(rec_opt_i$values, function(rec_opt_values){
          if(inherits(rec_opt_values, "nmrec_option")){
            rec_opt_values$value <- theta_i
          }
        })
      })
    })
    nmrec::write_ctl(parent_mod_lines, ctl_ext(.parent_mod))
  }

}



#' Get block label from parsed control stream file
#'
#' @param .block character vector or list of vectors containing the lines of the control stream file.
#'         Must contain a block header (e.g. $OMEGA)
#'
#' @keywords internal
get_block_labels <- function(.blocks){
  checkmate::assert_true(inherits(.blocks, "list"))

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
