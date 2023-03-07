
#' Inherit parameter estimates
#'
#' @param .mod
#'
#' @export
inherit_param_estimates <- function(.mod){

  based_on_path <- .mod %>% get_based_on()

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

  based_on_mod <- read_model(based_on_path)
  based_on_sum <- model_summary(based_on_mod)

  based_on_mod_lines <- bbr::ctl_ext(based_on_path) %>% readLines() %>%
    suppressSpecificWarning("incomplete final line found")


  # Identify THETA Blocks
  theta_specs <- get_param_block(based_on_mod_lines, .block = "THETA")
  theta_idxs <- theta_specs$param_idxs
  theta_block <- theta_specs$param_block
  new_thetas <- based_on_sum %>% get_theta()

  # Identify OMEGA Blocks
  omega_specs <- get_param_block(based_on_mod_lines, .block = "OMEGA")
  omega_idxs <- omega_specs$param_idxs
  omega_block <- omega_specs$param_block
  new_omegas <- based_on_sum %>% get_omega()

  # Identify SIGMA Blocks
  sigma_specs <- get_param_block(based_on_mod_lines, .block = "SIGMA")
  sigma_idxs <- sigma_specs$param_idxs
  sigma_block <- sigma_specs$param_block
  new_sigmas <- based_on_sum %>% get_sigma()

  # use string manipulation to paste back in

}


#' Get location and lines of specific code blocks in a ctl file
#'
#' @details
#' This function is used in `inherit_param_estimates`, and therefore contains some
#' additional filtering rules:
#'
#'  - Handles old and new method for specifying priors. These are filtered out
#'
#' @param .mod_lines ctl lines returned from readLines
#' @param .block character string defining which block in the control stream you are trying to parse
#'
#' @importFrom dplyr n_distinct
#'
#' @keywords internal
get_param_block <- function(.mod_lines, .block = c("THETA", "OMEGA", "SIGMA")){

  .block <- match.arg(.block)

  # Identify parameter blocks
  param_idxs <- get_block_idx(.mod_lines, .block = .block)

  param_block <- map(seq_along(param_idxs), ~{
    .mod_lines[param_idxs[[.x]]]
  })

  block_labels <- get_block_label(param_block) %>% unlist()

  # multiple blocks with -same name- is old method for specifying priors. Only copy over main block
  if(length(block_labels) > 1 & n_distinct(block_labels) == 1){
    param_idxs <- param_idxs[[1]]
    param_block <- param_block[[1]]
  }

  # new method for priors uses a 'P' or 'PV' at the end of the block label. Filter those out
  has_P_or_PV <- grepl("P$|PV$", block_labels) # search for "P$" or "PV$"
  if(any(has_P_or_PV)){
    param_idxs <- param_idxs[!has_P_or_PV]
    param_block <- param_block[!has_P_or_PV]
  }

  if (length(param_block) == 1) {
    param_block <- unlist(param_block)
    param_idxs <- unlist(param_idxs)
  }

  return(
    list(
      param_block = param_block,
      param_idxs = param_idxs
    )
  )
}


#' Get block label from parsed control stream file
#'
#' @param .block character vector or list of vectors containing the lines of the control stream file.
#'         Must contain a block header (e.g. $OMEGA)
#'
#' @keywords internal
get_block_label <- function(.block){
  if(inherits(.block, "list")){
    block_label <- purrr::map(.block, ~ {
      gsub("^\\$", "", regmatches(.x, regexpr("\\$([[:alpha:]]+)\\b", .x)))
    })
  }else{
    block_label <- gsub("^\\$", "", regmatches(.block, regexpr("\\$([[:alpha:]]+)\\b", .block)))
  }

  return(block_label)
}
