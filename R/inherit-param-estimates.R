
#' Inherit parameter estimates
#'
#' @param .mod
#'
#' @export
inherit_param_estimates <- function(.mod){

  based_on_path <- .mod %>% get_based_on() # TODO: make this an argument that can be overwritten with a filepath or model object

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
  # Extract numerical values from theta_block - except first element (which contains $THETA)
  # theta_values <- lapply(theta_block[-1], function(x) as.numeric(readr::parse_number(x))) %>% unlist()

  # Update THETA values
  new_thetas <- based_on_sum %>% get_theta() %>% unname()

  for (i in seq_along(new_thetas)) {
    theta_block[i+1] <- gsub("\\b[-+]?\\d+\\.?\\d*\\b", new_thetas[i], theta_block[i+1])
  }
  based_on_mod_lines[theta_idxs] <- theta_block

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
#'  - Will filter out any comments (as in they wont be overwritten)
#'  - Will remove any empty lines
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
  param_idxs <- get_block_idx(.mod_lines, .block = .block) # always a list

  param_block <- map(seq_along(param_idxs), ~{
    .mod_lines[param_idxs[[.x]]]
  })

  # Filter out any comments (';') and empty lines
  keep_idxs <- lapply(param_block, function(block){
    idxs <- grep("^\\s*;[^\\s]", block, invert = TRUE)
    idxs[nchar(block[idxs]) > 0]
  })
  # Subset param_idxs and param_block to remove the elements beginning with a comment
  param_idxs <- lapply(seq_along(param_idxs), function(i) param_idxs[[i]][keep_idxs[[i]]])
  param_block <- lapply(seq_along(param_block), function(i) param_block[[i]][keep_idxs[[i]]])


  # Remove any priors, as these dont need to be copied over
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

  # Returning a list should mean there are multiple parameter blocks, and none are priors (e.g., OMEGA blocks with covariance)
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
