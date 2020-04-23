
####### THIS WILL PROBABLY ALL GO IN SUMMARY INSTEAD. Or utils or something.
library(readr)
library(stringr)
library(tidyr)
library(purrr)
library(dplyr)



#######

#########################################
# extract labels from comments in ctl
#########################################

#' modified parse_theta() to parse out the comments into the right columns
#' @param x an object from the list that comes out of clean_ctl() (i.e. .ctl_clean$THETA, .ctl_clean$SIGMA, .ctl_clean$OMEGA)
#' @param .theta Boolean for if it's a theta. If TRUE, parses `[{}]` to unit, otherwise parses to type
#' @importFrom stringr str_match str_split str_trim
parse_param_comment <- function(x, .theta = TRUE){
  if(inherits(x,'list'))
    x <- unlist(x)

  full_label_text <- str_split(x,'\\;') %>% sapply('[',2)

  label <- gsub('^(.*?)\\]\\s?','',full_label_text)

  unit <- str_match(full_label_text, '\\[(.*?)\\]')

  if (isTRUE(.theta)) {
    out_tbl <- tibble(label = str_trim(label), unit = unit[,ncol(unit)])
  } else {
    out_tbl <- tibble(label = str_trim(label), type = unit[,1])
  }

  return(out_tbl)
}

#' The real function to parse the labels from the control stream
#'
#' Still not sure on the interface here. Should it be passed a model? a param_df?
#' @importFrom readr read_lines
#' @importFrom tidyr replace_na
#' @importFrom dplyr select everything
#' @importFrom purrr map_df
#' @export
param_labels <- function(.mod) {

  .ctl_path <- .mod %>% get_model_path()

  .ctl_raw <- .ctl_path %>% read_lines() %>% paste(collapse = "\n")

  .ctl_clean <- clean_ctl(.ctl_raw)

  .label_df <- map_df(c("THETA", "OMEGA", "SIGMA"), function(.pick) { # are there other options than these three?
    .pick_labels <- parse_param_comment(
      .ctl_clean[[.pick]],
      .theta = (.pick == "THETA")
    )
    .pick_labels$names <- rep(.pick, nrow(.pick_labels))

    #### try to parse the indices here

    .pick_labels %>% select(names, everything())
  }) %>%
    tidyr::replace_na(list(unit="", type=""))

  return(.label_df)
}


#' @importFrom dplyr filter mutate n
apply_indices <- function(.label_df, .theta = NULL, .omega = NULL, .sigma = NULL) {
  .theta_df <- .label_df %>% filter(names == "THETA") %>% mutate(param_type = names)
  .omega_df <- .label_df %>% filter(names == "OMEGA") %>% mutate(param_type = names)
  .sigma_df <- .label_df %>% filter(names == "SIGMA") %>% mutate(param_type = names)


  if(is.null(.theta)) {
    .theta <- .theta_df %>% nrow() %>% seq_len()
  }
  .theta_df <- .theta_df %>% mutate(names = paste0(names, .theta))

  if (is.null(.omega)) {
    is_diag <- ifelse(.omega_df$type == "[P]", T, F) # define boolean vector for diagonal
    .omega <- build_matrix_indices(is_diag)

  }
  .omega_df <- .omega_df %>% mutate(names = paste0(names, .omega))

  if (is.null(.sigma)) {
    is_diag <- ifelse(.sigma_df$type == "[P]" | .sigma_df$type == "[A]", T, F) # define boolean vector for diagonal
    .sigma <- build_matrix_indices(is_diag)

  }
  .sigma_df <- .sigma_df %>% mutate(names = paste0(names, .sigma))

  return(bind_rows(
    .theta_df,
    .omega_df,
    .sigma_df
  ))
}


#' @param is_diag Boolean vector denoting whether each element is a diagonal
build_matrix_indices <- function(is_diag) {
  total_diag <- sum(is_diag)
  .x <- character(length(is_diag))
  .y <- character(length(is_diag))

  last_diag <- total_diag + 1
  for (i in order(seq_along(is_diag), decreasing = TRUE)) {
    if (isTRUE(is_diag[i])) {
      last_diag <- last_diag - 1
      x_ind <- last_diag
      .x[i] <- last_diag
      .y[i] <- last_diag
    } else {
      x_ind <- x_ind - 1
      .x[i] <- x_ind
      .y[i] <- last_diag
    }
  }

  return(paste0("(", .x, ",", .y, ")"))
}

#### live coding from Devin
# ## refactor this block out into its own function
# is_diag <- ifelse(.omega_df$type == "[P]", 1, 0)
# total_diag <- sum(is_diag, na.rm = TRUE)
# labels <- character(length(is_diag))
# last_seen_diag <- total_diag + 1 # because in loop will decrement each time is_diag hits this
# last_seen_off_diag <- 0
# for (i in order(seq_along(is_diag), decreasing = TRUE)) {
#   cur_label <- is_diag[i]
#   if (cur_label == 1) {
#     last_seen_diag <- last_seen_diag - 1
#     last_seen_off_diag <- last_seen_diag
#     labels[i] <- sprintf("%s,%s", last_seen_diag, last_seen_diag)
#   } else {
#     last_seen_off_diag <- last_seen_off_diag - 1
#     labels[i] <- sprintf("%s,%s", last_seen_diag, last_seen_off_diag)
#   }
#
# }
# labels



#########################################
# parsing functions adapted from tidynm
#########################################


#' @importFrom purrr set_names
clean_ctl <- function(x){
  x0 <- ctl_rm_comments(x)
  x1 <- gsub('(\n\n|\n|\n;|\n\n;)$','',strsplit(x0,'\\$')[[1]])
  x2 <- sub('\n',' ',x1)
  x3 <- as.list(gsub('^(.*?)\\s+','',x2))
  names(x3) <- gsub('\\s+(.*?)$','',x2)

  x3 <- x3[sapply(x3,nzchar)]

  x3 <- split(x3,names(x3))

  x3 <- lapply(x3,purrr::set_names,nm=NULL)

  x4 <- lapply(x3,function(x){
    if(is.list(x)){
      out <- lapply(x,function(xx){
        out <- gsub('^\\s+|\\s+$','',strsplit(xx,'\n')[[1]])
        out[nzchar(out)]
      })
      list(out)
    }else{
      out <- gsub('^\\s+|\\s+$','',strsplit(x,'\n')[[1]])
      out[nzchar(out)]
    }

  })

  nc <- names(x4)[(!nzchar(gsub('[A-Z]','',names(x4))))]

  x4 <- x4[nc[nchar(nc)>1]]

  unlist(x4,recursive = FALSE)
}

ctl_rm_comments <- function(x){
  x0 <- strsplit(x,'\n')[[1]]
  paste0(x0[!grepl('^\\s*;',x0)],collapse = '\n')
}


#############
# dput(.pick_labels)
#
#
# .pick_labels <- structure(list(label = c(" Ka", " CL", NA, " V2"), type = c("[P]",
#                                                                             "[P]", NA, "[P]")), row.names = c(NA, -4L), class = c("tbl_df",
#                                                                                                                                   "tbl", "data.frame"))
#
# .pick_labels %>%
#   mutate(
#     diag = case_when(type == "[P]" ~ 1, TRUE ~ 0),
#     diag_num = cumsum(diag),
#     offdiag = ifelse(diag, 0, 1),
#     offdiag_num = cumsum(offdiag)
#   )


