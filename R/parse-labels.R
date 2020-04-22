
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
#' @importFrom stringr str_match str_split
parse_param_comment <- function(x, .theta = TRUE){
  if(inherits(x,'list'))
    x <- unlist(x)

  full_label_text <- str_split(x,'\\;') %>% sapply('[',2)

  label <- gsub('^(.*?)\\]\\s?','',full_label_text)

  unit <- str_match(full_label_text, '\\[(.*?)\\]')

  if (isTRUE(.theta)) {
    out_tbl <- tibble(label = label, unit = unit[,ncol(unit)])
  } else {
    out_tbl <- tibble(label = label, type = unit[,1])
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


