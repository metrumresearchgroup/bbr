setwd("~/tidynm/tests/testthat")

library(rbabylon)
library(dplyr)
library(purrr)
library(stringr)
library(glue)

#library(tidynm) # I just sourced this stuff from disk instead (I think most of it is private anyway)
source("~/tidynm/R/ctl_parse.R")
#only needed for second option ("let tidynm parse all the way through")
source("~/tidynm/R/parse_theta.R")
source("~/tidynm/R/ctl_to_mat.R")
source("~/tidynm/R/utils.R")

##################################################
# trying to do it manually with their helper functions

MODEL_DIR <- "../../inst/extdata"
options('rbabylon.bbi_exe_path' = '/data/apps/bbi')
set_model_directory(MODEL_DIR)
bbi_init(MODEL_DIR, "/opt/NONMEM", "nm74gf")

MODEL_PICK <- "510" ### 101 triggers fails because of this error https://github.com/metrumresearchgroup/babylon/issues/163

ref_df <- readRDS(glue("../benchmark/{MODEL_PICK}_PARAMTBL.rds"))

if (fs::file_exists(file.path(MODEL_DIR, glue("{MODEL_PICK}.yaml")))) fs::file_delete(file.path(MODEL_DIR, glue("{MODEL_PICK}.yaml")))
.param_df <- rbabylon::new_model(glue("{MODEL_PICK}.yaml"), glue("the {MODEL_PICK} model")) %>%
  rbabylon::model_summary() %>% param_estimates()

.ctl_raw <- readr::read_lines(glue("../../inst/extdata/{MODEL_PICK}.ctl")) %>% paste(collapse = "\n")

####### custom helpers for the parsing


#' modified parse_theta()
#' @param x an object from the list that comes out of clean_ctl() (i.e. .ctl_clean$THETA, .ctl_clean$SIGMA, .ctl_clean$OMEGA)
#' @param .theta Boolean for if it's a theta. If TRUE, parses [] to unit, otherwise parses to type
parse_param_comment <- function(x, .theta = TRUE){
  if(inherits(x,'list'))
    x <- unlist(x)

  full_label_text <- str_split(x,'\\;') %>% sapply('[',2)

  label <- gsub('^(.*?)\\]','',full_label_text)

  unit <- str_match(full_label_text, '\\[(.*?)\\]')

  if (isTRUE(.theta)) {
    out_tbl <- tibble(label = label, unit = unit[,ncol(unit)])
  } else {
    out_tbl <- tibble(label = label, type = unit[,1])
  }

  return(out_tbl)
}


#' THIS IS FOR THE SECOND OPTION ONLY
#' @param .ctl_list the list that's output from parse_ctl()
unpack_params_mat <- function(.pick = c("SIGMA", "OMEGA"), .param_df, .ctl_list) {
  .pick <- match.arg(.pick)
  .pick_names <- .param_df$names %>% str_subset(.pick)

  map_df(.pick_names, function(.name) {
    # extract indices from row name
    .ind = .name %>% str_replace_all(glue("{.pick}|\\(|\\)"), "") %>% str_split(",") %>% unlist() %>% as.numeric()

    # extract comment from comment matrix
    .comment <- .ctl_list[[glue("{.pick}_COMMENT")]][.ind[1], .ind[2]]

    list(names = .name, comment = .comment)
  })
}


#########
# just parse from cleaned ctl strings

.ctl_clean <- clean_ctl(.ctl_raw)

.label_df <- map_df(c("THETA", "OMEGA", "SIGMA"), function(.pick) {
  .pick_labels <- parse_param_comment(
                    .ctl_clean[[.pick]],
                    .theta = (.pick == "THETA")
                  )
  .pick_names <- .param_df$names %>% str_subset(.pick)

  if (length(.pick_names) != length(.pick_labels$label)) {
    stop(glue("{length(.pick_names)} names and {length(.pick_labels$label)} labels. Must be the same length."))
  }
  .pick_labels$names <- .pick_names

  .pick_labels
})
#.label_df

left_join(.param_df, .label_df)


#########
# let tidynm parse all the way through

.ctl_list <- ctl_parse(.ctl_raw)

.label_df <- map_df(c("THETA", "OMEGA", "SIGMA"), function(.pick) {
  if (.pick == "THETA") { #weirdly tidynm parses the theta table but not the others :shrug:
    .pick_labels <- tibble(
      names = .param_df$names %>% str_subset(.pick),
      label = .ctl_list$THETA$LABEL,
      unit = .ctl_list$THETA$UNIT
    )
  } else {
    .pick_labels <- unpack_params_mat(.pick, .param_df, .ctl_list) %>%
      mutate(
        label = str_split(.data$comment,'\\|\\|') %>% sapply('[',3),
        type = str_split(.data$comment,'\\|\\|') %>% sapply('[',2)
      ) %>%
      select(!comment)
  }

  .pick_labels
})
#.label_df

left_join(.param_df, .label_df)


