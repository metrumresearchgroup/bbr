
# Extract parameter labels for report tables, etc.

#' Generic s3 for parsing parameter labels from control stream
#' @param .mod generic model input
#' @param ... arguments passed through
#' @rdname param_labels
#' @export
param_labels <- function(.mod, ...) {
  UseMethod("param_labels", .mod)
}

#' Parse parameter labels from the model object control stream comments
#'
#' This is using the labeling scheme from "Census", as defined here: https://ghe.metrumrg.com/pages/software/mrgtable/reference/nm_tbl.html
#' It will _not_ return indices for the parameters, though they can be added with `param_labels() %>% apply_indices()`
#' @param .mod `bbi_{.model_type}_model` object
#' @importFrom readr read_file
#' @rdname param_labels
#' @export
param_labels.bbi_nonmem_model <- function(.mod, ...) {

  .ctl_path <- .mod %>% get_model_path()

  .ctl_raw <- .ctl_path %>% read_file()

  .label_df <- param_labels(.ctl_raw)

  return(.label_df)
}

#' Private implementation function for param_labels
#' @importFrom tidyr replace_na
#' @importFrom dplyr select everything
#' @importFrom purrr map_df
#' @rdname param_labels
#' @param .mod Character scaler of the raw control stream with newline character separating lines
#' @export
param_labels.character <- function(.mod, ...) {
  if(length(.mod) != 1) {
    stop(glue("param_labels() takes a character scaler of the raw control stream with newline character separating lines. A vector of length {length(.mod)} was passed."))
  }

  .ctl_clean <- clean_ctl(.mod)

  .label_df <- map_df(c("THETA", "OMEGA", "SIGMA"), function(.pick) { # are there other options than these three?
    .pick_labels <- parse_param_comment(
      .ctl_clean[[.pick]],
      .theta = (.pick == "THETA")
    )
    .pick_labels$names <- rep(.pick, nrow(.pick_labels))

    .pick_labels %>% select(names, everything())
  }) %>%
    tidyr::replace_na(list(label="", unit="", type=""))

  return(.label_df)
}

#' Add parameter indices to a label tibble
#' @param .label_df A tibble like the output of `param_labels()`, containing columns `names, label, unit, type`
#' @param .omega A logical vector indicating whether each Omega parameter is a diagonal. If `NULL` function assumes all are diagonal. Alternatively you can pass `block(.n)` or pass a custom vector if control stream has both block and non-block.
#' @param .sigma A logical vector indicating whether each Sigma parameter is a diagonal. If `NULL` function assumes all are diagonal. Alternatively you can pass `block(.n)` or pass a custom vector if control stream has both block and non-block.
#' @importFrom dplyr filter mutate n bind_rows
#' @rdname apply_indices
#' @export
apply_indices <- function(.label_df, .omega = NULL, .sigma = NULL) {
  .theta_df <- .label_df %>% filter(names == "THETA") %>% mutate(param_type = names)
  .omega_df <- .label_df %>% filter(names == "OMEGA") %>% mutate(param_type = names)
  .sigma_df <- .label_df %>% filter(names == "SIGMA") %>% mutate(param_type = names)

  # theta
  .theta_ind <- .theta_df %>% nrow() %>% seq_len()
  .theta_df <- .theta_df %>% mutate(names = paste0(names, .theta_ind))

  # omega
  if(is.null(.omega)) {
    .omega <- rep(TRUE, .omega_df %>% nrow()) # if null assume all are diagonals
  }
  .omega_ind <- build_matrix_indices(.omega)
  .omega_df <- .omega_df %>% mutate(names = paste0(names, .omega_ind))

  # sigma
  if(is.null(.sigma)) {
    .sigma <- rep(TRUE, .sigma_df %>% nrow()) # if null assume all are diagonals
  }
  .sigma_ind <- build_matrix_indices(.sigma)
  .sigma_df <- .sigma_df %>% mutate(names = paste0(names, .sigma_ind))


  return(bind_rows(
    .theta_df,
    .omega_df,
    .sigma_df
  ))
}

#' Helper function for formatting blocks into .omega or .sigma logical vectors
#' @param .n The size of the block
#' @rdname apply_indices
#' @export
block <- function(.n) {
  .is_diag <- logical(0)
  for (.d in seq_len(.n)) {
    .is_diag <- c(.is_diag, rep(FALSE, .d-1), TRUE)
  }
  return(.is_diag)
}


###################
# private helpers
###################

#' extract labels from comments in raw ctl string
#'
#' Modified tidynm::parse_theta() to parse out the comments into the right columns for all three params.
#' @param .x an object from the list that comes out of clean_ctl() (i.e. .ctl_clean$THETA, .ctl_clean$SIGMA, .ctl_clean$OMEGA)
#' @param .theta Boolean for if it's a theta. If TRUE, parses `[{}]` to unit, otherwise parses to type
#' @importFrom stringr str_match str_split str_trim
#' @importFrom dplyr mutate
parse_param_comment <- function(.x, .theta = FALSE){
  if(inherits(.x,'list'))
    .x <- unlist(.x)

  param_text <- str_split(.x,'\\;') %>% sapply('[',1)

  param_text <- str_split(param_text, " +")
  param_text <- map(param_text, function(.p) {
    .p <- as.numeric(.p)
    .p <- .p[!is.na(.p)]
    return(.p)
  })
  param_text


  full_label_text <- str_split(.x,'\\;') %>% sapply('[',2)

  ####
  if(length(param_text) != length(full_label_text)) {
    stop(glue("{length(param_text)} != {length(full_label_text)}"))
  }

  .out <- map_df(seq_len(length(param_text)), function(i) {

    this_param <- param_text[[i]]
      map_df(seq_len(length(this_param)), function(.p) {
        if (.p == (length(this_param))) {
          this_label <- full_label_text[[i]]
        } else {
          this_label <- ""
        }
      list(param = this_param[[.p]], label = this_label)
    })
  })

  full_label_text <- .out$label


  ####


  label <- gsub('^(.*?)\\]\\s?','',full_label_text)

  unit <- str_match(full_label_text, '\\[(.*?)\\]')

  if (isTRUE(.theta)) {
    out_tbl <- tibble(label = str_trim(label), unit = unit[,ncol(unit)])
  } else {
    out_tbl <- tibble(label = str_trim(label), type = unit[,1]) %>%
      mutate(
        type = ifelse(is.na(.data$type)|!nzchar(.data$type), "[A]", .data$type),
        # ^ from tidynm::param_tbl -- `TYPE = ifelse(is.na(!!(rlang::sym('TYPE')))|!nzchar(!!(rlang::sym('TYPE'))), "[A]", !!(rlang::sym('TYPE')))`
      )
  }

  return(out_tbl)
}


#' Builds matrix indices labels
#' @param .is_diag Boolean vector denoting whether each element is a diagonal
build_matrix_indices <- function(.is_diag) {
  total_diag <- sum(.is_diag)
  .x <- character(length(.is_diag))
  .y <- character(length(.is_diag))

  last_diag <- total_diag + 1
  for (i in order(seq_along(.is_diag), decreasing = TRUE)) {
    if (isTRUE(.is_diag[i])) {
      last_diag <- last_diag - 1
      y_ind <- last_diag
      .x[i] <- last_diag
      .y[i] <- last_diag
    } else {
      y_ind <- y_ind - 1
      .x[i] <- last_diag
      .y[i] <- y_ind
    }
  }

  return(paste0("(", .x, ",", .y, ")"))
}


########################################
# parsing function adapted from tidynm
########################################

#' Parse control stream to a named list of the blocks
#'
#' Taken directly from tidynm package.
#' @param x control stream as a character scaler with lines separated by newline characters
#' @importFrom purrr set_names
clean_ctl <- function(x){

  x0 <- strsplit(x,'\n')[[1]]
  x0 <- paste0(x0[!grepl('^\\s*;',x0)],collapse = '\n')

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

