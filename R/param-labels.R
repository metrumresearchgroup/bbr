
#' Extract parameter labels for report tables, etc.
#'
#' Parse parameter labels from the model object. Currently this parses labels from comments in the  control stream,
#' However it will be extended to parse labels from the model YAML file as well.
#' The syntax for the labeling is described in "Details" below.
#'
#' @details
#' Note that `param_labels()` will *not* return indices for the parameters, though they can be added with `param_labels() %>% apply_indices()`.
#' See the `apply_indices()` documentation for more details.
#'
#' The syntax for parsing labels from comments is inherited from the "Census" specification
#' that was also used in the `tidynm` and `mrgtable` packages. The syntax is as follows,
#' with everything after the comment character `;` parsed into the label.
#'
#' \code{<nonmem_inits> ; [directive] <Parameter name <LOGD>>}
#'
#' \itemize{
#' \item Inits are the NONMEM initial values, following NONMEM specs
#' \item Directives include:
#'   \itemize{
#'   \item For fixed effects the directives are translated to units, i.e., L/h
#'   \item For variance components (and only variance components) the directive is parsed into the `type` column
#'   and can be used for post-processing transformations. Some common types include:
#'   \itemize{
#'       \item \[A\] is the assumed directive and it yields standard deviation / correlation.
#'       Typically used when the random component has a linear relationship to the typical value.
#'       \item \[P\] yields Coefficient of Variation.
#'       Commonly transformed with \eqn{CV_lognormal = sqrt( exp(omega^2)-1 ) \times 100} for omegas,
#'       and the value of omega from the correlation matrix (i.e., the SD) for sigmas.
#'       Typically used with exponentiated random effects or proportional residual error.
#'       \item \[R\] indicates that this is an item that should be read from the correlation
#'       matrix as opposed to the covariance matrix.
#'       I.e., standard deviation and correlation of the parameter on the raw scale will be reported.
#'       \item \[C\] indicates that this item should be read from the covariance matrix, i.e., variance
#'        and covariance of the parameter on the raw scale will be reported.
#'      }
#'   }
#' \item Parameter name is parsed into the `label` column, and is commonly formatted to be run through a LaTex parser downstream.
#' \item LOGD is a flag to indicate that the parameter is modeled on the log
#' scale (i.e., \eqn{e^\theta}) and should be exponentiated for reporting.
#' If so, the standard error for the parameter should adjusted via the delta method.
#' NOTE: this is only a convention, such that downstream code can look for "LOGD" in the `label` column
#' and transform the estimates accordingly. None of that processing is done automatically by this function.
#' }
#'
#' @param .mod generic model input
#' @param ... arguments passed through (currently to nowhere)
#' @export
param_labels <- function(.mod, ...) {
  UseMethod("param_labels", .mod)
}


#' @describeIn param_labels Takes a `bbi_nonmem_model` object
#' @importFrom readr read_file
#' @export
param_labels.bbi_nonmem_model <- function(.mod, ...) {

  .ctl_path <- .mod %>% get_model_path()

  .ctl_raw <- .ctl_path %>% read_file()

  .label_df <- param_labels(.ctl_raw, ...)

  return(.label_df)
}

#' @describeIn param_labels Takes a character scalar of the raw control stream with newline character separating lines
#' @importFrom tidyr replace_na
#' @importFrom dplyr select everything
#' @importFrom purrr map_df
#' @importFrom tidyselect all_of
#' @export
param_labels.character <- function(.mod, ...) {
  if(length(.mod) != 1) {
    stop(glue("param_labels() takes a character scalar of the raw control stream with newline character separating lines. A vector of length {length(.mod)} was passed."))
  }

  .ctl_clean <- clean_ctl(.mod)

  .label_df <- map_df(c("THETA", "OMEGA", "SIGMA"), function(.pick) {
    .pick_list <- .ctl_clean[[.pick]]

    if (is.null(.pick_list)) {
      return(NULL)
    }

    .pick_labels <- parse_param_comment(
      .pick_list,
      .theta = (.pick == "THETA")
    )
    .pick_labels[[SUMMARY_PARAM_NAMES]] <- rep(.pick, nrow(.pick_labels))
     return(select(.pick_labels, all_of(SUMMARY_PARAM_NAMES), everything()))
  }) %>%
    tidyr::replace_na(list(label="", unit="", type=""))

  return(.label_df)
}

#' Add parameter indices to a label tibble
#'
#' Because there are numerous ways of specifying the diagonal and off-diagonal elements of an `$OMEGA` or `$SIGMA` block in a control stream,
#' automatically parsing the structure of these blocks can be brittle and error prone. For this reason, indices are *not* automatically added
#' to the output of the `param_labels()` function and are instead added with the  `apply_indices()` function.
#'
#' @details
#' `block()` is a helper function for formatting blocks into .omega or .sigma logical vectors.
#' It takes an integer and returns a logical vector indicating whether each element
#' of an `.n`-sized block with diagonal or not.
#'
#' For more details and examples of how to specify `$OMEGA` and `$SIGMA` block structure, see the "Parameter Labels" vignette:
#' [`vignette("parameter-labels", package = "bbr")`](../docs/articles/parameter-labels.html)
#'
#' @param .label_df A tibble like the output of `param_labels()`, containing columns `parameter_names, label, unit, type`
#' @param .omega A logical vector indicating whether each Omega parameter is a diagonal. If `NULL` function assumes all are diagonal. Alternatively you can pass `block(.n)` or pass a custom vector if control stream has both block and non-block.
#' @param .sigma A logical vector indicating whether each Sigma parameter is a diagonal. If `NULL` function assumes all are diagonal. Alternatively you can pass `block(.n)` or pass a custom vector if control stream has both block and non-block.
#' @importFrom dplyr filter mutate n bind_rows
#' @export
apply_indices <- function(.label_df, .omega = NULL, .sigma = NULL) {
  .theta_df <- .label_df %>% filter(.data[[SUMMARY_PARAM_NAMES]] == "THETA") %>% mutate(param_type = .data[[SUMMARY_PARAM_NAMES]])
  .omega_df <- .label_df %>% filter(.data[[SUMMARY_PARAM_NAMES]] == "OMEGA") %>% mutate(param_type = .data[[SUMMARY_PARAM_NAMES]])
  .sigma_df <- .label_df %>% filter(.data[[SUMMARY_PARAM_NAMES]] == "SIGMA") %>% mutate(param_type = .data[[SUMMARY_PARAM_NAMES]])

  # theta
  .theta_ind <- seq_len(nrow(.theta_df))
  .theta_df <- .theta_df %>% mutate(!!SUMMARY_PARAM_NAMES := paste0(.data[[SUMMARY_PARAM_NAMES]], .theta_ind))

  # omega
  if(!is.null(.omega) && length(.omega) != nrow(.omega_df)) {
    stop(glue("Found {nrow(.omega_df)} parameters for OMEGA but `.omega` argument specifies {length(.omega)}"))
  }
  if(is.null(.omega)) {
    .omega <- rep(TRUE, .omega_df %>% nrow()) # if null assume all are diagonals
  }
  .omega_ind <- build_matrix_indices(.omega)
  .omega_df <- .omega_df %>% mutate(!!SUMMARY_PARAM_NAMES := paste0(.data[[SUMMARY_PARAM_NAMES]], .omega_ind))

  # sigma
  if(!is.null(.sigma) && length(.sigma) != nrow(.sigma_df)) {
    stop(glue("Found {nrow(.sigma_df)} parameters for SIGMA but `.sigma` argument specifies {length(.sigma)}"))
  }
  if(is.null(.sigma)) {
    .sigma <- rep(TRUE, .sigma_df %>% nrow()) # if null assume all are diagonals
  }
  .sigma_ind <- build_matrix_indices(.sigma)
  .sigma_df <- .sigma_df %>% mutate(!!SUMMARY_PARAM_NAMES := paste0(.data[[SUMMARY_PARAM_NAMES]], .sigma_ind))


  return(bind_rows(
    .theta_df,
    .omega_df,
    .sigma_df
  ))
}

#' @rdname apply_indices
#' @param .n The size of the block
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

#' Parse parameter labels
#'
#' Extracts parameter labels from comments in raw ctl string.
#'
#' @details
#' `parse_param_comment()` is heavily modified from tidynm::parse_theta() to parse out the comments into the right columns for all three params.
#'
#' `parse_same_block()` iterates over a list of $OMEGA or $SIGMA blocks and replaces the SAME blocks with a copy of the block they are referring to.
#'
#' @param .x an object from the list that comes out of clean_ctl() (i.e. .ctl_clean$THETA, .ctl_clean$SIGMA, .ctl_clean$OMEGA)
#' @param .theta If `FALSE`, the default, assumes element is an OMEGA or SIGMA and parses `[{}]` to "type" column. If `TRUE`, assumes THETA and parses `[{}]` to unit.
#' @importFrom stringr str_match str_split str_trim str_replace str_replace_all regex
#' @importFrom dplyr mutate
#' @importFrom tidyr replace_na
#' @importFrom purrr map map_df
#' @keywords internal
parse_param_comment <- function(.x, .theta = FALSE){

  if(inherits(.x,'list')) {
    if (!isTRUE(.theta)) {
      # remove spaces between qualifier and parentheses
      .x <- map(.x, function(.l) {
        for (.s in c("DIAGONAL", "BLOCK", "SAME")) {
          .l <- str_replace_all(.l, glue("{.s} +\\("), glue("{.s}\\(")) # remove spaces between qualifier and parentheses
        }
        .l
      })

      # copy SAME blocks
      .x <- parse_same_block(.x)
    }

    # flatten to vector
    .x <- unlist(.x)
  }

  full_label_text <- str_split(.x,'\\;') %>% sapply('[',2) %>% tidyr::replace_na("")

  if (isTRUE(.theta)) {
    final_label_text <- full_label_text
  } else {
    # pull out each param element
    # these are only used to parse _how many_ params there are; the actual values are discarded
    param_text <- str_split(.x,'\\;') %>% sapply('[',1)
    param_text <- str_split(param_text, " +")
    param_text <- suppressSpecificWarning({
      map(param_text, function(.p) {
        #.p <- str_replace(.p, stringr::regex("SAME", ignore_case=TRUE), "1") # replace 'SAME' with a numeric
        .p <- str_replace_all(.p, "\\(|\\)", "") # erase parentheses because they break as.numeric()
        .p <- as.numeric(.p)
        .p <- .p[!is.na(.p)]
        return(.p)
      })
    }, .regexpr = "NAs introduced by coercion")

    # map labels to params, matching only the diagonals if block notation
    if(length(param_text) != length(full_label_text)) {
      stop(paste(
        "parse_param_comment() -- user shouldn't see this error!",
        glue("`length(param_text) != length(full_label_text)` :: {length(param_text)} != {length(full_label_text)}"),
        sep = "\n"))
    }

    .param_label_df <- map_df(seq_along(param_text), function(i) {
      this_param <- param_text[[i]]
      # if the rightmost param on this line, map the label to it, otherwise map empty string
      map_df(seq_along(this_param), function(.p) {
        if (.p == (length(this_param))) {
          this_label <- full_label_text[[i]]
        } else {
          this_label <- ""
        }
        list(param = this_param[[.p]], label = this_label)
      })
    })

    final_label_text <- .param_label_df$label
  }

  # parse out label and unit/type
  label <- gsub('^(.*?)\\]\\s?','',final_label_text)
  unit <- str_match(final_label_text, '\\[(.*?)\\]')

  if (isTRUE(.theta)) {
    out_tbl <- tibble(label = str_trim(label), unit = unit[,ncol(unit)])
  } else {
    out_tbl <- tibble(label = str_trim(label), type = unit[,1]) %>%
      mutate(
        type = ifelse(is.na(.data$type)|!nzchar(.data$type), "[A]", .data$type)
        # ^ from tidynm::param_tbl -- `TYPE = ifelse(is.na(!!(rlang::sym('TYPE')))|!nzchar(!!(rlang::sym('TYPE'))), "[A]", !!(rlang::sym('TYPE')))`
      )
  }

  return(out_tbl)
}

#' @rdname parse_param_comment
#' @importFrom stringr str_replace str_replace_all str_extract regex
#' @keywords internal
parse_same_block <- function(.x) {
  if(!inherits(.x,'list')) {
    warning(paste(
      "User shouldn't see this warning -- `parse_same_block()` only works for list input. Passed .x with class(es):",
      paste(class(.x), collapse = ", ")
      ))
    return(.x)
  }

  .x1 <- list()
  for (.l in .x) {
    # extract SAME if exists and get number
    .l <- str_replace(.l, stringr::regex("SAME(?=\\s|$)", ignore_case=TRUE), "SAME(1)")
    .same <- str_extract(.l, stringr::regex("SAME\\([0-9]+\\)", ignore_case=TRUE))
    .same <- .same[!is.na(.same)]
    if (length(.same) == 1) {
      .same <- as.numeric(str_replace_all(.same, "[^0-9]", ""))
      for (.si in seq_len(.same)) {
        .rep <- .x1[[length(.x1)]]
        .ind <- length(.x1) + 1
        .x1[[.ind]] <- .rep
      }
    } else {
      .ind <- length(.x1) + 1
      .x1[[.ind]] <- .l
    }
  }
  return(.x1)
}

#' Builds matrix indices labels
#' @param .is_diag Logical vector denoting whether each element is a diagonal
#' @keywords internal
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
#' @param x control stream as a character scalar with lines separated by newline characters
#' @importFrom purrr set_names
#' @keywords internal
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

