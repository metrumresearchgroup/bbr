#' Retrieve and format initial parameter estimates
#'
#' @param .mod a `bbr` model object
#' @param flag_fixed Logical (`TRUE`/`FALSE`). If `TRUE`, return which
#'        parameters are fixed.
#'
#' @details
#' `NA` values indicate that they were not specified in the control stream file.
#' This is true for `THETA` bounds and whether a given parameter is `FIXED` or not.
#'
#' If you would like the `OMEGA` or `SIGMA` records formatted as *full matrices*,
#' they are stored as attributes:
#' ```
#'  initial_est <- initial_estimates(.mod)
#'  attr(initial_est, "omega_mat")
#'  attr(initial_est, "sigma_mat")
#' ```
#'
#' @examples
#' mod1 <- read_model(
#'   system.file("model", "nonmem", "basic", "1", package = "bbr")
#' )
#'
#' initial_estimates(mod1)
#'
#' @export
initial_estimates <- function(.mod, flag_fixed = FALSE){
  initial_est <- get_initial_est(.mod, flag_fixed = flag_fixed)

  # THETA labels
  theta_inits <- initial_est$thetas
  theta_inits$parameter_names <- sprintf("THETA%d", 1:nrow(theta_inits))
  theta_inits <- theta_inits %>%
    dplyr::rename("lower_bound" = "low", "upper_bound" = "up")

  # Format OMEGA
  omega_inits <- matrix_to_df(initial_est$omegas, type = "omega")
  # Append record number
  omega_inits$record_number <- fmt_record_num(initial_est$omegas)
  # Order by row, column (must happen after `fmt_record_num`)
  omega_inits <- omega_inits %>% dplyr::arrange(.data$row, .data$col)
  # Optionally append fix column
  if(isTRUE(flag_fixed)){
    omega_inits <- omega_inits %>% dplyr::left_join(
      matrix_to_df(attr(initial_est$omegas, "nmrec_flags")$fixed, type = "omega") %>%
        dplyr::rename("fixed"="init"),
      by = c("parameter_names", "row", "col")
    )
  }

  # Format SIGMA
  sigma_inits <- matrix_to_df(initial_est$sigmas, type = "sigma")
  # Append record number
  sigma_inits$record_number <- fmt_record_num(initial_est$sigmas)
  # Order by row, column (must happen after `fmt_record_num`)
  sigma_inits <- sigma_inits %>% dplyr::arrange(.data$row, .data$col)
  # Optionally append fix column
  if(isTRUE(flag_fixed)){
    sigma_inits <- sigma_inits %>% dplyr::left_join(
      matrix_to_df(attr(initial_est$sigmas, "nmrec_flags")$fixed, type = "sigma") %>%
        dplyr::rename("fixed"="init"),
      by = c("parameter_names", "row", "col")
    )
  }

  # Combine
  initial_est_df <- dplyr::bind_rows(
    theta_inits %>% dplyr::mutate(record_type = "theta"),
    omega_inits %>% dplyr::mutate(record_type = "omega"),
    sigma_inits %>% dplyr::mutate(record_type = "sigma")
  )  %>% dplyr::select(-c("row", "col")) %>%
    dplyr::relocate("parameter_names") %>%
    dplyr::relocate("record_number", .after = "record_type")

  # Filter out NA values, as these were not specified in the control stream file
  initial_est_df <- initial_est_df %>% dplyr::filter(!is.na(.data$init))

  # Add original matrices as attributes if needed
  attr(initial_est_df, "omega_mat") <- initial_est$omegas
  attr(initial_est_df, "sigma_mat") <- initial_est$sigmas

  return(initial_est_df)
}


#' Retrieve initial parameter estimates
#'
#' @inheritParams initial_estimates
#'
#' @noRd
get_initial_est <- function(.mod, flag_fixed = FALSE){

  test_nmrec_version(.min_version = "0.4.0")
  check_model_object(.mod, "bbi_nonmem_model")

  ctl <- nmrec::read_ctl(get_model_path(.mod))

  if (isTRUE(using_old_priors(ctl))) {
    rlang::warn(
      c(
        "!" = "This model appears to be using $THETA, $OMEGA, and/or $SIGMA to specify priors.",
        "i" = " - That will cause this function to extract and display priors as if they are initial parameter estimates.",
        "i" = " - Consider changing the model to use the more specific prior record names (such as $THETAP and $THETAPV)."
      ))
  }

  # Set flags
  mark_flags <- if(isTRUE(flag_fixed)) "fix" else NULL

  # Get initial estimates
  theta_inits <- get_theta_inits(ctl, mark_flags = mark_flags)
  omega_inits <- nmrec::extract_omega(ctl, mark_flags = mark_flags)
  sigma_inits <- nmrec::extract_sigma(ctl, mark_flags = mark_flags)

  # Get matrix types and options - assign as attributes
  mat_opts <- get_matrix_opts(.mod)
  attr(omega_inits, "mat_opts") <- mat_opts %>% filter(.data$record_type == "omega")
  attr(sigma_inits, "mat_opts") <- mat_opts %>% filter(.data$record_type == "sigma")

  # Format as list
  inits <- list(
    thetas = theta_inits,
    omegas = omega_inits,
    sigmas = sigma_inits
  )

  return(inits)
}


#' Helper function for getting the initial theta values, including bounds and
#' the presence of `FIXED` parameters.
#'
#' @param ctl an `nmrec` ctl object.
#' @param mark_flags 	A vector of NONMEM flags (i.e. valueless options such as
#'   FIXED or SD). For each specified flag, construct a boolean vector (for THETA)
#'   or matrix (for OMEGA and SIGMA) indicating whether the flag is "active" for
#'   the value. Any valid spelling of the flag name is allowed.
#'   See `?nmrec::extract_theta` for more details.
#'
#' @keywords internal
get_theta_inits <- function(ctl, mark_flags = NULL){

  # Tabulate initial values and bounds
  theta_inits <- purrr::map_dfc(c("init", "low", "up"), function(type){
    theta_inits <- nmrec::extract_theta(ctl, type = type)
    tibble::tibble(!!rlang::sym(type) := theta_inits)
  })

  # Tabulate presence of FIXED parameters
  theta_inits_fixed <- nmrec::extract_theta(ctl, mark_flags = mark_flags)
  theta_inits$fixed <- attr(theta_inits_fixed, "nmrec_flags")$fixed

  # Tabulate record number
  theta_inits$record_number <- fmt_record_num(theta_inits_fixed)

  return(theta_inits)
}



#' Format OMEGA or SIGMA matrix as data.frame
#'
#' @param mat a matrix
#' @param type one of `c("omega","sigma")`
#'
#' @noRd
matrix_to_df <- function(mat, type = c("omega","sigma")) {
  type <- toupper(match.arg(type))

  n <- nrow(mat)
  indices <- lower.tri(mat, diag = TRUE)

  row_indices <- row(indices)[indices]
  col_indices <- col(indices)[indices]

  parameter_names <- sprintf(paste0(type,"(%d,%d)"), row_indices, col_indices)

  values <- mat[indices]

  result_df <- tibble::tibble(
    parameter_names = parameter_names,
    init = values,
    row = row_indices,
    col = col_indices
  )

  return(result_df)
}


#' Get options for all matrix-type records
#'
#' Returns a tibble indicating various specifications for matrix-type records.
#'
#' @details
#' This function returns the value types for both diagonal (variance vs
#' standard deviation) and off-diagonal (covariance vs correlation) options. If
#' cholesky decomposition was used, it will show up as under both the diagonal
#' and off diagonal columns. These options are used with `mod_matrix()` to ensure
#' the matrices are in the correct form before checking for positive-definiteness.
#'
#' This function also tabulates `SAME` specifications, matrix classes ("block" vs
#' "diagonal"), and the subtype ('plain', 'vpair', or 'values'). These options
#' are primarily used in `check_and_modify_pd()`, `expand_value_matrix()`, and
#' `cat_mat_diag()`. For more information, see the column definitions below.
#'
#' ### Column Definitions
#'  - **`record_type`**: Either "sigma" or "omega".
#'  - **`record_number`**: Order of occurrence in control stream file.
#'  - **`mat_class`**: Either "block" or "diagonal".
#'  - **`record_size`**: Size of the record. Would be `n` for a `n x n` matrix.
#'  - **`subtype`**: Matrix subtype. Either 'plain', 'vpair', or 'values'.
#'     - `vpair`: Parameter options represent `VALUES(diag,odiag)` pair.
#'     - `values`: Parameter options contain the form `(0.01)x2 0.1`.
#'     - `plain`: Parameter options reflect a typical block or diagonal matrix.
#'  - **`same`**: Whether `'SAME'` was used in the matrix-type record.
#'  - **`same_n`**: The number of times to duplicate the previous record.
#'  - **`param_x`**: Only used when `subtype = 'values'`. Denotes the number of
#'  duplicates for each specified value.
#'     - For example, for the record `'(0.01)x2 0.1'`, `param_x` would be the
#'     vector `c(2, 1)`, since the first value repeats twice.
#'  - **`diag`**: variance vs standard deviation, or cholesky.
#'  - **`off_diag`**: covariance vs correlation, or cholesky.
#'
#' ### Example:
#' ```
#' > get_matrix_opts(.mod)
#' # A tibble: 5 × 10
#' record_type record_number mat_class record_size subtype same  same_n param_x   diag     off_diag
#' <chr>       <chr>         <chr>           <int> <chr>   <lgl> <lgl>  <list>    <chr>    <chr>
#' 1 omega       1             block               2 plain   FALSE NA     <int [3]> variance covariance
#' 2 omega       2             block               2 plain   FALSE NA     <int [3]> standard covariance
#' 3 omega       3             block               2 plain   FALSE NA     <int [3]> standard correlation
#' 4 omega       4             block               2 plain   FALSE NA     <int [3]> variance correlation
#' 5 omega       5             block               2 plain   FALSE NA     <int [3]> cholesky cholesky
#' 6 sigma       1             diagonal            1 plain   FALSE NA     <int [1]> variance covariance
#' ```
#'
#' @inheritParams initial_estimates
#'
#' @keywords internal
get_matrix_opts <- function(.mod){

  check_model_object(.mod, "bbi_nonmem_model")
  ctl <- nmrec::read_ctl(get_model_path(.mod))

  # Function to grab flags
  get_flag_opts <- function(rec){
    purrr::keep(rec$values, function(opt){
      inherits(opt, "nmrec_option_flag") &&
        !inherits(opt, "nmrec_option_record_name")
    })
  }

  # Handling for nested options (diagonal matrix-type records)
  get_nested <- function(rec){
    purrr::keep(rec$values, function(opt){
      inherits(opt, "nmrec_option_nested")
    })
  }

  extract_mat_opts <- function(ctl, type = c("omega", "sigma")){
    type <- match.arg(type)
    recs <- nmrec::select_records(records = ctl, name = type)

    # Matrix classes and subtypes
    mat_specs <- get_matrix_types(recs)

    # Handling if record type doesn't exist
    if(!length(recs)){
      return(
        tibble::tibble(
          record_type = type, record_number = NA, mat_class = NA,
          record_size = NA, "diag" = NA, "off_diag" = NA) %>%
          dplyr::left_join(mat_specs, by = c("record_number", "mat_class")) %>%
          dplyr::relocate(c("diag", "off_diag"), .after = dplyr::everything())
      )
    }

    # Get record sizes
    if(type == "omega"){
      inits <- nmrec::extract_omega(ctl)
    }else{
      inits <- nmrec::extract_sigma(ctl)
    }
    record_sizes <- attr(inits, "nmrec_record_size")

    # Tabulate matrix format (diagonal and off-diagonal options)
    purrr::imap_dfr(recs, function(rec, rec_num){
      mat_class <- mat_specs$mat_class[rec_num]
      if(mat_class == "block"){
        rec_flags <- get_flag_opts(rec)
      }else if(mat_class == "diagonal"){
        rec$parse()
        nested_recs <- get_nested(rec)
        rec_flags <- unlist(purrr::map(nested_recs, get_flag_opts))
      }

      rec_flag_names <- purrr::map_chr(rec_flags, function(rec_flag){
        rec_flag$name
      })

      # If no flags found, return defaults
      if(length(rec_flag_names) == 0){
        mat_opts <- c("diag" = "variance", "off_diag" = "covariance")
      }else{
        # cholesky is applied to the full matrix
        if(any(str_detect(rec_flag_names, "cholesky"))){
          mat_opts <- c("diag" = "cholesky", "off_diag" = "cholesky")
        }else{
          # Handle diagonal
          if(any(str_detect(rec_flag_names, "standard"))){
            mat_opts <- c("diag" = "standard")
          }else{
            mat_opts <- c("diag" = "variance")
          }

          # Handle off-diagonal
          if(any(str_detect(rec_flag_names, "correlation"))){
            mat_opts <- c(mat_opts, "off_diag" = "correlation")
          }else{
            mat_opts <- c(mat_opts, "off_diag" = "covariance")
          }
        }
      }

      tibble::tibble(
        record_type = type, record_number = as.character(rec_num),
        mat_class = mat_class, record_size = record_sizes[rec_num],
        diag = mat_opts[["diag"]], off_diag = mat_opts[["off_diag"]]) %>%
        dplyr::left_join(mat_specs[rec_num, ], by = c("record_number", "mat_class")) %>%
        dplyr::relocate(c("diag", "off_diag"), .after = dplyr::everything())
    })
  }

  bind_rows(
    extract_mat_opts(ctl, "omega"),
    extract_mat_opts(ctl, "sigma")
  )
}


#' Get matrix type for omega and sigma records
#'
#' Returns a vector denoting either "block" or "diagonal" for each record.
#'
#' @param records Either a list of records, or a single `nmrec_record` object
#'
#' @return a vector
#' @noRd
get_matrix_types <- function(records){
  if(!inherits(records, "list")) records <- list(records)

  if(!length(records)){
    return(
      tibble::tibble(
        record_number = NA, mat_class = NA,
        subtype = NA, same = NA, same_n = NA
      )
    )
  }

  purrr::imap_dfr(records, function(rec, rec_num){
    rec$parse()

    # Matrix class
    mat_class <- ifelse(
      is.null(nmrec::get_record_option(rec, "block")),
      "diagonal", "block"
    )


    # Parse `SAME` option if any
    same_lbl <- nmrec::get_record_option(rec, "same")
    same <- ifelse(is.null(same_lbl), FALSE, TRUE)
    if(isTRUE(same)){
      if(inherits(same_lbl, "nmrec_option_value")) {
        same_n <- readr::parse_number(same_lbl$value)
        if(is.na(same_n)) {
          rlang::abort(c("Failed to parse same (n) value.", rec$format()))
        }
      }else{
        same_n <- 1L
      }
    }else{
      same_n <- NA
    }

    # Tabulate matrix subtypes
    popts <- param_options(rec)
    if(matrix_is_vpair(popts)){
      subtype <- "vpair"
    }else if(matrix_has_values(popts)){
      subtype <- "values"
    }else{
      subtype <- "plain"
    }

    tibble::tibble(
      record_number = as.character(rec_num), mat_class = mat_class,
      subtype = subtype, same = same, same_n = same_n,
      param_x = list(purrr::map_int(popts, param_x))
    )
  })
}


#' Get record number of initial estimates
#'
#' @param inits initial estimates object as returned by `nmrec::extract_*`
#'  functions. Should be a vector or matrix with `nmrec_record_size` attributes.
#'
#' @details
#' `attr(inits, "nmrec_record_size")` stores a vector of sizes, one for each
#' occurrence of a given record type.
#' This function creates a vector indicating which record the initial
#' estimate belongs to, and correlates to the order of the initial estimates.
#' - For `THETA` records, this is simply `rep(seq_along(sizes), sizes)`
#' - For `OMEGA` or `SIGMA` records, it's a bit more complicated. Here we need
#' to subset the columns (not the rows) of the full lower triangular matrix to
#' update the `size` to include values created when diagonally concatenating
#' multiple matrix-type records.
#'
#' This function is intended to be used with `matrix_to_df()` for matrix-type
#' records.
#'
#' @returns a vector indicating the which record the initial estimate belongs to
#'
#' @keywords internal
fmt_record_num <- function(inits){
  sizes <- attr(inits, "nmrec_record_size")

  # If matrix, we need to take the full lower triangular matrix into account here,
  # and cannot treat each sub-matrix as a complete matrix, or some off-diagonals
  # will be missing when we go to bind this to the table returned by `matrix_to_df`
  if(inherits(inits, "matrix")){
    full_lower_tri <- lower.tri(inits, diag = TRUE)
    sizes <- purrr::imap_dbl(sizes, function(.x, .y){
      mat_start <- if(.y > 1) sum(sizes[1:(.y-1)]) + 1 else 1
      rec_lower_tri <- full_lower_tri[, mat_start:(mat_start+.x-1)]
      sum(rec_lower_tri)
    })
  }

  rep(seq_along(sizes), sizes)
}


#' Check if *non-specific* priors are being used.
#'
#' The *non-specific* method of priors means using `$THETA` instead of `$THETAPV`
#' to specify `prior` records.
#'
#' @param ctl An `nmrec_ctl_records` object.
#'
#' @keywords internal
using_old_priors <- function(ctl) {
  # pull prior related records
  prior_recs <- nmrec::select_records(ctl, "prior")
  subroutine_recs <- nmrec::select_records(ctl, "sub")

  # Check for PRIOR subroutines
  if(length(subroutine_recs) > 0){
    prior_subs <- purrr::map_lgl(subroutine_recs, function(prior_sub){
      prior_sub_str <- prior_sub$get_lines()
      # filter out comments
      prior_sub_str <- gsub(";.*", "", prior_sub_str)
      # look for priors
      any(stringr::str_detect(prior_sub_str, "(?i)PRIOR"))
    })
    prior_subs <- any(prior_subs)
  }else{
    prior_subs <- FALSE
  }

  # Check if SPECIFIC style is present
  prior_names <- c("thetap", "thetapv", "omegap", "omegapd", "sigmap", "sigmapd")
  specific_priors <- purrr::map(prior_names, function(.x){
    recs <- nmrec::select_records(ctl, .x)
    if(length(recs) == 0) return(NULL) else return(recs)
  })
  specific_priors <- Filter(Negate(is.null), specific_priors)

  # If priors are specified AND _not_ using specific style, then assume using old style
  if ((length(prior_recs) > 0 || isTRUE(prior_subs)) && length(specific_priors) == 0){
    return(TRUE)
  }

  return(FALSE)
}



## The functions below were either taken directly from `nmrec`, or slightly
## adjusted (except `matrix_has_values`).

#' Return all options for initial estimates
#' @noRd
param_options <- function(record) {
  name <- record[["name"]]
  purrr::keep(record$get_options(), function(o) {
    inherits(o, "nmrec_option_nested") && identical(o[["name"]], name)
  })
}

#' Do the parameter options represent VALUES(diag,odiag) pair?
#' @param popts list of options for initial estimates. Output of `param_options()`.
#' @noRd
matrix_is_vpair <- function(popts) {
  length(popts) == 1 &&
    purrr::some(popts[[1]]$values, function(v) {
      inherits(v, "nmrec_option") && v[["name"]] == "values"
    })
}


#' Do the parameter options contain the form (0.01)x2 0.1?
#' @param popts list of options for initial estimates. Output of `param_options()`.
#' @noRd
matrix_has_values <- function(popts){
  lengths <- purrr::map_int(popts, param_x)
  return(any(lengths > 1))
}

#' Get number of inferred values per option.
#'
#' In cases where values are specified like `(0.01)x2 0.1`, we would extract `2`
#' since the value `0.01` is repeated twice.
#' @inheritParams matrix_has_values
#' @noRd
param_x <- function(popt) {
  xopt <- purrr::keep(popt$values, function(x) {
    inherits(x, "nmrec_option") && x[["name"]] == "x"
  })
  n_opts <- length(xopt)
  if(!n_opts) return(1L)

  x <- strtoi(xopt[[1]]$value, base = 10)
  return(x)
}
