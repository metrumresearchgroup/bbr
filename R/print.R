#' Print methods for bbr objects
#'
#' The various objects defined by `bbr` have their own print methods to
#' allow users to get a quick view of the contents of the object.
#' **When printing a `bbi` object in an `.Rmd` file** that is intended to be
#' knit, consider setting `results = 'asis'` in the chunk options. This
#' will make for prettier formatting, especially of table outputs.
#'
#' @param x Object to format or print.
#' @param ... Other arguments passed on to individual methods.
#'
#' @name print_bbi
NULL

#' @describeIn print_bbi Prints the call made to bbi and whether the process is still running or has finished.
#' @param .call_limit Integer scalar for the max number of characters to print
#'   before truncating the call string. This is compared with the entire length,
#'   but only the positional arguments between the executable path and the first
#'   long option will be truncated.
#' @importFrom stringr str_split str_detect
#' @importFrom fs path_norm
#' @importFrom cli cat_line
#' @export
print.bbi_process <- function(x, ..., .call_limit = 250) {
  exec_path <- x[[PROC_BBI]]
  call_str <- paste(x[[PROC_CMD_ARGS]], collapse = " ")
  len_call <- nchar(exec_path) + nchar(call_str) + 1  # +1 for space

  trunc_marker <- " ... [truncated]"
  # Adjust the .call_limit so that we don't "truncate" just to end up at the
  # same length but with less information.
  .call_limit <- .call_limit - nchar(trunc_marker)

  # truncate model list if too long, keeping flags at the end (if any present)
  if (len_call > .call_limit) {
    split_call_str <- str_split(call_str, " \\-\\-", n = 2)
    mod_str <- split_call_str[[1]][1]
    flag_str <- split_call_str[[1]][2]

    # The length check above looked at unsplit call string. Do a second length
    # check to avoid marking an untruncated string as truncated.
    if (nchar(mod_str) > .call_limit) {
      call_str <- paste0(substr(mod_str, 1, .call_limit), trunc_marker)
      if(!is.na(flag_str)) {
        call_str <- paste0(call_str, " --", flag_str)
      }
    }
  }
  # ... and keeping the executable path at the beginning.
  call_str <- paste(exec_path, call_str)

  # print call string
  cat_line("Running:", col = "green")
  cat(paste0("  ", call_str, "\n"))

  # format and print run directory string
  run_dir <- fs::path_norm(x[[PROC_WD]])
  if (run_dir == ".") {
    run_dir <- getwd()
  }
  cat_line(paste("In", run_dir), col = "green")

  # format and print status string
  if (length(x[[PROC_STDOUT]]) > 1) {
    cat_line("Process finished.", col = "green")
  } else if (x[[PROC_STDOUT]] == "DRY_RUN") {
    cat_line("DRY RUN! Process not actually run.", col = "red")
  } else if (str_detect(x[[PROC_STDOUT]], ".wait = FALSE")) {
    cat_line("Not waiting for process to finish.", col = "blue")
  }

}

#' Print a list of files.
#'
#' [print.bbi_model()] calls this after printing the YAML and model path to
#' allow specific model types to display additional files.
#'
#' @param .mod Model object.
#' @param print_fn A function that this method should call to print each file.
#' @keywords internal
#' @export
print_model_files <- function(.mod, print_fn) {
  UseMethod("print_model_files")
}

#' @export
print_model_files.default <- function(.mod, print_fn) {
  return(invisible(NULL))
}

#' @describeIn print_bbi Prints the information contained in the model object and whether the model has been run
#' @importFrom cli cli_h1 cli_h2 cat_bullet style_italic col_blue col_green col_red cat_rule
#' @importFrom purrr iwalk walk
#' @export
print.bbi_model <- function(x, ...) {

  # make sure a summary object doesn't slip through
  tryCatch(
    check_model_object(x),
    error = function(.e) {
      .error_msg <- paste(as.character(.e$message), collapse = " -- ")
      if (grepl("Must pass a model object", .error_msg, fixed = TRUE)) {
        dev_error(paste("print.bbi_model:", .error_msg))
      } else {
        stop(.e)
      }
    }
  )

  is_valid_print <- function(.x) {
    if (!is.null(.x)) {
      length(.x) != 0
    } else {
      FALSE
    }
  }

  heading <- cli_h1
  subheading <- cli_h2
  bullet_list <- cat_bullet

  if (isTRUE(getOption('knitr.in.progress'))) {
    is_asis <- knitr::opts_current$get("results") == 'asis'

    heading <- function(x) {
      # alphanumeric width = 1, line width = 2
      # w = ncharacters in output to width = 80
      cat('\n')
      if (is_asis) {
        w <- ceiling(nchar(x) + (80 - nchar(x)) / 2)
        cat_rule(x, width = w)
      } else {
        cat_rule(x)
      }
    }

    bullet_list <- subheading <- function(x) {
      if (is_asis) {
        cat("\n")
      }
      cat_bullet(x)
    }
  }

  model_type_status <- cli::style_bold(color_model_type(x, msg = "Status"))
  status <- bbi_nonmem_model_status(x)
  heading(model_type_status)
  subheading(color_status(status))

  heading("Absolute Model Path")
  bullet_list(x[[ABS_MOD_PATH]])

  # Dont print for simulations or bootstrap runs
  if (!inherits(x, c(NMSIM_MOD_CLASS, NMBOOT_MOD_CLASS))) {
    heading("YAML & Model Files")
    bullet_list(get_yaml_path(x, .check_exists = FALSE))
    bullet_list(get_model_path(x, .check_exists = FALSE))
    print_model_files(x, bullet_list)
  }

  # Attach select simulation args to bbi_nonmem_model objects, or print all for
  # bbi_nmsim_model objects
  if (has_simulation(x)) {
    sim_spec <- get_sim_spec(x)
    # Print simulation model status if printing a NM_MOD_CLASS model object
    if (inherits(x, NM_MOD_CLASS)) {
      .sim <- get_simulation(x)
      sim_status <- bbi_nonmem_model_status(.sim)
      heading('Attached Simulation')
      bullet_list(paste('Status:', color_status(sim_status)))
    } else {
      heading('Simulation Args')
      sim_status <- status
    }

    # Print simulation args
    sim_args <- sim_spec[SPEC_NMSIM_KEYS]
    names(sim_args) <- c("Number of Simulations")
    iwalk(sim_args,
          ~ bullet_list(paste0(.y, ": ", col_blue(.x))))
    # If simulation is not run (e.g., the output directory isn't committed by default),
    # include an additional message pointing to how to re-run the simulation
    if(sim_status == "Not Run"){
      msg <- "See 'Re-running existing simulation' section of ?get_simulation"
      cli::cli_alert_info(cli::col_blue(msg))
    }
  }

  if (inherits(x, NMBOOT_MOD_CLASS)) {
    heading('Bootstrap Args')
    boot_spec <- get_boot_spec(x)
    # Spec file doesnt exist until bootstrap run is set up via setup_bootstrap_run
    if(!is.null(boot_spec)){
      boot_args <- boot_spec[SPEC_NMBOOT_KEYS]
      names(boot_args) <- c("Number of runs", "Stratification Columns")
      # strat_cols can be NULL
      boot_args[sapply(boot_args, is.null)] <- NA
      iwalk(boot_args,
            ~ bullet_list(paste0(.y, ": ", col_blue(paste(.x, collapse = ", ")))))
      # Add bullet if cleaned up
      if(isTRUE(bootstrap_is_cleaned_up(x))){
        cli::cat_bullet(paste("Cleaned up:", col_green(TRUE)))
      }
    }else{
      bullet_list(cli::col_red("Not set up"))
    }
  }

  if (is_valid_print(x[[YAML_DESCRIPTION]])) {
    heading('Description')
    bullet_list(style_italic(x[[YAML_DESCRIPTION]]))
  }

  if (is_valid_print(x[[YAML_TAGS]])) {
    heading('Tags')
    walk(x[[YAML_TAGS]], bullet_list)
  }

  if (is_valid_print(x[[YAML_NOTES]])) {
    heading('Notes')
    iwalk(x[[YAML_NOTES]],
          ~ bullet_list(paste0(.y, ": ", style_italic(.x))))
  }

  if (is_valid_print(x[[YAML_BBI_ARGS]])) {
    heading("BBI Args")
    iwalk(x[[YAML_BBI_ARGS]],
          ~ bullet_list(paste0(.y, ": ", col_blue(.x))))
  }
}


#' @describeIn print_bbi Prints a high level summary of a model from a `bbi_nonmem_summary` object
#' @importFrom purrr map_chr
#' @importFrom cli cat_line
#' @importFrom dplyr mutate_if
#' @importFrom checkmate assert_number
#'
#' @param .digits Number of significant digits to use for parameter table. Defaults to 3.
#' @param .fixed If `FALSE`, the default, omits fixed parameters from the parameter table.
#' @param .off_diag If `FALSE`, the default, omits off-diagonals of OMEGA and SIGMA matrices from the parameter table.
#' @param .nrow If `NULL`, the default, print all rows of the parameter table. If `0`, don't print table at all.
#'   Otherwise, prints only `.nrow` rows.
#' @export
print.bbi_nonmem_summary <- function(x, .digits = 3, .fixed = FALSE, .off_diag = FALSE, .nrow = NULL, ...) {

  # print top line info
  .d <- x[[SUMMARY_DETAILS]]
  cat_line(glue("Dataset: {.d$data_set}\n\n"))
  cat_line(glue("Records: {.d$number_of_data_records}\t Observations: {.d$number_of_obs}\t Subjects: {.d$number_of_subjects}\n\n"))

  only_sim <- isTRUE(.d$only_sim)
  if (only_sim) {
    cat_line("No Estimation Methods (ONLYSIM)\n")
  } else {
    cat_line(glue("Objective Function Value (final est. method): {extract_ofv(list(x))}\n\n"))
    cli::cat_line("Estimation Method(s):\n")
    purrr::walk(paste(.d$estimation_method, "\n"), cli::cat_bullet, bullet = "en_dash")
  }

  # Presence of simulation
  if(isTRUE(has_simulation(x))){
    sim_spec <- get_sim_spec(x)
    sim_status <- paste0("Yes (N = ", sim_spec[[SPEC_NMSIM_NSIM]], ")")
  }else{
    sim_status <- "No"
  }
  cli::cat_line(paste('Simulation:', sim_status, "\n"))

  # check heuristics
  .h <- unlist(x[[SUMMARY_HEURISTICS]])
  if (any(.h)) {
    # h_str <- c("**Heuristic Problem(s) Detected:**\n", map_chr(names(which(.h)), ~glue("  - {.x}\n\n")))
    # cat_line(h_str, col = "red")
    #
    cli::cat_line("**Heuristic Problem(s) Detected:**\n", col = "red")
    purrr::walk(paste(names(which(.h)), "\n"), cli::cat_bullet, bullet = "en_dash", col = "red")
  } else {
    cat_line("No Heuristic Problems Detected\n\n")
  }

  if (only_sim) {
    return(invisible(NULL))
  }


  # build parameter table (catch Bayesian error)
  param_df <- tryCatch(
    param_estimates(x),
    error = function(.e) {
      .error_msg <- paste(as.character(.e$message), collapse = " -- ")
      if (grepl(PARAM_BAYES_ERR_MSG, .error_msg, fixed = TRUE)) {
        cat_line(glue("**{PARAM_BAYES_ERR_MSG}**"), col = "red")
        return(NULL)
      } else {
        stop(.e)
      }
    }
  )
  if(is.null(param_df)) {
    return(invisible(NULL))
  }

  if (isFALSE(.fixed)) {
    param_df <- filter(param_df, !.data$fixed)
  }

  if (isFALSE(.off_diag)) {
    param_df <- filter(param_df, is.na(.data$diag) | .data$diag)
  }

  if (!is.null(.nrow)) {
    checkmate::assert_number(.nrow)
    orig_rows <- nrow(param_df)
    param_df <- param_df[1:.nrow, ]
  }

  param_df <- param_df %>%
    select("parameter_names", "estimate", "stderr", "shrinkage") %>%
    mutate_if(is.numeric, sig, .digits = .digits)

  if (requireNamespace("knitr", quietly = TRUE)) {
    param_str <- param_df %>%
      knitr::kable() %>%
      as.character()

    # add color for shrinkage
    param_str <- map_chr(param_str, highlight_cell, .i = 5, .threshold = 30)
  } else {
    param_str <- param_df %>%
      print() %>%
      capture.output()
  }

  if (is.null(.nrow) || .nrow != 0) {
    cat_line(param_str)
    if (!is.null(.nrow)) cat_line(glue("... {orig_rows - .nrow} more rows"), col = "grey")
  }

}


#' @describeIn print_bbi Prints a high level summary of a model from a `bbi_nmboot_summary` object
#' @importFrom purrr map_chr
#' @importFrom cli cat_line
#' @importFrom dplyr mutate_if
#' @importFrom checkmate assert_number
#'
#' @export
print.bbi_nmboot_summary <- function(x, .digits = 3, .nrow = 10, ...) {

  # print top line info
  .d <- x[[SUMMARY_DETAILS]]
  cli_h1("Based on")
  cat_line(paste("Model:", col_blue(x$based_on_model_path)))
  cat_line(paste("Dataset:", col_blue(x$based_on_data_set)))

  # Run specifications (seed, stratification columns, cleaned_up)
  cli_h1("Run Specifications")

  strat_cols <- if(is.null(x$strat_cols)) "None" else paste(x$strat_cols, collapse = ", ")
  names(strat_cols) <- "Stratification Columns"

  seed <- if(is.null(x$seed)) "None" else x$seed
  names(seed) <- "Seed"

  run_specs <- c(strat_cols, seed)
  iwalk(run_specs, ~ cat_bullet(paste0(.y, ": ", col_blue(.x))))

  # Add bullet if cleaned up
  if(isTRUE(bootstrap_is_cleaned_up(x))){
    cli::cat_bullet(paste("Cleaned up:", col_green(TRUE)))
  }

  # Bootstrap run summary (n_samples, any heuristics)
  cli_h1("Bootstrap Run Summary")

  # TODO: confirm this is appropriate for only_sim (unsure where this comes from)
  only_sim <- isTRUE("only_sim" %in% names(.d))
  if (only_sim) {
    cat_line("No Estimation Methods (ONLYSIM)\n")
  } else {
    cli::cat_line("Estimation Method(s):\n")
    purrr::walk(paste(x$estimation_method, "\n"), cli::cat_bullet, bullet = "en_dash")
  }

  cli::cat_line("Run Status:\n")
  n_samples <- c("Number of runs" = x$n_samples)
  cli::cat_bullet(paste("Number of runs:", col_blue(x$n_samples)), bullet = "en_dash")

  # check heuristics
  .h <- x[[SUMMARY_HEURISTICS]]
  heuristics_cols <- names(.h)[!grepl(ABS_MOD_PATH, names(.h))]
  heuristics <- purrr::map_dfr(heuristics_cols, function(col){
    tibble(heuristic = col, any_found = any(.h[[col]]), n_found = sum(.h[[col]]))
  })

  if (any(heuristics$any_found)) {
    heuristics_found <- heuristics$heuristic[which(heuristics$any_found)]
    heuristics_n <- heuristics$n_found[which(heuristics$any_found)]
    heuristics_perc <- round((heuristics_n/n_samples) * 100, 2)
    purrr::walk(
      paste0(heuristics_found, ": ", col_red(heuristics_n)," (", col_red(heuristics_perc), " %)"),
      cli::cat_bullet, bullet = "en_dash"
    )
    cat("\n")
  } else {
    cat_line("\n")
  }

  if (only_sim) {
    return(invisible(NULL))
  }


  # Build parameter comparison table if it exists
  # To avoid printing issues before the comparison is added to the summary object
  # see summarize_bootstrap_run() for details.
  if(!is.null(x$boot_compare)){
    param_df <- x$boot_compare %>% mutate_if(is.numeric, sig, .digits = .digits)

    if (!is.null(.nrow)) {
      checkmate::assert_number(.nrow)
      orig_rows <- nrow(param_df)
      .nrow <- min(.nrow, nrow(param_df))
      param_df <- param_df[1:.nrow, ]
    }

    if (requireNamespace("knitr", quietly = TRUE)) {
      param_str <- param_df %>%
        knitr::kable() %>%
        as.character()
    } else {
      param_str <- param_df %>%
        print() %>%
        capture.output()
    }

    cat_line(param_str)
    if (!is.null(.nrow)) cat_line(glue("... {orig_rows - .nrow} more rows"), col = "grey")
  }
}


#' @describeIn print_bbi Prints the `NM-TRAN` evaluation of a `bbi_nonmem_model`
#'  object
#' @export
print.nmtran_process <- function(x, ...){

  is_valid_print <- function(.x) {
    if (!is.null(.x)) {
      length(.x) != 0
    } else {
      FALSE
    }
  }

  heading <- cli_h1
  subheading <- cli_h2
  bullet_list <- cat_bullet

  status <- x[["status"]]
  if (x[["status_val"]] == 0) {
    status <- col_green(status)
  } else {
    status <- col_red(status)
  }

  heading(cli::col_magenta("NM-TRAN Status"))
  subheading(status)

  heading("Absolute Model Path")
  bullet_list(x[[ABS_MOD_PATH]])

  heading("NM-TRAN Specifications")
  cli::cli_bullets(
    c(
      "*" = "NONMEM Version: {.val {x[['nonmem_version']]}}",
      "*" = "NM-TRAN Executable: {.path {x[['nmtran_exe']]}}",
      "*" = "nmtran_presort: {cli::col_cyan(!is.null(x[['nmtran_presort_exe']]))}"
    )
  )

  # Print run directory if it still exists (clean = FALSE)
  if(fs::dir_exists(x[['run_dir']])){
    cli::cli_bullets(c("*" = "Run Directory: {.path {x[['run_dir']]}}"))
  }

  if (is_valid_print(x[["output"]])) {
    heading('Output')
    cat_line(x[["output"]])
  }
}


#' @describeIn print_bbi Draw model tree as a static plot
#' @param x plot to display
#' @param newpage Logical (T/F). If `TRUE`, draw new (empty) page first.
#' @param vp viewport to draw plot in
#' @param ... other arguments not used by this method
#'
#' @examples
#' \dontrun{
#' pl_tree <- model_tree(MODEL_DIR, static = TRUE)
#'
#' print(pl_tree)
#' print(pl_tree, vp = grid::viewport(width=0.5, height=0.5))
#' print(pl_tree, newpage = TRUE, vp = grid::viewport(width=0.5, height=0.5))
#' }
#' @export
print.model_tree_static <- function(x, newpage = is.null(vp), vp = NULL, ...){
  x_height <- grid::unit(0.85, "npc")
  x_width <- grid::unit(1, "npc")
  if(newpage) grid::grid.newpage()
  if(is.null(vp)){
    grid::grid.raster(x$png_array, height = x_height, width = x_width)
  }else{
    if(is.character(vp)) grid::seekViewport(vp) else grid::pushViewport(vp)
    grid::grid.raster(x$png_array, height = x_height, width = x_width)
    grid::upViewport()
  }
  return(invisible(x))
}

#####################
# INTERNAL HELPERS
#####################

#' Function to color the status. Green for finished; red otherwise.
#' @param status Character string. Either `'Finished Running'`, `'Incomplete Run'`,
#'  or `'Not Run'`.
#' @keywords internal
color_status <- function(status){
  if (status == "Finished Running") {
    status <- cli::col_green(status)
  } else {
    status <- cli::col_red(status)
  }
  return(status)
}

#' Format and color the model type of a model
#'
#' Create colored text denoting the model type for use in print methods.
#' @param .mod a `bbi_base_model` object.
#' @param msg Character string. Appended to the end of the formatted model type
#'  if provided.
#' @keywords internal
color_model_type <- function(.mod, msg = NULL){
  UseMethod("color_model_type")
}

#' @keywords internal
color_model_type.bbi_base_model <- function(.mod, msg = NULL){
  checkmate::assert_string(msg, null.ok = TRUE)
  model_type <- .mod[[YAML_MOD_TYPE]]
  if (model_type == "nonmem") {
    model_type <- cli::col_cyan(paste("NONMEM Model", msg))
  } else if(model_type == "nmsim"){
    model_type <- cli::col_br_magenta(paste("Simulation", msg))
  } else if (model_type == "nmboot"){
    model_type <- cli::col_yellow(paste("Bootstrap Run", msg))
  } else {
    # For bbr.bayes or other bbi_base_models not defined within bbr
    #  - Other packages may implement separate methods rather than relying
    #    on this one
    model_type <- cli::col_cyan(paste(toupper(model_type), "Model", msg))
  }
  return(model_type)
}

# Register private S3 methods for development purposes
.S3method("color_model_type", "bbi_base_model", color_model_type.bbi_base_model)

#' Format digits
#'
#' Simplified version of `pmtables::sig()` for formatting numbers
#' to a specific number of significant digits.
#'
#' @param x numeric, value to manipulate
#' @param digits numeric, number of significant digits
#'
#' @return character vector of formatted values
#'
#' @keywords internal
sig <- function(.x, .digits) {
  namez <- names(.x)

  .x <- .x %>%
    as.numeric() %>%
    suppressSpecificWarning("NAs introduced by coercion") %>%
    formatC(digits = .digits, format = 'g', flag = '#')
  .x <- gsub("\\.$", "", .x)
  .x <- gsub("NA", "", .x)

  names(.x) <- namez

  return(.x)
}


#' Highlight cell in kable table
#'
#' Highlights in red numeric cells that are above the specified threshold.
#'
#' @param .l character scalar of the line to be formatted (a row of a kable table)
#' @param .i the index of the column to check
#' @param .threshold the threshold to check against. If value is greater than
#'   .threshold then it is formatted as red.
#'
#' @return character vector of formatted values
#'
#' @keywords internal
highlight_cell <- function(.l, .i, .threshold) {
  split_l <- unlist(str_split(.l, "\\|"))
  ie1 <- .i-1
  is2 <- .i+1
  ie2 <- length(split_l)

  to_check <- split_l[[.i]]
  check_pad <- stringr::str_extract_all(to_check, " ") %>%
    unlist() %>%
    paste(collapse = "")

  to_check <- to_check %>%
    as.numeric() %>%
    suppressSpecificWarning("NAs introduced by coercion")

  if (is.na(to_check) || to_check <= .threshold) {
    return(.l)
  }

  paste(
    paste(split_l[1:ie1], collapse = '|'),
    paste0(col_red(to_check), check_pad),
    paste(split_l[is2:ie2], collapse = '|'),
    sep = "|"
  )
}

