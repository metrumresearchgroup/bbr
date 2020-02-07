check_lst_file <- function(.x) {
  lst_file <- fs::dir_ls(.x, type = "file", glob = "*.lst")
  if (!length(lst_file)) {
    stop("unable to locate lst file in dir: {.x}, cannot proceed...")
  }
  lst_file
}

#' get a model summary
#' @param .path Full path to one of the following:
#'     1) path to a model file in the output directory
#'     2) path to a nonmem output directory
#' @param .args A named list specifying arguments to pass to babylon formatted like `list("nm_version" = "nm74gf_nmfe", "json" = T, "threads" = 4)`. Run `print_nonmem_args()` to see valid arguments.
#' @param ... args passed through to `bbi_exec()`
#' @param .json return output as json
#' @param .dry_run show what the command would be without actually running it
#' @return List of S3 Class "bbi_nonmem_summary" with all summary information
#' @export
nonmem_output <- function(.path,
                                .args = NULL,
                                ...,
                                .dry_run = FALSE
                                ) {

  if (!fs::is_dir(.path)) {
    .path <- dirname(.path)
  }
  # lst file can both be the check for whether the dir is a nonmem output dir
  # and also the output always should be runname.lst so we can determine the model name
  # this is definitely better checking for .mod as there are temporary files with that extension
  # as well
  lst_file_path <- check_lst_file(.path)

  # build command line args
  if (is.null(.args)) {
    .args <- list()
  }
  .args <- purrr::list_modify(.args, json = TRUE)

  args_vec <- check_nonmem_args(.args)
  cmd_args <- c("nonmem", "summary", get_mod_id(lst_file_path), args_vec)

  # if .dry_run return output call
  if (.dry_run) {
    return(list(
      cmd_args = cmd_args,
      wd = .path,
      call = paste(
        "cd", .path, ";",
        getOption("rbabylon.bbi_exe_path"),
        paste(cmd_args, collapse = " ")
      )
    ))
  }

  # otherwise, execute
  res <- bbi_exec(cmd_args, wd = .path, ..., .wait = TRUE)
  get_exit_status(res, .check = TRUE)

  res_list <- get_stdout(res)$output %>%
    paste(collapse="") %>%
    jsonlite::fromJSON(simplifyDataFrame = FALSE)

  attr(res_list, "class") <- "bbi_nonmem_summary"

  return(res_list)
}

