

psn_dir <- system.file(file.path("model", "nonmem", "psn"), package = "bbr", mustWork = TRUE)
psn_dir <- fs::path_rel(psn_dir, getwd())

psn_type <- "mod_table"

.psn_mod_dir <- file.path(psn_dir, psn_type)
.psn_run_dir <- file.path(psn_dir, psn_type, "modelfit_dir1")
.bbr_mod_dir <- file.path(psn_dir, psn_type, "bbr_test")

#' Convert the output of a PsN run to a bbr model
#'
#' @param .psn_mod_dir Directory containing where the model and table files exist.
#'        Note that the model file must have a `.mod` or `.ctl` file extension.
#' @param .psn_run_dir Directory containing where the model was run (i.e. the generated `modelfit_dir`).
#' @param .bbr_mod_dir Directory to copy the generated bbr model to. If `NULL`, defaults to `.psn_mod_dir`.
#' @param .method How the model was run. Only `'execute'` is currently supported
#' @inheritParams new_model
#' @param .cleanup_psn logical (T/F). Whether to delete relevant PsN files after the bbr model is created
#' @param .overwrite logical (T/F). Whether to overwrite files in `.bbr_mod_dir`
#'
#' @export
convert_psn <- function(.psn_mod_dir,
                        .psn_run_dir,
                        .bbr_mod_dir = NULL,
                        .method = c("execute", "bootstrap", "scm"),
                        .description = NULL,
                        .based_on = NULL,
                        .tags = NULL,
                        .star = NULL,
                        .cleanup_psn = FALSE,
                        .overwrite = FALSE){

  .method <- match.arg(.method)

  # Normalize paths
  .psn_mod_dir <- normalizePath(.psn_mod_dir)
  .psn_run_dir <- normalizePath(.psn_run_dir)

  # Get model path
  mod_files <- list.files(.psn_mod_dir)
  .mod_file <- mod_files[grep(".mod|.ctl", mod_files)]

  # New run folder
  .bbr_run_dir <- fs::path_ext_remove(.mod_file)

  # Make sure file exists and
  current_mod_path <- file.path(.psn_mod_dir, .mod_file)
  checkmate::assert_file_exists(current_mod_path)

  # Get Table files
  table_paths <- get_nonmem_tables(current_mod_path, .psn_mod_dir)

  # Copy files to new location (if specified)
  if(is.null(.bbr_mod_dir)){
    .bbr_mod_dir <- .psn_mod_dir
    .bbr_run_dir <- file.path(.bbr_mod_dir, .bbr_run_dir)
    if(!fs::dir_exists(.bbr_run_dir)) fs::dir_create(.bbr_run_dir)

    # Rename table files
    if(!is.null(table_paths)){
      browser() # Put a temporary stop to avoid accidentally overwriting table files during development
      purrr::walk(table_paths, ~{
        new_tab_name <- ifelse(fs::path_ext(basename(.x)) == "", paste0(basename(.x), ".tab"), basename(.x))
        fs::file_copy(.x, file.path(.bbr_run_dir, new_tab_name), overwrite = .overwrite)
      })
    }
  }else{
    if(!fs::dir_exists(.bbr_mod_dir)) fs::dir_create(.bbr_mod_dir)
    .bbr_mod_dir <- normalizePath(.bbr_mod_dir)
    .bbr_run_dir <- file.path(.bbr_mod_dir, .bbr_run_dir)
    if(!fs::dir_exists(.bbr_run_dir)) fs::dir_create(.bbr_run_dir)

    new_mod_path <- file.path(.bbr_mod_dir, .mod_file)
    fs::file_copy(current_mod_path, new_mod_path, overwrite = .overwrite)

    # Copy table files
    if(!is.null(table_paths)){
      purrr::walk(table_paths, ~{
        new_tab_name <- ifelse(fs::path_ext(basename(.x)) == "", paste0(basename(.x), ".tab"), basename(.x))
        fs::file_copy(.x, file.path(.bbr_run_dir, new_tab_name), overwrite = .overwrite)
      })
    }
  }

  # Model Path
  .mod_path <- file.path(.bbr_mod_dir, .mod_file)

  # Absolute Model Path (no extension)
  .absolute_mod_path <- sanitize_file_extension(.mod_path)

  # create model object
  .mod <- list()
  .mod[[ABS_MOD_PATH]] <- .absolute_mod_path
  .mod[[YAML_MOD_TYPE]] <- "nonmem"
  .mod <- create_model_object(.mod, save_yaml = TRUE)

  # update model from passed args
  if (!is.null(.description)) .mod <- replace_description(.mod, .description)
  if (!is.null(.tags))        .mod <- replace_all_tags(.mod, .tags)
  if (!is.null(.based_on))    .mod <- replace_all_based_on(.mod, .based_on)
  if (isTRUE(.star))          .mod <- add_star(.mod)

  if(.method == "execute"){
    convert_psn_execute(.psn_mod_dir, .psn_run_dir, .bbr_mod_dir)
  }



}


#' Get NONMEM table files from control file
#'
#' @param .mod_path path to a bbr model object
#' @param .mod_dir directory containing table files
#'
#' @keywords internal
get_nonmem_tables <- function(.mod_path, .mod_dir){
  .l <- parse_ctl_to_list(.mod_path)

  # get file names from table statements and construct paths
  .f <- .l[grep("^TAB", names(.l))] %>%
    map_chr(~paste(.x, collapse = " ")) %>%
    str_extract("\\bFILE\\s*=\\s*([^ ]+)") %>%
    str_replace("\\bFILE\\s*=\\s*", "") %>%
    str_replace("^\\.\\/", "") %>%
    file.path(.mod_dir, .)

  if(rlang::is_empty(.f)){
    return(NULL)
  }else{
    return(.f)
  }
}



#' Convert the output of a PsN execute run to a bbr model
#'
#' @inheritParams convert_psn
#'
#' @keywords internal
convert_psn_execute <- function(.psn_mod_dir, .psn_run_dir, .bbr_mod_dir){


}








