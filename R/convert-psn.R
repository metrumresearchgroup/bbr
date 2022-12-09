


#' Convert the output of a PsN run to a bbr model
#'
#' @param .psn_mod_dir Directory containing where the model and table files exist.
#'        Note that the model file must have a `.mod` or `.ctl` file extension.
#' @param .psn_run_dir Directory containing where the model was run (i.e. the generated `modelfit_dir`).
#' @param .bbr_mod_dir Directory to copy the generated bbr model to. If `NULL`, defaults to `.psn_mod_dir`.
#' @param .method How the model was run. Only `'execute'` is currently supported.
#' @inheritParams new_model
#' @param .cleanup_psn logical (T/F). Whether to delete relevant PsN files after the bbr model is created.
#' @param .overwrite logical (T/F). Whether to overwrite files if they already exist.
#'
#'
#' @details
#' `.psn_run_dir` should point to the latest fit (e.g., `modelfit_dir1`, `modelfit_dir2`)
#'
#' @export
convert_psn <- function(.psn_mod_dir,
                        .psn_run_dir,
                        .bbr_mod_dir = NULL,
                        .method = c("execute", "bootstrap", "scm"), # Doesn't do anything at the moment
                        .description = NULL,
                        .based_on = NULL,
                        .tags = NULL,
                        .star = NULL,
                        .cleanup_psn = FALSE,
                        .overwrite = FALSE){

  .method <- match.arg(.method)

  # TODO: Make sure bbi path is set BEFORE copying any files
  # this is needed to create a fake bbi json, and we dont want to
  # copy everything just to fail at the end
  if(rlang::is_empty(bbi_version())){
    stop("Make sure your bbi path is set before running this function.\n`options('bbr.bbi_exe_path' = '/path/to/bbi')`")
  }

  # Normalize paths
  .psn_mod_dir <- normalizePath(.psn_mod_dir)
  .psn_run_dir <- normalizePath(.psn_run_dir)

  # Get model path
  mod_files <- list.files(.psn_mod_dir)
  .mod_file <- mod_files[grep(".mod|.ctl", mod_files)]

  if(length(.mod_file) > 1) stop("Can only have one file with a `.mod` or `.ctl` extension in `.psn_mod_dir`")

  # Make sure file exists and
  current_mod_path <- file.path(.psn_mod_dir, .mod_file)
  checkmate::assert_file_exists(current_mod_path)

  # Create new model location (if specified)
  if(is.null(.bbr_mod_dir)){
    .bbr_mod_dir <- .psn_mod_dir
  }else{
    if(!fs::dir_exists(.bbr_mod_dir)) fs::dir_create(.bbr_mod_dir)
    .bbr_mod_dir <- normalizePath(.bbr_mod_dir)

    # Copy model to new -model- directory - force .ctl extension to normalize data paths in control file
    fs::file_copy(current_mod_path, ctl_ext(file.path(.bbr_mod_dir, .mod_file)), overwrite = .overwrite)
  }

  # Create new run directory
  .bbr_run_dir <- file.path(.bbr_mod_dir, fs::path_ext_remove(.mod_file))
  if(!fs::dir_exists(.bbr_run_dir)) fs::dir_create(.bbr_run_dir)

  # Copy model to new -run- directory - force .ctl extension to normalize data paths in control file
  fs::file_copy(current_mod_path, ctl_ext(file.path(.bbr_run_dir, .mod_file)), overwrite = .overwrite)

  # Model Path
  .mod_path <- ctl_ext(file.path(.bbr_mod_dir, .mod_file))

  # Absolute Model Path (no extension)
  .absolute_mod_path <- sanitize_file_extension(.mod_path)

  # Copy all run files
  psn_run_files <- copy_psn_run_files(.psn_mod_dir, .psn_run_dir, .bbr_run_dir, .overwrite = .overwrite)

  # Copy and rename table files
  table_paths <- copy_psn_tables(.mod_path, .psn_mod_dir, .bbr_run_dir, .overwrite = .overwrite)

  # Get submission attributes
  .psn_info <- yaml::read_yaml(file.path(.psn_run_dir, "meta.yaml"))

  # Create fake PsN bbi_config.json
  json_file <- create_psn_json(.mod_path, .psn_mod_dir, .bbr_run_dir, .psn_info)

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

  if(isTRUE(.cleanup_psn)){
    files_keep <- c(.mod_path,
                    psn_run_files,
                    table_paths,
                    json_file)
    # TODO: remove all other files
    # Figure out what other files we need (besides the ones I already added)
  }

  return(.mod)
}

#' Create a fake bbi_config.json file for a previous PsN model submission
#'
#' @param .mod_path path to a bbr model object
#' @inheritParams copy_psn_run_files
#'
#' @keywords internal
create_psn_json <- function(.mod_path,
                            .psn_mod_dir,
                            .bbr_run_dir,
                            .psn_info){

  # TODO: confirm if we should also change the data path of the ctl file
  data_path <- convert_psn_data_path(.mod_path, .psn_mod_dir, .bbr_run_dir)
  data_path_absolute <- fs::path_abs(file.path(.bbr_run_dir, data_path))

  checkmate::assert_file_exists(data_path_absolute)

  n_threads <- as.numeric(.psn_info$common_options$threads) # confirm this is accurate (cant be)

  # Cant assume they're on metworx, or the system used when the PsN call was run
  # Assign to NONMEM used during PsN evaluation
  nm_name <- basename(.psn_info$NONMEM_directory)
  nm_home <- .psn_info$NONMEM_directory
  nm_call <- strsplit(.psn_info$command_line, " ")[[1]][1]
  nm_list <- list(
    list(
      home = nm_home,
      executable = nm_call
    )
  ) %>% stats::setNames(nm_name)

  configuration_list <- list(
    overwrite = FALSE, # confirm what this is, since you can always overwrite a model
    clean_lvl = 1, # confirm what this is
    git = NULL, # confirm what this is
    bbi_binary = getOption("bbr.bbi_exe_path"),
    save_config = TRUE,
    output_dir = "{{ .Name }}",
    threads = n_threads,
    local = list(create_child_dirs = TRUE),
    nonmem = nm_list,
    parallel = ifelse(n_threads > 1, TRUE, FALSE)
    # Missing a few options that are unobtainable
    # Might want to add one saying PsN was used?
  )

  new_json_list <- list(
    bbi_version = bbi_version(),
    model_name = basename(.mod_path),
    original_model = basename(.mod_path),
    model_path = .mod_path,
    data_path = data_path,
    data_md5 = tools::md5sum(data_path_absolute),
    model_md5 = tools::md5sum(.mod_path),
    model_filename = basename(.bbr_run_dir),
    model_extension = fs::path_ext(.mod_path),
    original_path = dirname(.mod_path),
    output_dir = .bbr_run_dir,
    configuration = configuration_list,
    error = NULL
  )

  new_json_file <- file.path(.bbr_run_dir, "bbi_config.json")
  new_json <- jsonlite::toJSON(new_json_list, pretty=TRUE, auto_unbox = TRUE, null="null")
  readr::write_file(new_json, new_json_file)

  return(new_json_file)
}

#' Convert data path during conversion of PsN to bbr model submission
#'
#' @param .mod_path path to a bbr model object
#' @inheritParams copy_psn_run_files
#'
#' @keywords internal
convert_psn_data_path <- function(.mod_path,
                                  .psn_mod_dir,
                                  .bbr_run_dir){

  # Get Datapath from ctl file and remove any attributes
  data_path <- parse_ctl_to_list(.mod_path)$DATA
  data_path <- strsplit(data_path, " ")[[1]][1]

  # Adjust file path for new bbr run directory
  path_diff <- fs::path_rel(.bbr_run_dir, .psn_mod_dir)
  num_folders <- length(strsplit(path_diff, "/")[[1]])
  if(num_folders != 0){
    if(any(grepl("..", path_diff))){
      # If the difference grew
      path_diff <- paste(rep("../",num_folders), collapse = "")
      data_path <- paste0(path_diff, data_path)
    }else{
      # If the difference shrank
      data_path_parts <- strsplit(data_path, "/")[[1]][-(1:num_folders)]
      data_path <- paste(data_path_parts, collapse = "/")
    }
  }
  return(data_path)
}


#' Copy PsN run files from control from model and run directory
#'
#' @inheritParams convert_psn
#' @param .bbr_run_dir Directory to copy the generated bbr run files to.
#'
#' @details
#' Following a PsN run, some run files are stored in the upper level "model" directory (`.psn_mod_dir`), such
#' as `run.cov`, `run.cor`, etc. However, some important run files only exist in the `NM_run` subdirectory, such as
#' `psn.shk` or `psn.grd`.
#'
#' All these files will be copied to `.bbr_run_dir`, though run files in the `NM_run` subdirectory will be renamed to have the
#' model name be part of the file name.
#'
#' @keywords internal
copy_psn_run_files <- function(.psn_mod_dir,
                               .psn_run_dir,
                               .bbr_run_dir,
                               .overwrite = FALSE){

  exts_to_copy <- c("coi", "cor", "cov", "ext", "lst", "phi", "grd", "shk", "shm", "xml")
  collapsed_exts <- paste(exts_to_copy, collapse = "|")

  main_files <- list.files(.psn_mod_dir, full.names = TRUE)
  main_files_to_copy <- main_files[grep(collapsed_exts, fs::path_ext(main_files))]
  main_exts_copied <- fs::path_ext(main_files_to_copy)

  # Assumption: get the lastest run from the specified fit (cant do in reality)
  run_folders <- list.dirs(.psn_run_dir, recursive = FALSE)
  run_folders <- run_folders[grep("NM_run", run_folders)]
  # TODO: provide handling for multiple runs within a fit (less common, but not uncommon)
  # (e.g., `NM_run1`, `NM_run2`)
  if(length(run_folders) > 1){
    run_folders <- max(run_folders)
    # stop("multiple runs are not yet supported")
  }

  run_files <- list.files(run_folders, full.names = TRUE)
  run_exts_to_copy <- setdiff(exts_to_copy, main_exts_copied)
  collapsed_exts <- paste(run_exts_to_copy, collapse = "|")
  run_files_to_copy <- run_files[grep(collapsed_exts, fs::path_ext(run_files))]

  files_to_copy <- c(main_files_to_copy, run_files_to_copy)

  # Rename run files to correspond to model name
  mod_name <- basename(.bbr_run_dir)
  files_to_rename <- purrr::map_chr(run_files_to_copy, ~{
    file_split <- strsplit(basename(.x), '[.]')[[1]]
    paste(mod_name, file_split[2], sep='.')
  })

  new_file_paths <- file.path(.bbr_run_dir, c(basename(main_files_to_copy), files_to_rename))

  if(!rlang::is_empty(files_to_copy)){
    purrr::iwalk(files_to_copy, ~{
      fs::file_copy(.x, new_file_paths[.y], overwrite = .overwrite)
    })
  }
  return(new_file_paths)
}

#' Get NONMEM table files from control file
#'
#' @param .mod_path path to a bbr model object
#' @inheritParams copy_psn_run_files
#'
#' @returns a vector of file paths
#'
#' @keywords internal
copy_psn_tables <- function(.mod_path,
                            .psn_mod_dir,
                            .bbr_run_dir,
                            .overwrite = FALSE){
  .l <- parse_ctl_to_list(.mod_path)

  # get file names from table statements and construct paths
  table_paths <- .l[grep("^TAB", names(.l))] %>%
    map_chr(~paste(.x, collapse = " ")) %>%
    str_extract("\\bFILE\\s*=\\s*([^ ]+)") %>%
    str_replace("\\bFILE\\s*=\\s*", "") %>%
    str_replace("^\\.\\/", "") %>%
    file.path(.psn_mod_dir, .)

  if(rlang::is_empty(table_paths)){
    return(NULL)
  }else{
    new_tab_names <- purrr::map_chr(table_paths, ~{
      new_tab_name <- ifelse(fs::path_ext(basename(.x)) == "", paste0(basename(.x), ".tab"), basename(.x))
      file.path(.bbr_run_dir, new_tab_name)
    })
    purrr::iwalk(table_paths, ~{
      fs::file_copy(.x, new_tab_names[.y], overwrite = .overwrite)
    })
  }

  return(new_tab_names)
}



