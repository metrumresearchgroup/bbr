


#' Convert the output of a PsN run to a bbr model
#'
#' @param .modelfit_dir Directory containing where the model was run (i.e. the generated `modelfit_dir`).
#' @param .bbr_dir Directory to copy the generated bbr model to. If `NULL`, defaults to `.psn_mod_dir`.
#' @param .psn_model_file Model file used during PsN execution. Note that the model file must have a `.mod` or `.ctl` file extension.
#' @inheritParams new_model
#' @param .cleanup_psn Logical (T/F). Whether to delete relevant PsN files after the bbr model is created.
#' @param .overwrite Logical (T/F). Whether to overwrite files if they already exist.
#'
#'
#' @details
#' `.modelfit_dir` should point to the latest fit (e.g., `modelfit_dir1`, `modelfit_dir2`)
#'
#' `.psn_model_file` must be specified if the model was run with `PsN < 4.8.0` and the model was executed
#' on a different machine or in a directory that no longer exists.
#'
#' If PsN > 4.8.0
#'
#' @returns a bbr model object
#'
#' @export
convert_psn <- function(.modelfit_dir,
                        .bbr_dir = dirname(.modelfit_dir),
                        .psn_model_file = NULL,
                        .description = NULL,
                        .based_on = NULL,
                        .tags = NULL,
                        .star = NULL,
                        .cleanup_psn = FALSE,
                        .overwrite = FALSE){


  # TODO: Make sure bbi path is set BEFORE copying any files
  # this is needed to create a fake bbi json, and we dont want to
  # copy everything just to fail at the end
  if(rlang::is_empty(bbi_version())){
    stop("Make sure your bbi path is set before running this function.\n`options('bbr.bbi_exe_path' = '/path/to/bbi')`")
  }

  # Normalize paths
  .modelfit_dir <- normalizePath(.modelfit_dir)

  # Find Run folder within fit directory
  # Assumption: get the -lastest run- from the specified run (e.g., `NM_run1`, `NM_run2`)
  run_folders <- list.dirs(.modelfit_dir, recursive = FALSE)
  .psn_run_dir <- run_folders[grep("NM_run", run_folders)]
  if(length(.psn_run_dir) > 1){
    .psn_run_dir <- .psn_run_dir[which.max(readr::parse_number(basename(.psn_run_dir)))]
  }

  # Get submission attributes - model path is updated if provided
  .psn_info <- get_psn_submission_info(.modelfit_dir, .psn_model_file)

  # Get model path
  .psn_mod_path <- .psn_info$model_files
  .mod_name <- basename(.psn_mod_path)

  # if(!is.null(.psn_model_file)){
  #   .psn_mod_path <- normalizePath(.psn_model_file)
  #   .mod_name <- basename(.psn_mod_path)
  # }else{
  #   mod_files <- .psn_info$model_files
  #   .psn_mod_path <- mod_files[grep(".mod|.ctl", mod_files)]
  #   if(length(.psn_mod_path) > 1) stop("Multiple model files found:\n", paste(.psn_mod_path, collapse = "\n"))
  #   .mod_name <- basename(.psn_mod_path)
  # }

  # Make sure model file exists in directory
  checkmate::assert_file_exists(.psn_mod_path)


  # Create new model location (if specified)
  if(is.null(.bbr_dir)){
    .bbr_dir <- .modelfit_dir
  }else{
    if(!fs::dir_exists(.bbr_dir)) fs::dir_create(.bbr_dir)
    .bbr_dir <- normalizePath(.bbr_dir)

    # Copy model to new -model- directory
    fs::file_copy(.psn_mod_path, file.path(.bbr_dir, .mod_name), overwrite = .overwrite)
  }

  # Create new run directory
  .bbr_run_dir <- file.path(.bbr_dir, fs::path_ext_remove(.mod_name))
  if(!fs::dir_exists(.bbr_run_dir)) fs::dir_create(.bbr_run_dir)

  # New Model Paths (to be referenced by bbr)
  .bbr_mod_path <- file.path(.bbr_dir, .mod_name)
  .bbr_mod_run_path <- file.path(.bbr_run_dir, .mod_name)

  # Copy model to new -run- directory
  fs::file_copy(.psn_mod_path, .bbr_mod_run_path, overwrite = .overwrite)

  # Absolute Model Path (no extension)
  .absolute_mod_path <- sanitize_file_extension(.bbr_mod_path)

  # Copy all run files
  psn_run_files <- copy_psn_run_files(.psn_run_dir, .bbr_run_dir, .overwrite = .overwrite)

  # Copy and rename table files
  table_paths <- copy_psn_tables(.psn_mod_path, .psn_run_dir, .bbr_run_dir, .overwrite = .overwrite)

  # Create fake PsN bbi_config.json
  json_file <- create_psn_json(.psn_mod_path, .bbr_run_dir, .psn_info)

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


  # Cleanup
  # Cant just keep .bbr_dir, as that could also be equivalent to .psn_mod_dir
  # Compile list of specific files to keep
  if(isTRUE(.cleanup_psn)){
    files_keep <- c(.bbr_mod_path,
                    psn_run_files,
                    table_paths,
                    json_file)
    # TODO: remove all other files
    # Figure out what other files we need (besides the ones I already added)
  }

  return(.mod)
}


#' Get PsN execution information
#'
#' Retrieves PsN model execution information from either "version_and_option_info.txt" or "meta.yaml" depending on the version
#' of PsN the user has installed
#'
#' @inheritParams convert_psn
#'
#' @keywords internal
get_psn_submission_info <- function(.modelfit_dir, .psn_model_file = NULL){

  .yaml_file <- file.path(.modelfit_dir, "meta.yaml")

  if(fs::file_exists(.yaml_file)){
    # If PsN version >= 4.8.0
    .psn_info <- yaml::read_yaml(.yaml_file)

    # Overwrite model_files in psn info to .psn_model_file if provided
    # If model was executed on a different machine, the path listed in the yaml is meaningless
    if(!is.null(.psn_model_file)){
      .psn_info$model_files <- normalizePath(.psn_model_file)
    }else{
      .psn_mod_path <- .psn_info$model_files[grep(".mod|.ctl", .psn_info$model_files)]
      if(length(.psn_mod_path) > 1) stop("Multiple model files referenced in ",.yaml_file, ":\n", paste(.psn_mod_path, collapse = "\n"))
      .psn_info$model_files <- .psn_mod_path
    }

    # Ensure model files still exist
    if(!any(fs::file_exists(.psn_info$model_files))){
      stop("The model path does not exist: ", .psn_info$model_files)
    }
  }else{
    # If PsN version <= 4.8.0
    submission_file <- file.path(.modelfit_dir, "version_and_option_info.txt")
    .psn_info <- parse_psn_submission_info(submission_file, .psn_model_file)
  }
  return(.psn_info)
}


#' Parse `version_and_option_info.txt` submission file
#'
#' @param .submission_file file path to a `"version_and_option_info.txt"` file.
#' @inheritParams convert_psn
#'
#' @keywords internal
parse_psn_submission_info <- function(.submission_file, .psn_model_file){

  .sub_info <- readLines(submission_file)
  .psn_info <- list()

  # Helper functions
  rm_dash <- function(txt) sub("-", "", txt) # Remove first dash for readr::parse_number to work
  parse_option <- function(txt, sep = "=") stringr::str_trim(strsplit(txt, sep)[[1]])

  # Get NONMEM installation (2 lines)
  cmd_line <- grep("(?i)nonmem", .sub_info)
  .psn_info$NONMEM_directory <- .sub_info[cmd_line[2]]

  # Get PsN version (1 line)
  cmd_line <- grep("(?i)PsN version", .sub_info)
  .psn_info$PsN_version <- parse_option(.sub_info[cmd_line], sep = ":")[2]

  # Get command (2 lines)
  cmd_line <- grep("(?i)command", .sub_info)
  .psn_info$command_line <- .sub_info[cmd_line+1]

  # Get model file(s)
  if(!is.null(.psn_model_file)){
    .psn_mod_path <- normalizePath(.psn_model_file)
    .psn_info$model_files <- .psn_mod_path
  }else{
    run_dir <- .sub_info[grep("(?i)directory=", .sub_info)]
    run_dir <- parse_option(rm_dash(run_dir))[2]
    mod_name <- parse_option(.psn_info$command_line, " ")[2]
    # Check if run directory still exists
    if(!fs::dir_exists(run_dir)){
      msg <- paste0("The version info for ", mod_name, " indicates the model was run at ", run_dir, ". This Directory does not exist.\n",
                   "If the model was run on another machine, please specify the model path via `.psn_model_file`")
      stop(msg)
    }else{
      # If directory exists, check if model file can be found one directory up
      mod_dir <- fs::path_abs(file.path(run_dir, ".."))
      maybe_mod_path <- file.path(mod_dir, mod_name)
      if(fs::file_exists(maybe_mod_path)){
        .psn_info$model_files <- maybe_mod_path
      }else{
        msg <- paste0("No model file was passed in. Checked for ", mod_name, " in ", mod_dir, ", but no model was found.")
        stop(msg)
      }
    }
  }


  # Get threads
  cmd_lines <- c(grep("Actual values optional PsN \\(common\\) options", .sub_info), length(.sub_info))
  sub_options <- .sub_info[(cmd_lines[1]+1):cmd_lines[2]]
  sub_option_names <- purrr::map_chr(sub_options, ~{parse_option(rm_dash(.x))[1]})

  common_options <- purrr::map(sub_options, ~{
    opt_val <- parse_option(rm_dash(.x))[2]
    if(is.na(suppressWarnings(as.numeric(opt_val)))) opt_val else readr::parse_number(opt_val)
  }) %>% stats::setNames(sub_option_names)

  .psn_info$common_options <- common_options

  return(.psn_info)
}



#' Copy PsN run files from control from model and run directory
#'
#' @inheritParams convert_psn
#' @param .psn_run_dir PsN execution directory where the model was run (e.g., `modelfit_dir/NM_run1`).
#' @param .bbr_run_dir Directory to copy the \strong{generated bbr run files} to. Subdirectory of `.bbr_dir`
#'
#' @details
#' Following a PsN run, some run files are stored in the upper level "model" directory (`.psn_mod_dir`), such
#' as `run.cov`, `run.cor`, etc. However, some important run files only exist in the `NM_run` subdirectory, such as
#' `psn.shk` or `psn.grd`.
#'
#' All these files exist in `NM_run`, so we only need that directory. These files will be copied to `.bbr_run_dir`,
#' and will be renamed to have the model name be part of the file path.
#'
#' @keywords internal
copy_psn_run_files <- function(.psn_run_dir,
                               .bbr_run_dir,
                               .overwrite = FALSE){

  exts_to_copy <- c("coi", "cor", "cov", "ext", "lst", "phi", "grd", "shk", "shm", "xml")
  collapsed_exts <- paste(exts_to_copy, collapse = "|")

  run_files <- list.files(.psn_run_dir, full.names = TRUE)
  files_to_copy <- run_files[grep(collapsed_exts, fs::path_ext(run_files))]

  # Rename run files to correspond to model name
  mod_name <- basename(.bbr_run_dir)
  files_to_rename <- purrr::map_chr(files_to_copy, ~{
    file_split <- strsplit(basename(.x), '[.]')[[1]]
    paste(mod_name, file_split[2], sep='.')
  })

  new_file_paths <- file.path(.bbr_run_dir, files_to_rename)

  if(!rlang::is_empty(files_to_copy)){
    purrr::iwalk(files_to_copy, ~{
      fs::file_copy(.x, new_file_paths[.y], overwrite = .overwrite)
    })
  }
  return(new_file_paths)
}



#' Get NONMEM table files from control file
#'
#' @param .mod_path Path to a NONMEM control stream file.
#' @inheritParams copy_psn_run_files
#'
#' @returns a vector of file paths
#'
#' @keywords internal
copy_psn_tables <- function(.mod_path,
                            .psn_run_dir,
                            .bbr_run_dir,
                            .overwrite = FALSE){
  .l <- parse_ctl_to_list(.mod_path)

  # get file names from table statements and construct paths
  table_paths <- .l[grep("^TAB", names(.l))] %>%
    map_chr(~paste(.x, collapse = " ")) %>%
    str_extract("\\bFILE\\s*=\\s*([^ ]+)") %>%
    str_replace("\\bFILE\\s*=\\s*", "") %>%
    str_replace("^\\.\\/", "") %>%
    file.path(.psn_run_dir, .)

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



#' Create a fake bbi_config.json file for a previous PsN model submission
#'
#' @param .mod_path Path to a NONMEM control stream file.
#' @inheritParams copy_psn_run_files
#'
#' @keywords internal
create_psn_json <- function(.mod_path,
                            .bbr_run_dir,
                            .psn_info){

  # Change datapath after copying to new location
  data_path <- convert_psn_data_path(.mod_path, .bbr_run_dir)
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
#' @param .mod_path Path to a NONMEM control stream file.
#' @inheritParams copy_psn_run_files
#'
#' @keywords internal
convert_psn_data_path <- function(.mod_path,
                                  .bbr_run_dir){

  # Get Datapath from ctl file and remove any attributes
  data_path <- parse_ctl_to_list(.mod_path)$DATA
  data_path <- strsplit(data_path, " ")[[1]][1]

  # Adjust file path for new bbr run directory
  .psn_mod_dir <- dirname(.mod_path)
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

