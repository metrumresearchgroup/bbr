
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
  # this is needed to create a fake bbi json, and we dont want to copy everything just to fail at the end
  # is_empty wont capture all cases - revisit how to properly address this concern
  if(rlang::is_empty(bbi_version())){
    stop("Make sure your bbi path is set before running this function.\n`options('bbr.bbi_exe_path' = '/path/to/bbi')`")
  }

  # Normalize paths
  .modelfit_dir <- normalizePath(.modelfit_dir)


  # Find Run folder within fit directory
  trans_res <- read_translation_file(.modelfit_dir)
  mod_files <- unique(names(trans_res))

  if(length(mod_files) == 1){ # this when there's only one .mod file & .psn_model_file isnt provided (we're not sure this can happen)
    .psn_run_dirs <- unlist(unname(trans_res))
    # Assumption: get the -lastest (max) run- from the specified run (e.g., `NM_run2` in c(`NM_run1`, `NM_run2`))
    .psn_run_dir <- .psn_run_dirs[which.max(readr::parse_number(.psn_run_dirs))]
    .psn_run_dir <- file.path(.modelfit_dir, .psn_run_dir)
  }else if(length(mod_files) > 1){
    if (is.null(.psn_model_file)) {
      # Error and ask them to pass .psn_model_file
      mod_files_txt <- paste(mod_files, collapse = ", ")
      stop(glue::glue("More that one model submission was found in {.modelfit_dir}: {mod_files_txt}
                      Please specify `.psn_model_file` to identify which model you're trying to convert"))
    }else{
      .psn_run_dir <- trans_res[[basename(.psn_model_file)]]
      # if this is NULL we'll need to error... hopefully that won't happen but it could (e.g., specifying the wrong model)
      if(is.null(.psn_run_dir)){
        stop(glue::glue("Something went wrong. Make sure {.psn_model_file} corresponds to {.modelfit_dir}"))
      }
      .psn_run_dir <- file.path(.modelfit_dir, .psn_run_dir)
    }

  }else{
    trans_file <- file.path(.modelfit_dir, "model_NMrun_translation.txt")
    stop(glue::glue("The required model translation script, {trans_file}, was not found"))
  }

  # Get submission attributes - model path is updated if provided
  .psn_info <- get_psn_submission_info(.modelfit_dir, .psn_model_file)

  # Get model path
  .psn_mod_path <- .psn_info$model_files
  .mod_name <- basename(.psn_mod_path)

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
  # fs::file_copy(.psn_mod_path, .bbr_mod_run_path, overwrite = .overwrite)

  # Absolute Model Path (no extension)
  .absolute_mod_path <- sanitize_file_extension(.bbr_mod_path)

  # Copy all run files
  psn_run_files <- copy_psn_run_files(.psn_run_dir, .bbr_run_dir, .overwrite = .overwrite)

  # Copy and rename table files
  table_paths <- copy_psn_tables(.psn_mod_path, .psn_run_dir, .bbr_run_dir, .overwrite = .overwrite)

  # Change datapath after copying to new location
  .data_path <- convert_psn_data_path(.psn_mod_path, .bbr_run_dir)

  # Create fake PsN bbi_config.json
  json_file <- create_psn_json(.psn_mod_path, .data_path, .bbr_run_dir, .psn_info)

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
# browser()
  .sub_info <- readLines(.submission_file)
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
    # Check run directory and nearby directory possible locations
    run_dir <- .sub_info[grep("(?i)directory=", .sub_info)]
    run_dir <- parse_option(rm_dash(run_dir))[2]
    mod_name_cmd <- parse_option(.psn_info$command_line, " ")
    mod_name <- mod_name_cmd[grep("(?i)mod|ctl", mod_name_cmd)] # this could be a file path too

    # If run directory exists, check if model file can be found one directory up
    if(fs::dir_exists(run_dir)){
      mod_dir <- fs::path_abs(file.path(run_dir, ".."))
      maybe_mod_path <- file.path(mod_dir, mod_name)
      if(fs::file_exists(maybe_mod_path)){
        .psn_info$model_files <- maybe_mod_path
      }
    }

    # Check directory containing modelfit folder (2 folders up)
    nearby_dir <- dirname(dirname(.submission_file))
    maybe_mod_path <- fs::path_abs(file.path(nearby_dir, mod_name))
    # if(length(maybe_mod_path)>1) browser()
    if(fs::file_exists(maybe_mod_path)){
      .psn_info$model_files <- maybe_mod_path
    }

    # If the model file cant be found and .psn_model_file was not provided, error out
    if(is.null(.psn_info$model_files)){
      msg <- paste0("The version info for ", mod_name, " indicates the model was run at ", run_dir, ". That Directory does not exist on this machine.\n\n",
                   "The nearby directory, ", nearby_dir, " also did not contain the model. Please specify the model path via `.psn_model_file` to proceed.")
      stop(msg)
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
#' @param .mod_path Path to **original** NONMEM control stream file (when ran with PsN).
#' @param .data_path Path to dataset described in $DATA of control stream file.
#' @inheritParams copy_psn_run_files
#'
#' @keywords internal
create_psn_json <- function(.mod_path,
                            .data_path,
                            .bbr_run_dir,
                            .psn_info){

  .bbr_mod_path <- file.path(dirname(.bbr_run_dir), basename(.mod_path))
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
    model_path = .bbr_mod_path,
    data_path = .data_path,
    data_md5 = tools::md5sum(.data_path),
    model_md5 = tools::md5sum(.bbr_mod_path),
    model_filename = basename(.bbr_run_dir),
    model_extension = fs::path_ext(.mod_path),
    original_path = dirname(.mod_path), # where it was copied from (differs from bbr)
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
#' @param .mod_path Path to **original** NONMEM control stream file (when ran with PsN).
#' @inheritParams copy_psn_run_files
#'
#' @keywords internal
convert_psn_data_path <- function(.mod_path,
                                  .bbr_run_dir){

  # Get Datapath from ctl file and remove any attributes
  data_line <- parse_ctl_to_list(.mod_path)$DATA
  data_line_args <- strsplit(data_line, " ")[[1]]
  data_path_rel <- data_line_args[1]

  # Get absolute path
  data_path_absolute <- fs::path_abs(file.path(dirname(.mod_path), data_path_rel))

  # Adjust file path for new bbr run directory
  data_path_mod <- if(fs::path_ext(.mod_path) == "mod"){
    fs::path_rel(data_path_absolute, dirname(.bbr_run_dir))
  }else{
    # ctl extension points to the run directory
    fs::path_rel(data_path_absolute, .bbr_run_dir)
  }

  # Overwrite $DATA block in new model with new path
  .bbr_mod_path <- file.path(dirname(.bbr_run_dir), basename(.mod_path))

  mod_lines <- readLines(.bbr_mod_path) %>%
    suppressSpecificWarning("incomplete final line found")

  # Identify DATA path location and update with new one
  data_line_loc <- which(str_detect(mod_lines, "^\\s*\\$DATA"))
  data_path_loc <- grep("(?i)csv", data_line_args)
  data_line_args[data_path_loc] <- data_path_mod
  new_data_line <- paste("$DATA", paste(data_line_args, collapse = " "))
  mod_lines[data_line_loc] <- new_data_line

  # Overwrite model (mod_lines) to .bbr_mod_path
  writeLines(mod_lines, .bbr_mod_path)

  # If dataset is not found, tell the user that the overwritten path is incorrect
  if(!fs::file_exists(data_path_absolute)){
    warning(glue::glue("Expected to find data at {data_path_absolute}, but no file is there. Please adjust the `$DATA` block in {.bbr_mod_path}"))
  }


  return(data_path_absolute)
}

#' Read `model_NMrun_translation.txt` text file from `NM_run` directory
#'
#' @inheritParams convert_psn
#'
#' @keywords internal
read_translation_file <- function(.modelfit_dir){
  trans_file <- file.path(.modelfit_dir, "model_NMrun_translation.txt")
  trans_res <- readr::read_table(trans_file, col_names = FALSE, show_col_types = FALSE) %>%
    stats::setNames(c("mod", "run_dir"))
  trans_res$run_dir %>% as.list() %>% stats::setNames(trans_res$mod)
}
