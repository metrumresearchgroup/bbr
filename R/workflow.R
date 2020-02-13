
#' Create new model spec from a model yaml
#' @param .yaml_path
#' @importFrom yaml read_yaml
#' @importFrom purrr list_modify
#' @return S3 object of class `bbi_nonmem_spec` that can be passed to `submit_nonmem_model()`
#' @export
create_model <- function(.yaml_path) {
  # read in yaml
  mod_yaml <- parse_mod_yaml(.yaml_path)

  # create spec S3 object
  spec <- list_modify(mod_yaml, yaml_path = .yaml_path)
  class(spec) <- c("bbi_nonmem_spec", class(spec))

  return(spec)
}

#' Create new .mod/ctl and new .yaml files based on a previous model. Used for iterating on model development.
#' Also fills in necessary YAML fields for using `create_run_log()` later.
#' Also fills in necessary YAML fields for using `create_run_log()` later.
#' @param .parent_model Path to model that will be copied WITHOUT FILE EXTENSION. Function will expect to find `{.parent_model}.yaml`` to begin the copy process.
#' @param .new_model Path to write new model files to WITHOUT FILE EXTENSION. Function will create both `{.new_model}.yaml` and `{.new_model}.[mod|ctl]` based on this path.
#' @param .description Description of new model run. This will be stored in the yaml (to be used later in `create_run_log()`) and optionally passed into the `$PROBLEM` of the new control stream.
#' @param .based_on_additional The run id for the `.parent_model` will automatically be added to the `based_on` field but this argument can contain a character scaler or vector of additional run id's (model names) that this model was "based on." These are used to reconstuct model developement and ancestry.
#' @param .add_tags A character scaler or vector with any new tags to be added to `{.new_model}.yaml`
#' @param .inherit_tags Boolean for whether to inherit any tags from `.parent_model.yaml`
#' @param .update_mod_file Boolean for whether to update the `$PROBLEM` line in the new control stream. By default it is TRUE, but if FALSE is passed `{.new_model}.[mod|ctl]` will be an exact copy of it's parent control stream.
#' @importFrom fs file_copy
#' @importFrom readr read_file write_file
#' @importFrom stringr str_replace
#' @importFrom yaml write_yaml
#' @importFrom purrr list_modify
#' @return S3 object of class `bbi_nonmem_spec` that can be passed to `submit_nonmem_model()`
#' @export
copy_model_from <- function(
  .parent_model,
  .new_model,
  .description,
  .based_on_additional = NULL,
  .add_tags = NULL,
  .inherit_tags = TRUE,
  .update_mod_file = TRUE
) {
  # parse yaml of original and copy it
  parent_yaml <- parse_mod_yaml(as.character(glue("{.parent_model}.yaml")))
  new_yaml <- parent_yaml

  # build new model path
  .file_ext <- tools::file_ext(parent_yaml[[YAML_MOD_PATH]])
  new_mod_path <- as.character(glue("{.new_model}.{.file_ext}"))
  new_yaml[[YAML_MOD_PATH]] <- new_mod_path

  # copy control steam to new path
  if (.update_mod_file) {
    # read parent control stream
    mod_str <- read_file(parent_yaml[[YAML_MOD_PATH]])

    # replace the $PROBLEM line(s)
    mod_str <- str_replace(mod_str,
                "\\$PROB(.|\n)*?\\$",
                as.character(glue("$PROBLEM {get_mod_id(.new_model)} {.description}\n\n$")))

    # read parent control stream
    write_file(mod_str, new_mod_path)
  } else {
    fs::file_copy(parent_yaml[[YAML_MOD_PATH]], new_mod_path)
  }

  # fill based_on
  new_yaml[[YAML_BASED_ON]] <- c(get_mod_id(parent_yaml[[YAML_MOD_PATH]]), .based_on_additional)

  # fill description
  new_yaml[[YAML_DESCRIPTION]] <- .description

  # fill tags
  if (.inherit_tags) {
    new_yaml[[YAML_TAGS]] <- c(parent_yaml$tags, .add_tags)
  } else {
    new_yaml[[YAML_TAGS]] <- .add_tags
  }

  # write new_yaml out
  new_yaml_path <- as.character(glue("{.new_model}.yaml"))
  write_yaml(new_yaml, new_yaml_path)

  # create spec S3 object
  spec <- list_modify(new_yaml, yaml_path = new_yaml_path)
  class(spec) <- c("bbi_nonmem_spec", class(spec))

  return(spec)
}


#' Parses model outputs into a
#' @importFrom stringr str_subset
#' @importFrom fs dir_ls
#' @importFrom purrr map map_lgl transpose
#' @importFrom dplyr as_tibble mutate_at mutate select everything
#' @importFrom yaml read_yaml
#' @return tibble with information on each run
#' @export
create_run_log <- function(
  .base_dir,
  .recurse = TRUE
) {
  # get yaml files
  yaml_files <- str_subset(dir_ls(.base_dir, recurse = .recurse), "\\.ya?ml$")

  # read in all candidate yaml's
  all_yaml <- map(yaml_files, function(.x) {read_yaml(.x)})

  # filter to only model yaml's
  mod_yaml_bool <- map_lgl(all_yaml, function(.x) {check_mod_yaml_keys(.x)})
  not_mod <- yaml_files[!mod_yaml_bool]
  if (length(not_mod) > 0) {
    warning(glue("Found {length(not_mod)} YAML files that do not contain required keys for a model YAML. Ignoring the following files: `{paste(not_mod, collapse='`, `')}`"))
  }
  mod_yaml <- all_yaml[which(mod_yaml_bool)]

  # transpose yaml list to tibble
  df <- mod_yaml %>% transpose() %>% as_tibble() %>%
    mutate_at(c(YAML_MOD_PATH, YAML_DESCRIPTION), unlist) %>%
    mutate(run_id = get_mod_id(.data[[YAML_MOD_PATH]])) %>%
    select(run_id, everything())

  # add yaml path
  df$yaml_path <- yaml_files[mod_yaml_bool]

  # potentially add other stuff (optionally?) from bbi summary, etc.

  return(df)
}



