
#' Create new .mod/ctl and new .yaml files based on a previous model. Used for iterating on model development.
#' Also fills in necessary YAML fields for using `create_run_log()` later.
#' @importFrom fs file_copy
#' @importFrom yaml write_yaml
#' @return Character scaler with the name of the model .yaml file that can be passed to `submit_nonmem_model()`
#' @export
copy_model_from <- function(
  .parent_model,
  .new_model,
  .description,
  .based_on_additional = NULL,
  .inherit_tags = T,
  .add_tags = NULL
) {
  # parse yaml of original and copy it
  parent_yaml <- parse_mod_yaml(glue("{.parent_model}.yaml"))
  new_yaml <- parent_yaml

  # build new model path
  .file_ext <- tools::file_ext(parent_yaml[[YAML_MOD_PATH]])
  new_mod_path <- glue("{.new_model}.{.file_ext}")
  new_yaml[[YAML_MOD_PATH]] <- new_mod_path

  # copy control steam to new path
  fs::file_copy(parent_yaml[[YAML_MOD_PATH]], new_mod_path)

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
  write_yaml(new_yaml, glue("{.new_model}.yaml"))
  return(.new_model)
}


#copy_model_from("inst/nonmem/acop", "inst/nonmem/acop2", "new description")
# !!! manually changed model_path from inst/nonmem/acop2.mod to acop2.mod
#setwd("inst/nonmem"); copy_model_from("acop2", "acop3", "new description"); setwd("../..")
#setwd("inst/nonmem"); copy_model_from("acop2", "naw/acop4", "new description", .based_on_additional="acop", .add_tags="naw2"); setwd("../..")


#' Parses model outputs into a
#' @importFrom stringr str_subset
#' @importFrom fs dir_ls
#' @importFrom purrr map
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

  ## I want this, but it won't quite work
  #names(mod_yaml) <- yaml_files[mod_yaml_bool]
  #mod_yaml %>% transpose %>% lapply(unlist, recursive = T) %>% as_tibble
  df <- mod_yaml %>% transpose %>% as_tibble() # this is not quite right. They're all lists
  df$yaml_path <- yaml_files[mod_yaml_bool]

  return(df)
}



