
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
#'
#' @return tibble with information on each run
#' @export
create_run_log <- function(
  .base_dir
) {
  return(NULL)
}
