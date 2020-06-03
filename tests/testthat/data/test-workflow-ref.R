# constants and cleanup function for workflow unit tests. Used by:
# - test-copy-model-from.R
# - test-modify-model-field.R
# - test-new-model.R
# - test-run-log.R


# define constants
MODEL_DIR <- "model-examples"
YAML_TEST_FILE <- file.path(MODEL_DIR, "1.yaml")
NEW_MOD2 <- file.path(MODEL_DIR, "2")
NEW_MOD3 <- file.path(MODEL_DIR, "3")

LEVEL2_DIR <- file.path(MODEL_DIR, "level2")
LEVEL2_MOD <- file.path(LEVEL2_DIR, "1")

ORIG_DESC <- "original acop model"
NEW_DESC <- "new description"
DESC_IN_CTL <- "PK model 1 cmt base"

ORIG_TAGS <- c("acop tag", "other tag")
NEW_TAGS <- c("new tag 1", "new tag 2")

NEW_TEXT1 <- c("naw", "paw")
NEW_TEXT2 <- c("all", "done")

MODEL_CLASS_LIST <- c("bbi_nonmem_model", "list")

MOD1_ABS_PATH <- file.path(getwd(), tools::file_path_sans_ext(YAML_TEST_FILE))
MOD2_ABS_PATH <- file.path(getwd(), NEW_MOD2)
MOD3_ABS_PATH <- file.path(getwd(), NEW_MOD3)
MOD4_ABS_PATH <- file.path(getwd(), LEVEL2_MOD)


# model refs

REF_LIST_1 <- list(
  description = ORIG_DESC,
  model_type = "nonmem",
  tags = ORIG_TAGS,
  bbi_args = list(
    overwrite = TRUE,
    threads = 4L),
  model_working_dir = file.path(getwd(), "model-examples"),
  orig_yaml_file ="1.yaml",
  yaml_md5 = "ee5a30a015c4e09bc29334188ff28b58",
  model_path = "1.ctl",
  output_dir = "1"
)
class(REF_LIST_1) <- MODEL_CLASS_LIST


REF_LIST_TMP <- list(
  description = ORIG_DESC,
  model_type = "nonmem",
  tags = ORIG_TAGS,
  bbi_args = list(
    overwrite = TRUE,
    threads = 4L),
  model_working_dir = file.path(getwd(), "model-examples"),
  orig_yaml_file ="tmp.yml",
  yaml_md5 = "ee5a30a015c4e09bc29334188ff28b58",
  model_path = "tmp.ctl",
  output_dir = "tmp"
)
class(REF_LIST_TMP) <- MODEL_CLASS_LIST

# test helper functions

create_all_models <- function() {
  mod1 <- read_model(YAML_TEST_FILE)
  mod2 <- copy_model_from(mod1, NEW_MOD2,   "level 1 copy of 1")
  mod3 <- copy_model_from(mod1, NEW_MOD3,   "level 1 copy of 1")
  fs::dir_create(LEVEL2_DIR)
  mod4 <- copy_model_from(mod2, LEVEL2_MOD, "level 2 copy of 2")

  # load or create models and assign model objects to global environment
  assign("mod1", mod1, pos = parent.frame())
  assign("mod2", mod2, pos = parent.frame())
  assign("mod3", mod3, pos = parent.frame())
  assign("mod4", mod4, pos = parent.frame())
}

cleanup <- function() {
  # delete tmp files if they are leftover from previous test
  mods_to_kill <- purrr::map_chr(seq(2,7), ~ file.path(MODEL_DIR, .x))
  for (m in mods_to_kill) {
    if (fs::file_exists(yaml_ext(m))) fs::file_delete(yaml_ext(m))
    if (fs::file_exists(paste0(m, ".yml"))) fs::file_delete(paste0(m, ".yml"))
    if (fs::file_exists(ctl_ext(m))) fs::file_delete(ctl_ext(m))
  }
  if (fs::dir_exists(LEVEL2_DIR)) fs::dir_delete(LEVEL2_DIR)

  # delete model objects from memory
  suppressSpecificWarning(rm(mod1, pos = parent.frame()), .regexpr = "object.+not found")
  suppressSpecificWarning(rm(mod2, pos = parent.frame()), .regexpr = "object.+not found")
  suppressSpecificWarning(rm(mod3, pos = parent.frame()), .regexpr = "object.+not found")
  suppressSpecificWarning(rm(mod4, pos = parent.frame()), .regexpr = "object.+not found")
  suppressSpecificWarning(rm(log_df, pos = parent.frame()),.regexpr = "object.+not found")
}
