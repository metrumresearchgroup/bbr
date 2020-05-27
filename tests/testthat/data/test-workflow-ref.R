# constants and cleanup function for workflow unit tests. Used by:
# - test-copy-model-from.R
# - test-modify-model-field.R
# - test-new-model.R
# - test-run-log.R


# define constants
MODEL_DIR <- "model-examples"
LEVEL2_DIR <- file.path(MODEL_DIR, "level2")
YAML_TEST_FILE <- file.path(MODEL_DIR, "1.yaml")
NEW_MOD2 <- file.path(MODEL_DIR, "2")
NEW_MOD3 <- file.path(MODEL_DIR, "3")

ORIG_DESC <- "original acop model"
NEW_DESC <- "new description"
DESC_IN_CTL <- "PK model 1 cmt base"

ORIG_TAGS <- c("acop tag", "other tag")
NEW_TAGS <- c("new tag 1", "new tag 2")

NEW_TEXT1 <- c("naw", "paw")
NEW_TEXT2 <- c("all", "done")

MODEL_CLASS_LIST <- c("bbi_nonmem_model", "list")

cleanup <- function() {
  # delete tmp files if they are leftover from previous test
  for (m in c(NEW_MOD2, NEW_MOD3)) {
    if (fs::file_exists(yaml_ext(m))) fs::file_delete(yaml_ext(m))
    if (fs::file_exists(paste0(m, ".yml"))) fs::file_delete(paste0(m, ".yml"))
    if (fs::file_exists(ctl_ext(m))) fs::file_delete(ctl_ext(m))
  }
  if (fs::dir_exists(LEVEL2_DIR)) fs::dir_delete(LEVEL2_DIR)
}
