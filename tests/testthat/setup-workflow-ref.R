# constants and helper functions for unit tests. Used by:
# - test-new-model.R
# - test-copy-model-from.R
# - test-modify-model-field.R
# - test-new-model.R
# - test-run-log.R
# - test-summary-log.R
# - test-config-log.R
# - test-model-summaries.R
# - test-utils.R
# - test-get-path-from-object.R
# - test-get-based-on.R
# - maybe others...

# clear any model_directory that is set
set_model_directory(NULL)

# define constants
MOD_ID <- "1"
MODEL_DIR <- "model-examples"
MODEL_DIR_X <- "model-examples-complex"
MOD1_PATH <- file.path(MODEL_DIR, MOD_ID)
MOD1 <- MOD1_PATH %>% read_model()

NEW_MOD2 <- file.path(MODEL_DIR, "2")
NEW_MOD3 <- file.path(MODEL_DIR, "3")

# file names and file paths
OUTPUT_DIR <-     MOD1_PATH
YAML_TEST_FILE <- as.character(glue::glue("{MOD1_PATH}.yaml"))
CTL_TEST_FILE <-  as.character(glue::glue("{MOD1_PATH}.ctl"))
MOD_TEST_FILE <-  as.character(glue::glue("{MOD1_PATH}.mod"))
LST_TEST_FILE <-  as.character(glue::glue("{MOD1_PATH}/{MOD_ID}.lst"))
GRD_TEST_FILE <-  as.character(glue::glue("{MOD1_PATH}/{MOD_ID}.grd"))
EXT_TEST_FILE <-  as.character(glue::glue("{MOD1_PATH}/{MOD_ID}.ext"))
OUTPUT_FILE <-    file.path(MOD1_PATH, "OUTPUT")

LEVEL2_DIR <- file.path(MODEL_DIR, "level2")
LEVEL2_MOD <- file.path(LEVEL2_DIR, "1")

ORIG_DESC <- "original acop model"
NEW_DESC <- "new description"
DESC_IN_CTL <- "PK model 1 cmt base"

ORIG_TAGS <- c("acop tag", "other tag")
NEW_TAGS <- c("new tag 1", "new tag 2")

NEW_TEXT1 <- c("naw", "paw")
NEW_TEXT2 <- c("all", "done")

SUMMARY_REF_FILE <- "data/acop_summary_obj_ref_200910.rds"
PARAM_REF_FILE <- "data/acop_param_table_ref_200818.rds"

MOD_CLASS <- "bbi_nonmem_model"
SUM_CLASS <- "bbi_nonmem_summary"
SL_CLASS <- "bbi_summary_list"
PROC_CLASS <- "babylon_process"
LOG_CLASS <- "bbi_run_log_df"


SUM_CLASS_LIST <- c(SUM_CLASS, "list")
MOD_CLASS_LIST <- c(MOD_CLASS, "list")
PROC_CLASS_LIST <- c(PROC_CLASS, "list")

SUMS_LIST_NAMES_REF <- c("absolute_model_path", "bbi_summary", "error_msg", "needed_fail_flags")

SUM_NAMES_REF <- c("run_details", "run_heuristics", "parameters_data", "parameter_names",
                   "ofv", "condition_number", "shrinkage_details")

NOT_FINISHED_ERR_MSG <- "nonmem_summary.*modeling run has not finished"
NO_LST_ERR_MSG <- "Unable to locate `.lst` file.*NONMEM output folder"

MOD1_ABS_PATH <- file.path(getwd(), tools::file_path_sans_ext(YAML_TEST_FILE))
MOD2_ABS_PATH <- file.path(getwd(), NEW_MOD2)
MOD3_ABS_PATH <- file.path(getwd(), NEW_MOD3)
MOD4_ABS_PATH <- file.path(getwd(), LEVEL2_MOD)

RUN_LOG_ROWS <- 3
RUN_LOG_COLS <- 8
CONFIG_COLS <- 4
SUM_LOG_COLS <- 21

CONFIG_DATA_PATH <- "../../data/acop.csv"
CONFIG_DATA_MD5 <- "4ddb44da897c26681d892aa7be99f74b"
CONFIG_MODEL_MD5 <- "6b930119c4224ba077091b47959b0604"

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
class(REF_LIST_1) <- MOD_CLASS_LIST


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
class(REF_LIST_TMP) <- MOD_CLASS_LIST

ALL_MODS_YAML_MD5 <- c("ee5a30a015c4e09bc29334188ff28b58", "9d689b937fb36e5c3e98e59053b59e73", "b65d8911e9f8007a743e7c93934ce88e", "793cf7a6a04ddcdfc3b6511466a690b3")

#####################
# utils.R constants
#####################

# lists for combining and merging
LIST1 <- list(naw=4, paw=6)
LIST2 <- list(naw=5, saw="hey")

# for combine_directory_path()
ABS_CTL_PATH <- file.path(getwd(), MODEL_DIR, glue::glue("{MOD_ID}.ctl"))
FAKE_CTL_PATH <- file.path(getwd(), MODEL_DIR, CTL_TEST_FILE)

########################
# test helper functions
########################

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

  if (fs::dir_exists(NEW_MOD2)) fs::dir_delete(NEW_MOD2)
  if (fs::dir_exists(NEW_MOD3)) fs::dir_delete(NEW_MOD3)
  if (fs::dir_exists(LEVEL2_DIR)) fs::dir_delete(LEVEL2_DIR)

  # delete model objects from memory
  suppressSpecificWarning(rm(mod1, pos = parent.frame()), .regexpr = "object.+not found")
  suppressSpecificWarning(rm(mod2, pos = parent.frame()), .regexpr = "object.+not found")
  suppressSpecificWarning(rm(mod3, pos = parent.frame()), .regexpr = "object.+not found")
  suppressSpecificWarning(rm(mod4, pos = parent.frame()), .regexpr = "object.+not found")
  suppressSpecificWarning(rm(log_df, pos = parent.frame()),.regexpr = "object.+not found")
}

