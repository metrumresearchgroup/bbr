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
CTL_FILENAME <- ctl_ext(MOD_ID)
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
PARAM_REF_FILE <- "data/acop_param_table_ref_200914.rds"

SUM_CLASS_LIST <- c(SUM_CLASS, "list")
MOD_CLASS_LIST <- c(NM_MOD_CLASS, "list")
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
CONFIG_COLS <- 8L
SUM_LOG_COLS <- 21

CONFIG_DATA_PATH <- "../../data/acop.csv"
CONFIG_DATA_MD5 <- "4ddb44da897c26681d892aa7be99f74b"
CONFIG_MODEL_MD5 <- "6b930119c4224ba077091b47959b0604"

# yaml md5 hashes
MOD1_YAML_MD5 <- "ee5a30a015c4e09bc29334188ff28b58"
MOD_LEVEL2_MD5 <- "1c18961a778fc6b71e3990b500f4bca5"
ALL_MODS_YAML_MD5 <- c(MOD1_YAML_MD5, rep("22c7176b3dbbbf932091c82c2fd9f749", 2), "edd548af82d8556971c442f36a3907e0")
RUN_LOG_YAML_MD5 <- c(MOD1_YAML_MD5, "62fc26bf3bf941c6bad352f23cc6c2ad", "ebd4861c9d0584b03a23edcfdda9a67e")

# model refs

REF_LIST_1 <- list(
  description = ORIG_DESC,
  model_type = "nonmem",
  tags = ORIG_TAGS,
  bbi_args = list(
    overwrite = TRUE,
    threads = 4L),
  absolute_model_path = file.path(getwd(), "model-examples", "1"),
  yaml_md5 = MOD1_YAML_MD5
)
class(REF_LIST_1) <- MOD_CLASS_LIST


REF_LIST_TMP <- list(
  description = ORIG_DESC,
  model_type = "nonmem",
  tags = ORIG_TAGS,
  bbi_args = list(
    overwrite = TRUE,
    threads = 4L),
  absolute_model_path = file.path(getwd(), "model-examples", "temp"),
  yaml_md5 = MOD1_YAML_MD5
)
class(REF_LIST_TMP) <- MOD_CLASS_LIST


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
  mod1 <- read_model(MOD1_PATH)
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

copy_all_output_dirs <- function() {
  if (!fs::dir_exists(LEVEL2_DIR)) { fs::dir_create(LEVEL2_DIR) }
  fs::dir_copy(MOD1_PATH, NEW_MOD2)
  fs::dir_copy(MOD1_PATH, NEW_MOD3)
  fs::dir_copy(MOD1_PATH, LEVEL2_MOD)
}

create_rlg_models <- function() {
  # copy models before creating run log
  mod1 <- read_model(MOD1_PATH)
  copy_model_from(mod1, NEW_MOD2, NEW_DESC, .add_tags = NEW_TAGS)
  copy_model_from(mod1,
                  NEW_MOD3,
                  NEW_DESC,
                  .based_on_additional = get_model_id(NEW_MOD2),
                  .inherit_tags = TRUE,
                  .update_model_file = FALSE)
}

cleanup <- function() {
  # delete tmp files if they are leftover from previous test
  mods_to_kill <- purrr::map_chr(seq(2,7), ~ file.path(MODEL_DIR, .x))
  for (m in mods_to_kill) {
    if (fs::file_exists(yaml_ext(m))) fs::file_delete(yaml_ext(m))
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

#' Temporarily perturb a file
#'
#' Appends a line to the end of a text file, and reverts the change in an
#' environment specified by the caller.
#'
#' @param path string giving the file path
#' @inheritParams withr::defer
perturb_file <- function(path, envir = parent.frame()) {
  checkmate::assert_string(path)
  original <- readr::read_file(path)
  readr::write_lines("foo", path, append = TRUE)
  withr::defer(readr::write_file(original, path), envir)
}

#' Selectively repeat a value
#'
#' It can be useful to create a vector of a particular length with certain
#' elements set to `NA`.
#'
#' @param x The scalar value to repeat.
#' @param i Integer vector of indices that should be `NA`.
#' @param len Non-negative integer giving the desired length of the output
#'   vector.
#'
#' @return A vector of length `len` whose elements are either `x` or `NA`.
rep_missing <- function(x, i, len) {
  checkmate::assert_scalar(x)
  checkmate::assert_integerish(i)
  checkmate::assert_count(len)

  res <- rep(x, len)
  res[i] <- NA
  res
}

#' Create a temporary model
#'
#' It is useful to create a model file and YAML file that can be discarded. The
#' files will be deleted when `envir` exits.
#'
#' @param path The path to the YAML file to copy.
#' @param mod_content A string giving the content of the model file.
#' @param mod_ext The extension for the model file.
#' @inheritParams withr::defer
#'
#' @return The path to the temporary YAML file.
create_temp_model <- function(path = YAML_TEST_FILE,
                              mod_content = "foo",
                              mod_ext = "ctl",
                              envir = parent.frame()) {
  temp_yaml <- tempfile(fileext = ".yaml")
  fs::file_copy(path, temp_yaml)
  temp_ctl <- fs::path_ext_set(temp_yaml, mod_ext)
  readr::write_file(mod_content, temp_ctl)
  withr::defer(fs::file_delete(c(temp_yaml, temp_ctl)), envir)
  temp_yaml
}
