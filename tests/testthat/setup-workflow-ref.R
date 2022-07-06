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

# define constants
REF_DIR <- system.file("test-refs",   package = "bbr")
ABS_MODEL_DIR <- system.file("model", "nonmem", "basic",   package = "bbr")

MOD_ID <- "1"
MODEL_DIR <-   fs::path_rel(ABS_MODEL_DIR, getwd()) %>% as.character()
MOD1_PATH <- file.path(MODEL_DIR, MOD_ID)
MOD1 <- read_model(MOD1_PATH)
# if we're on Metworx or Drone, run the summary
if (!Sys.getenv("METWORX_VERSION") == "" || Sys.getenv("DRONE") == "true") {
  withr::with_options(list(bbr.bbi_exe_path = read_bbi_path()), {
    SUM1 <- model_summary(MOD1)
  })
}

NEW_MOD2 <- file.path(MODEL_DIR, "2")
NEW_MOD3 <- file.path(MODEL_DIR, "3")
BATCH_PARAM_TEST_DIR <- file.path(MODEL_DIR, "test_batch_params")

MODEL_DIR_X <- fs::path_rel(system.file("model", "nonmem", "complex",   package = "bbr"), getwd()) %>% as.character()

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

DATA_TEST_FILE <- as.character(fs::path_norm(file.path(REF_DIR, "..", "extdata", "acop.csv")))
DATA_TEST_FIRST_LINE <- "id,time,mdv,evid,dv,amt,sex,wt,etn,num"
DATA_TEST_COLS <- length(unlist(stringr::str_split(DATA_TEST_FIRST_LINE, ",")))
DATA_TEST_ROWS <- 799
DATA_TEST_ROWS_IGNORE <- DATA_TEST_ROWS - 20

TAB_FILE <- "1.tab"
MOD1_TABLE_FILES <- c(TAB_FILE, "1par.tab", "1first1.tab", "1first2.tab", "1first3.tab", "1dups.tab")
TAB_NEW_COLS <- 7
PARTAB_NEW_COLS <- 5

LEVEL2_SUBDIR <- "level2"
LEVEL2_DIR <- file.path(MODEL_DIR, LEVEL2_SUBDIR)
LEVEL2_MOD <- file.path(LEVEL2_DIR, MOD_ID)

ORIG_DESC <- "original acop model"
NEW_DESC <- "new description"
DESC_IN_CTL <- "PK model 1 cmt base"

ORIG_TAGS <- c("acop tag", "other tag")
NEW_TAGS <- c("new tag 1", "new tag 2")

NEW_NOTES <- c("I like it", "Actually I don't like it")
EXTRA_NOTE <- "Oh wait I do like it"

NEW_TEXT1 <- c("naw", "paw")
NEW_TEXT2 <- c("all", "done")

SUMMARY_REF_FILE <- file.path(REF_DIR, "1_summary_obj.R")
PARAM_REF_FILE <-   file.path(REF_DIR, "1_param_table.R")

NM_SUM_CLASS_LIST <- c(NM_SUM_CLASS, BBI_PARENT_CLASS, "list")
NM_MOD_CLASS_LIST <- c(NM_MOD_CLASS, BBI_PARENT_CLASS, "list")
PROC_CLASS_LIST <- c(PROC_CLASS, "list")

PROC_HELP_STR <- as.character(glue("  {read_bbi_path()} --help"))

SUMS_LIST_NAMES_REF <- c("absolute_model_path", "bbi_summary", "error_msg", "needed_fail_flags")

SUM_NAMES_REF <- c("absolute_model_path", "run_details", "run_heuristics", "parameters_data",
                   "parameter_names", "ofv", "condition_number", "shrinkage_details")

NOT_FINISHED_ERR_MSG <- "nonmem_summary.*modeling run has not finished"
NO_LST_ERR_MSG <- "Unable to locate `.lst` file.*NONMEM output folder"

MOD1_ABS_PATH <- fs::path_norm(file.path(getwd(), tools::file_path_sans_ext(YAML_TEST_FILE))) %>% as.character()
MOD2_ABS_PATH <- fs::path_norm(file.path(getwd(), NEW_MOD2)) %>% as.character()
MOD3_ABS_PATH <- fs::path_norm(file.path(getwd(), NEW_MOD3)) %>% as.character()
MOD4_ABS_PATH <- fs::path_norm(file.path(getwd(), LEVEL2_MOD)) %>% as.character()

RUN_LOG_ROWS <- 3L
RUN_LOG_COLS <- 10L
CONFIG_COLS <- 9L
SUM_LOG_COLS <- 23L

ref_json <- jsonlite::fromJSON(system.file("test-refs", "ref_values.json", package = "bbr"))
CONFIG_DATA_PATH <- ref_json$CONFIG_DATA_PATH
CONFIG_DATA_MD5 <- ref_json$CONFIG_DATA_MD5
CONFIG_MODEL_MD5 <- ref_json$CONFIG_MODEL_MD5
MOD_BBI_VERSION <- ref_json$MOD_BBI_VERSION
MOD1_PARAM_COUNT <- ref_json$MOD1_PARAM_COUNT
MOD1_PARAM_COUNT_FIXED <- ref_json$MOD1_PARAM_COUNT_FIXED
MOD1_OFV_REF <- ref_json$MOD1_OFV_REF
MOD_BBI_VERSION <- ref_json$MOD_BBI_VERSION
MOD_NM_VERSION <- ref_json$MOD_NM_VERSION

# yaml md5 hashes
MOD1_YAML_MD5 <- "b5f22ae85c9c0c22405c0e99587f3ed9"
MOD_LEVEL2_MD5 <- "eb3cada879c886a98d30a18f06f14a68"
ALL_MODS_YAML_MD5 <- c(MOD1_YAML_MD5, rep("cd3bd133e23d863f308b64c20e331d33", 2), "7eeb498c50d8ec72d85ec260cba5e40c")
RUN_LOG_YAML_MD5 <- c(MOD1_YAML_MD5, "ac75f5771d66c9b55a1ec68c8789a043", "77525be36ddd665e1508c7ca7541882e")

# model refs

REF_LIST_1 <- list(
  model_type = "nonmem",
  description = ORIG_DESC,
  tags = ORIG_TAGS,
  bbi_args = list(
    overwrite = TRUE,
    threads = 4L,
    parallel = TRUE),
  absolute_model_path = file.path(ABS_MODEL_DIR, "1"),
  yaml_md5 = MOD1_YAML_MD5
)
class(REF_LIST_1) <- NM_MOD_CLASS_LIST


REF_LIST_TMP <- list(
  model_type = "nonmem",
  description = ORIG_DESC,
  tags = ORIG_TAGS,
  bbi_args = list(
    overwrite = TRUE,
    threads = 4L,
    parallel = TRUE),
  absolute_model_path = file.path(ABS_MODEL_DIR, "temp"),
  yaml_md5 = MOD1_YAML_MD5
)
class(REF_LIST_TMP) <- NM_MOD_CLASS_LIST


#####################
# utils.R constants
#####################

# lists for combining and merging
LIST1 <- list(naw=4, paw=6)
LIST2 <- list(naw=5, saw="hey")

# for combine_directory_path()
ABS_CTL_PATH <-  fs::path_norm(file.path(getwd(), MODEL_DIR, glue::glue("{MOD_ID}.ctl"))) %>% as.character()
FAKE_CTL_PATH <- fs::path_norm(file.path(getwd(), MODEL_DIR, CTL_TEST_FILE)) %>% as.character()

########################
# test helper functions
########################

create_all_models <- function() {
  mod1 <- read_model(MOD1_PATH)
  mod2 <- copy_model_from(mod1, basename(NEW_MOD2),   "level 1 copy of 1")
  mod3 <- copy_model_from(mod1, basename(NEW_MOD3),   "level 1 copy of 1")
  fs::dir_create(LEVEL2_DIR)
  mod4 <- copy_model_from(mod2, file.path(LEVEL2_SUBDIR, MOD_ID), "level 2 copy of 2")

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
  copy_model_from(mod1, basename(NEW_MOD2), .add_tags = NEW_TAGS)
  copy_model_from(
    mod1,
    basename(NEW_MOD3),
    .based_on_additional = get_model_id(NEW_MOD2),
    .inherit_tags = TRUE,
    .update_model_file = FALSE
  )
}

clean_test_enviroment <- function(.f = NULL , env = parent.frame())
{
    cleanup(env)
    if(!is.null(.f)) .f()
    withr::defer(cleanup(env), envir = env)

}



cleanup <- function(env = parent.frame()) {
  # delete tmp files if they are leftover from previous test
  mods_to_kill <- purrr::map_chr(c(seq(2,7), "Parent", "Child"), ~ file.path(MODEL_DIR, .x))
  for (m in mods_to_kill) {
    if (fs::file_exists(yaml_ext(m))) fs::file_delete(yaml_ext(m))
    if (fs::file_exists(ctl_ext(m))) fs::file_delete(ctl_ext(m))
  }

  if (fs::dir_exists(NEW_MOD2)) fs::dir_delete(NEW_MOD2)
  if (fs::dir_exists(NEW_MOD3)) fs::dir_delete(NEW_MOD3)
  if (fs::dir_exists(BATCH_PARAM_TEST_DIR)) fs::dir_delete(BATCH_PARAM_TEST_DIR)
  if (fs::dir_exists(LEVEL2_DIR)) fs::dir_delete(LEVEL2_DIR)
  if (file.path(MODEL_DIR, "Parent") %>% dir_exists()) dir_delete(file.path(MODEL_DIR, "Parent"))
  if (file.path(MODEL_DIR, "Child") %>% dir_exists()) dir_delete(file.path(MODEL_DIR, "Child"))

  # delete model objects from memory
  suppressSpecificWarning(rm(mod1, pos = env), .regexpr = "object.+not found")
  suppressSpecificWarning(rm(mod2, pos = env), .regexpr = "object.+not found")
  suppressSpecificWarning(rm(mod3, pos = env), .regexpr = "object.+not found")
  suppressSpecificWarning(rm(mod4, pos = env), .regexpr = "object.+not found")
  suppressSpecificWarning(rm(log_df, pos = env),.regexpr = "object.+not found")
}

#' Temporarily perturb a file
#'
#' Appends a line to the end of a text file, and reverts the change in an
#' environment specified by the caller.
#'
#' @param path string giving the file path
#' @param txt string to temporarily append to file
#' @inheritParams withr::defer
perturb_file <- function(path, txt = "foo", envir = parent.frame()) {
  checkmate::assert_string(path)
  original <- readr::read_file(path)
  readr::write_lines(txt, path, append = TRUE)
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
#' @return The absolute model path to the temporary model.
create_temp_model <- function(path = YAML_TEST_FILE,
                              mod_content = "foo",
                              mod_ext = "ctl",
                              envir = parent.frame(),
                              delete_yaml = TRUE,
                              delete_mod = TRUE
                              ) {
  temp_yaml <- tempfile(fileext = ".yaml")
  fs::file_copy(path, temp_yaml)
  temp_ctl <- fs::path_ext_set(temp_yaml, mod_ext)
  readr::write_file(mod_content, temp_ctl)

  to_delete <- character()
  if (isTRUE(delete_yaml)) to_delete <- c(to_delete, temp_yaml)
  if (isTRUE(delete_mod)) to_delete <- c(to_delete, temp_ctl)

  withr::defer(fs::file_delete(to_delete), envir)
  # normalizePath() needs to be called when the file actually exists
  fs::path_ext_remove(normalizePath(temp_yaml))
}

#' Delete a model object and all of it's accompanying files
cleanup_model <- function(.mod) {
  if (fs::file_exists(get_yaml_path(.mod, .check_exists = FALSE)))  fs::file_delete(get_yaml_path(.mod))
  if (fs::file_exists(get_model_path(.mod, .check_exists = FALSE))) fs::file_delete(get_model_path(.mod))
  if (fs::dir_exists(get_output_dir(.mod, .check_exists = FALSE)))  fs::dir_delete(get_output_dir(.mod))
  rm(.mod)
}


#' helper to copy a control stream and .ext file to
#' BATCH_PARAM_TEST_DIR for testing
#' @param orig_model_path Path that can load a model with `read_model()`
#' @param new_name Name of new model (not full path)
copy_to_batch_params <- function(orig_model_path, new_name) {

  # sanitize new name
  checkmate::assert_string(new_name)
  new_name <- stringr::str_replace_all(new_name, "[^A-Za-z0-9]", "")

  .mod <- read_model(orig_model_path)

  new_dir <- file.path(BATCH_PARAM_TEST_DIR, new_name)
  if (!fs::dir_exists(new_dir)) fs::dir_create(new_dir)

  fs::file_copy(
    get_model_path(.mod),
    ctl_ext(new_dir)
  )

  fs::file_copy(
    build_path_from_model(.mod, ".ext"),
    file.path(new_dir, paste0(new_name, ".ext"))
  )
}
