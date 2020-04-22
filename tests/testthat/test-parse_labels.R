#### test outline

skip("This is a fake test right now. More like a sandbox.")


setwd("~/rbabylon/tests/testthat")
library(rbabylon)
library(glue)

source("../../R/parse-labels.R")


MODEL_DIR <- "../../inst/extdata"
options('rbabylon.bbi_exe_path' = '/data/apps/bbi')
set_model_directory(MODEL_DIR)
bbi_init(MODEL_DIR, "/opt/NONMEM", "nm74gf")

MODEL_PICK <- "510" ### 101 triggers fails because of this error https://github.com/metrumresearchgroup/babylon/issues/163

ref_df <- readRDS(glue("data/{MODEL_PICK}_PARAMTBL.rds"))[[1]]

if (fs::file_exists(file.path(MODEL_DIR, glue("{MODEL_PICK}.yaml")))) fs::file_delete(file.path(MODEL_DIR, glue("{MODEL_PICK}.yaml")))
.mod <-  rbabylon::new_model(glue("{MODEL_PICK}.yaml"), glue("the {MODEL_PICK} model"))
.param_df <- .mod %>% rbabylon::model_summary() %>% param_estimates()

#.ctl_raw <- readr::read_file(glue("../../inst/extdata/{MODEL_PICK}.ctl"))
.ctl_path <- glue("../../inst/extdata/{MODEL_PICK}.ctl")


.label_df <- .mod %>% param_labels()
.label_df

# this doesn't work anymore because we don't have the indices in the names yet, so we can't join on them

# # join
# .new_df <- left_join(.param_df, .label_df)
.new_df <- bind_cols(.param_df, .label_df %>% select(-names)) # bind instead, but you have to filter .param_df for diagonals
names(.new_df) <- toupper(names(.new_df))
.new_df


# join against reference to see if they're the same
ref_df %>% full_join(.new_df, by = c("LABEL", "UNIT", "TYPE")) %>% select(PARAM, NAMES, LABEL, UNIT, TYPE, value, ESTIMATE, se, STDERR)



### cleanup
if (fs::file_exists(file.path(MODEL_DIR, "babylon.yaml"))) fs::file_delete(file.path(MODEL_DIR, "babylon.yaml"))
if (fs::file_exists(file.path(MODEL_DIR, glue("{MODEL_PICK}.yaml")))) fs::file_delete(file.path(MODEL_DIR, glue("{MODEL_PICK}.yaml")))
