####################################################
# testing a composable workflow
# This test file actually runs the bbi calls
# and so it must have working version of both bbi and NONMEM.
# Because of this, it is disabled unless on Metworx.
####################################################

if (Sys.getenv("METWORX_VERSION") == "") skip("test-workflow-composable-bbi only runs on Metworx")

# define constants
STARTER_PATH <- "model-examples/acop"

YAML_TEST_FILE <- "model-examples/1.yaml"
NEW_MOD2 <- "model-examples/2"
NEW_MOD3 <- "model-examples/3"

ORIG_DESC <- "original acop model"
NEW_DESC <- "new description"

ORIG_TAGS <- c("acop tag", "other tag")
NEW_TAGS <- c("new tag 1", "new tag 2")

NEW_TEXT1 <- c("naw", "paw")
NEW_TEXT2 <- c("all", "done")


# # cleanup function
# cleanup <- function() {
#
#   # delete original acop output and yaml
#   if (fs::dir_exists(STARTER_PATH)) fs::dir_delete(STARTER_PATH)
#   if (fs::file_exists(yaml_ext(STARTER_PATH))) fs::file_delete(yaml_ext(STARTER_PATH))
#
#   # delete demo models
#   # delete tmp files if they are leftover from previous test
#   for (m in c(NEW_MOD2, NEW_MOD3)) {
#     if (fs::file_exists(yaml_ext(m))) fs::file_delete(yaml_ext(m))
#     if (fs::file_exists(ctl_ext(m))) fs::file_delete(ctl_ext(m))
#     if (fs::dir_exists(m)) fs::dir_delete(m)
#   }
# }
# cleanup()

test_that("naw", { expect_true(1 ==2) })
