# reference character vectors for checking the contents of output files

# lists for combining and merging
LIST1 <- list(naw=4, paw=6)
LIST2 <- list(naw=5, saw="hey")

# file names and file paths
MOD_ID <- "1"
MODEL_DIR <- "model-examples"
OUTPUT_DIR <-     as.character(glue::glue("{MODEL_DIR}/{MOD_ID}"))
YAML_TEST_FILE <- as.character(glue::glue("{MODEL_DIR}/{MOD_ID}.yaml"))
CTL_TEST_FILE <-  as.character(glue::glue("{MODEL_DIR}/{MOD_ID}.ctl"))
MOD_TEST_FILE <-  as.character(glue::glue("{MODEL_DIR}/{MOD_ID}.mod"))
LST_TEST_FILE <-  as.character(glue::glue("{MODEL_DIR}/{MOD_ID}/{MOD_ID}.lst"))
GRD_TEST_FILE <-  as.character(glue::glue("{MODEL_DIR}/{MOD_ID}/{MOD_ID}.grd"))
EXT_TEST_FILE <-  as.character(glue::glue("{MODEL_DIR}/{MOD_ID}/{MOD_ID}.ext"))

# for combine_directory_path()
ABS_CTL_PATH <- file.path(getwd(), MODEL_DIR, glue::glue("{MOD_ID}.ctl"))
FAKE_CTL_PATH <- file.path(getwd(), MODEL_DIR, CTL_TEST_FILE)

# fake result object
MOD1 <- read_model(YAML_TEST_FILE, .directory = NULL)

