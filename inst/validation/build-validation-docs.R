#######################################################
## This script runs the test suite and builds
## the validation documents from the test outputs
##
## The script expects the following:
## * Validation stories in mrgvalprep YAML format in "inst/validation/{PKGNAME}-stories.yaml"
## * A standard testthat test suite than can be run with devtools::test()
##
## It will write the validation documents to
## "inst/validation/{PKGNAME}-{PKGVERSION}-validation-docs/")
##
#######################################################

PKGNAME <- "bbr"
PKGVERSION <- "1.4.0"
STYLE_REF_DIR <- "docx-ref-header-image" # set to NULL if not using style ref

# set up directories and clear existing output dirs, if they exist
val_dir <- system.file("validation", package = PKGNAME)
print(val_dir)

style_ref_path <- NULL
if (!is.null(STYLE_REF_DIR)) {
  style_ref_path <- file.path(val_dir, STYLE_REF_DIR)
}

test_dir <- file.path(val_dir, "test_results")
if (fs::dir_exists(test_dir)) fs::dir_delete(test_dir)
fs::dir_create(test_dir)

docs_dir <- file.path(val_dir, paste0(PKGNAME, "-", PKGVERSION, "-validation-docs"))
if (fs::dir_exists(docs_dir)) fs::dir_delete(docs_dir)
fs::dir_create(docs_dir)

# run tests and write res to disk
test_res <- mrgvalprep::parse_testthat_list_reporter(
  devtools::test(Reporter = testthat::ListReporter),
  roll_up_ids = TRUE
)

write.csv(
  test_res,
  file.path(test_dir, paste0(PKGNAME, "-tests.csv"))
)

# capture commit hash and other system info
git_hash <- system("git rev-parse HEAD", intern=TRUE)
Sys.setenv("COMMIT_HASH" = git_hash)

mrgvalprep::get_sys_info(
  out_path = file.path(test_dir, paste0(PKGNAME, "-tests.json")),
  env_vars = c("METWORX_VERSION", "COMMIT_HASH")
)

# read in stories
spec <- mrgvalprep::read_spec_yaml(
  file.path(val_dir, paste0(PKGNAME, "-stories.yaml")),
  file.path(val_dir, paste0(PKGNAME, "-requirements.yaml"))
)

# make docs
mrgvalidate::create_validation_docs(
  PKGNAME,
  PKGVERSION,
  spec,
  auto_test_dir = test_dir,
  output_dir = docs_dir,
  style_dir = style_ref_path
)
