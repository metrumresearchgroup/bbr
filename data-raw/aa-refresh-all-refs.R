# Re-runs example models and refreshes all reference objects

.proj_root <- rprojroot::find_rstudio_root_file()

source(file.path(.proj_root, "data-raw/run-test-model.R"))
source(file.path(.proj_root, "data-raw/build-read-output-refs.R"))
