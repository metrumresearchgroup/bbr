context("creating S3 objects")

# reference
MOD_CLASS <- "bbi_nonmem_model"
PROC_CLASS <- "babylon_process"

test_that("create_model_object() correctly assigns class", {
  .mod <- list()
  .mod[[WORKING_DIR]] <- "naw"
  .mod[[YAML_YAML_NAME]] <- "naw"
  .mod[[YAML_YAML_MD5]] <- "naw"
  .mod[[YAML_MOD_TYPE]] <- "nonmem"
  .mod[[YAML_DESCRIPTION]] <- "naw"
  .mod[[YAML_MOD_PATH]] <- "naw.ctl"
  .mod[[YAML_OUT_DIR]] <- "naw"
  expect_false(MOD_CLASS %in% class(.mod))
  .mod <- create_model_object(.mod)
  expect_true(MOD_CLASS %in% class(.mod))
})

test_that("create_model_object() fails with non-valid model type", {
  .mod <- list()
  .mod[[WORKING_DIR]] <- "naw"
  .mod[[YAML_MOD_TYPE]] <- "naw"
  .mod[[YAML_DESCRIPTION]] <- "naw"
  .mod[[YAML_MOD_PATH]] <- "naw.ctl"
  .mod[[YAML_OUT_DIR]] <- "naw"
  expect_error(create_model_object(.mod), regexp = "Invalid model_type")
})

test_that("create_model_object() fails with non-valid model file extension", {
  .mod <- list()
  .mod[[WORKING_DIR]] <- "naw"
  .mod[[YAML_YAML_NAME]] <- "naw"
  .mod[[YAML_YAML_MD5]] <- "naw"
  .mod[[YAML_MOD_TYPE]] <- "nonmem"
  .mod[[YAML_DESCRIPTION]] <- "naw"
  .mod[[YAML_MOD_PATH]] <- "naw"
  .mod[[YAML_OUT_DIR]] <- "naw"
  expect_error(create_model_object(.mod), regexp = "model_path defined in yaml at naw must have either a .ctl or .mod extension")
})

test_that("create_model_object() errors if keys are missing", {
  .mod <- list()
  .mod[[WORKING_DIR]] <- "naw"
  .mod[[YAML_MOD_TYPE]] <- "naw"
  #.mod[[YAML_DESCRIPTION]] <- "naw"
  expect_error(create_model_object(.mod), regexp = "Model list must have keys")
})

test_that("create_process_object() correctly assigns class", {
  .proc <- list()
  .proc[[PROC_PROCESS]] <- "naw"
  .proc[[PROC_STDOUT]] <- "naw"
  .proc[[PROC_BBI]] <- "naw"
  .proc[[PROC_CMD_ARGS]] <- "naw"
  .proc[[PROC_WD]] <- "naw"
  expect_false(PROC_CLASS %in% class(.proc))
  .proc <- create_process_object(.proc)
  expect_true(PROC_CLASS %in% class(.proc))
})

test_that("create_process_object() errors if keys are missing", {
  .proc <- list()
  .proc[[PROC_PROCESS]] <- "naw"
  .proc[[PROC_STDOUT]] <- "naw"
  .proc[[PROC_BBI]] <- "naw"
  #.proc[[PROC_CMD_ARGS]] <- "naw"
  .proc[[PROC_WD]] <- "naw"
  expect_error(create_process_object(.proc))
})

