skip_if_not_ci_or_metworx("dofv")

withr::local_options(list(bbr.bbi_exe_path = read_bbi_path()))

test_that("add_dofv: default based_on reference model", {
  ofvs <- c(10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0, 100.0)
  tdir <- local_model_dir_nonmem_dummies(
    list(
      list(
        subpath = "01",
        data_set = "../data.csv",
        final_est_method = "First Order Conditional Estimation with Interaction",
        number_of_obs = 800,
        ofv_no_constant = ofvs[1]
      ),
      list(
        subpath = "02",
        based_on = "01",
        data_set = "../data.csv",
        final_est_method = "First Order Conditional Estimation with Interaction",
        number_of_obs = 800,
        ofv_no_constant = ofvs[2]
      ),
      list(
        # First based_on item is used as reference.
        subpath = "03",
        based_on = c("01", "02"),
        data_set = "../data.csv",
        final_est_method = "First Order Conditional Estimation with Interaction",
        number_of_obs = 800,
        ofv_no_constant = ofvs[3]
      ),
      list(
        # Second-level directory works.
        subpath = "sub/04",
        based_on = "../03",
        data_set = "../../data.csv",
        final_est_method = "First Order Conditional Estimation with Interaction",
        number_of_obs = 800,
        ofv_no_constant = ofvs[4]
      ),
      list(
        # dofv is set to NA due to different number_of_obs.
        subpath = "05",
        based_on = "03",
        data_set = "../data.csv",
        final_est_method = "First Order Conditional Estimation with Interaction",
        number_of_obs = 900,
        ofv_no_constant = ofvs[5]
      ),
      list(
        # dofv is set to NA due to different data path.
        subpath = "06",
        based_on = "03",
        data_set = "../foo.csv",
        final_est_method = "First Order Conditional Estimation with Interaction",
        number_of_obs = 800,
        ofv_no_constant = ofvs[6]
      ),
      list(
        # dofv is set to NA due to different final method.
        subpath = "07",
        based_on = "03",
        data_set = "../data.csv",
        final_est_method = "Laplacian Conditional Estimation (Centered)",
        ofv_no_constant = ofvs[7]
      ),
      list(
        # dofv is non-NA for matching methods.
        subpath = "08",
        based_on = "07",
        data_set = "../data.csv",
        final_est_method = "Laplacian Conditional Estimation (Centered)",
        ofv_no_constant = ofvs[8]
      ),
      list(
        # dofv is NA due unsupported method, plus the method doesn't match the
        # base's.
        subpath = "09",
        based_on = "03",
        data_set = "../data.csv",
        final_est_method = "Importance Sampling of Variance-Covariance (SIR)",
        ofv_no_constant = ofvs[9]
      ),
      list(
        # dofv is set to NA due to unsupported method, despite base's matching
        # method.
        subpath = "10",
        based_on = "09",
        data_set = "../data.csv",
        final_est_method = "Importance Sampling of Variance-Covariance (SIR)",
        ofv_no_constant = ofvs[10]
      )
    )
  )

  # Run log input

  rlog <- run_log(tdir, .recurse = TRUE)
  # Sort so that these tests don't assume run_log's output order.
  rlog <- dplyr::arrange(rlog, .data[[RUN_ID_COL]])

  res_rlog <- add_dofv(rlog)
  expect_equal(res_rlog[[OFV_COL]], ofvs)
  expect_equal(
    res_rlog[[DOFV_COL]],
    c(
      NA,
      ofvs[2] - ofvs[1],
      ofvs[3] - ofvs[1],
      ofvs[4] - ofvs[3],
      NA,
      NA,
      NA,
      ofvs[8] - ofvs[7],
      NA,
      NA
    )
  )

  # Summary log input

  slog <- add_summary(rlog)

  res_slog <- add_dofv(slog)
  cols <- c(ABS_MOD_PATH, OFV_COL, DOFV_COL)
  for (col in cols) {
    expect_equal(res_rlog[[!!col]], res_slog[[!!col]])
  }

  # Filtered log

  rlog_filtered <- dplyr::filter(
    rlog,
    .data[[RUN_ID_COL]] %in% c("02", "03", "04")
  )

  res_rlog <- add_dofv(rlog_filtered)
  expect_equal(
    res_rlog[[OFV_COL]],
    c(ofvs[2], ofvs[3], ofvs[4])
  )
  expect_equal(
    res_rlog[[DOFV_COL]],
    c(
      ofvs[2] - ofvs[1],
      ofvs[3] - ofvs[1],
      ofvs[4] - ofvs[3]
    )
  )

  res_slog <- add_dofv(add_summary(rlog_filtered))
  for (col in cols) {
    expect_equal(res_rlog[[!!col]], res_slog[[!!col]])
  }
})

test_that("add_dofv: single reference model", {
  ofvs <- c(10.0, 20.0, 30.0, 40.0)
  tdir <- local_model_dir_nonmem_dummies(
    list(
      list(
        subpath = "main/01",
        data_set = "../../data.csv",
        final_est_method = "First Order Conditional Estimation with Interaction",
        number_of_obs = 800,
        ofv_no_constant = ofvs[1]
      ),
      list(
        subpath = "main/02",
        based_on = "01",
        data_set = "../../data.csv",
        final_est_method = "First Order Conditional Estimation with Interaction",
        number_of_obs = 800,
        ofv_no_constant = ofvs[2]
      ),
      list(
        subpath = "main/03",
        based_on = "02",
        data_set = "../../data.csv",
        final_est_method = "First Order Conditional Estimation with Interaction",
        number_of_obs = 800,
        ofv_no_constant = ofvs[3]
      ),
      list(
        subpath = "other/04",
        based_on = "../main/03",
        data_set = "../../data.csv",
        final_est_method = "First Order Conditional Estimation with Interaction",
        number_of_obs = 800,
        ofv_no_constant = ofvs[4]
      )
    )
  )

  rlog <- run_log(file.path(tdir, "main"))
  rlog <- dplyr::arrange(rlog, .data[[RUN_ID_COL]])

  # Reference model that's included in run log
  mod2 <- read_model(file.path(tdir, "main", "02"))
  res <- add_dofv(rlog, mod2)

  expect_equal(
    res[[OFV_COL]],
    c(ofvs[1], ofvs[2], ofvs[3])
  )
  expect_equal(
    res[[DOFV_COL]],
    c(
      ofvs[1] - ofvs[2],
      ofvs[2] - ofvs[2],
      ofvs[3] - ofvs[2]
    )
  )

  # Reference model from outside run log
  mod4 <- read_model(file.path(tdir, "other", "04"))
  res <- add_dofv(rlog, mod4)

  expect_equal(
    res[[OFV_COL]],
    c(ofvs[1], ofvs[2], ofvs[3])
  )
  expect_equal(
    res[[DOFV_COL]],
    c(
      ofvs[1] - ofvs[4],
      ofvs[2] - ofvs[4],
      ofvs[3] - ofvs[4]
    )
  )
})

test_that("add_dofv supports missing models", {
  ofvs <- c(10.0, 20.0, 30.0, 40.0, 50.0)
  tdir <- local_model_dir_nonmem_dummies(
    list(
      list(
        subpath = "01",
        data_set = "../data.csv",
        ofv_no_constant = ofvs[1]
      ),
      list(
        subpath = "02",
        based_on = "01",
        data_set = "../data.csv",
        ofv_no_constant = ofvs[2]
      ),
      list(
        subpath = "03",
        based_on = "02",
        data_set = "../data.csv",
        ofv_no_constant = ofvs[3]
      ),
      list(
        subpath = "04",
        based_on = "03",
        data_set = "../data.csv",
        ofv_no_constant = ofvs[4]
      ),
      list(
        subpath = "05",
        based_on = "04",
        data_set = "../data.csv",
        ofv_no_constant = ofvs[5]
      )
    )
  )

  # Delete everything for model 01.
  fs::dir_delete(file.path(tdir, "01"))
  fs::file_delete(file.path(tdir, paste0("01.", c("ctl", "yaml"))))

  # Delete output directory for model 02.
  fs::dir_delete(file.path(tdir, "02"))

  rlog <- run_log(tdir, .recurse = TRUE)
  # Sort so that these tests don't assume run_log's output order.
  rlog <- dplyr::arrange(rlog, .data[[RUN_ID_COL]])

  slog <- add_summary(rlog)

  # Delete everything for model 03 _after_ the run log and summary log have
  # already been generated.
  fs::dir_delete(file.path(tdir, "03"))
  fs::file_delete(file.path(tdir, paste0("03.", c("ctl", "yaml"))))

  expect_warning(
    res_rlog <- add_dofv(rlog),
    "failed to read model",
    ignore.case = TRUE
  )
  expect_equal(res_rlog[[OFV_COL]], c(NA, NA, ofvs[4], ofvs[5]))
  expect_equal(
    res_rlog[[DOFV_COL]],
    c(NA, NA, NA, ofvs[5] - ofvs[4])
  )

  expect_warning(
    res_slog <- add_dofv(slog),
    "failed to read model",
    ignore.case = TRUE
  )
  # Model 03's ofv is available through the summary log generated before the
  # model was deleted.
  expect_equal(res_slog[[OFV_COL]], c(NA, ofvs[3], ofvs[4], ofvs[5]))
  expect_equal(
    res_slog[[DOFV_COL]],
    c(NA, NA, ofvs[4] - ofvs[3], ofvs[5] - ofvs[4])
  )
})
