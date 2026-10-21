context("filtering bootstrap summaries (no bbi or NONMEM needed)")

# Minimal bbi_nmboot_summary with 10 runs; `based_on_model_path` doesnt exist
# so param_estimates_compare() only summarizes the bootstrap estimates
make_boot_sum <- function(n = 10){
  runs <- formatC(seq_len(n), width = nchar(n), flag = "0")
  paths <- file.path("/fake/1-boot", runs)

  prderr <- rep(FALSE, n); prderr[c(2, 5)] <- TRUE
  boundary <- rep(FALSE, n); boundary[c(5, 7)] <- TRUE
  boundary[9] <- NA

  boot_sum <- list(
    absolute_model_path = "/fake/1-boot",
    based_on_model_path = "/fake/1.ctl",
    n_samples = n,
    run_details = tibble::tibble(
      absolute_model_path = paths, number_of_obs = 100L
    ),
    run_heuristics = tibble::tibble(
      absolute_model_path = paths,
      prderr = prderr,
      parameter_near_boundary = boundary
    ),
    boot_summary = tibble::tibble(
      absolute_model_path = paths,
      run = runs,
      THETA1 = as.numeric(seq_len(n)),
      OMEGA.1.1. = as.numeric(seq_len(n)) / 10
    )
  )
  class(boot_sum) <- c(NMBOOT_SUM_CLASS, "list")
  boot_sum
}

test_that("filter_bootstrap_summary pads numeric run IDs and drops duplicates", {
  filtered <- suppressWarnings(
    filter_bootstrap_summary(make_boot_sum(100), exclude_runs = c(42, "042", 7L))
  )
  expect_equal(filtered$excluded$run, c("007", "042"))
  expect_equal(filtered$excluded$reason, rep("user-specified", 2))
  expect_error(
    filter_bootstrap_summary(make_boot_sum(100), exclude_runs = 1.5), "not found"
  )
})

test_that("filter_bootstrap_summary excludes flagged runs", {
  boot_sum <- make_boot_sum()
  filtered <- suppressWarnings(
    filter_bootstrap_summary(boot_sum, exclude_heuristics = "prderr")
  )

  expect_s3_class(filtered, NMBOOT_SUM_CLASS)
  expect_equal(filtered$excluded$run, c("02", "05"))
  expect_equal(filtered$excluded$reason, c("prderr", "prderr"))
  expect_equal(nrow(filtered$boot_summary), 8)
  expect_equal(nrow(filtered$run_heuristics), 8)
  expect_equal(nrow(filtered$run_details), 8)
  expect_false(any(filtered$run_heuristics$prderr))

  # boot_compare reflects only remaining runs
  med <- filtered$boot_compare[filtered$boot_compare[[1]] == "THETA1", ]
  expect_equal(unname(unlist(med[, 2])), median(setdiff(1:10, c(2, 5))))
})

test_that("filter_bootstrap_summary combines heuristics and runs, keeps NA", {
  boot_sum <- make_boot_sum()
  filtered <- suppressWarnings(
    filter_bootstrap_summary(
      boot_sum,
      exclude_heuristics = c("prderr", "parameter_near_boundary"),
      exclude_runs = c(5, 10)
    )
  )

  # run 09 has NA boundary: kept
  expect_equal(filtered$excluded$run, c("02", "05", "07", "10"))
  expect_equal(
    filtered$excluded$reason,
    c("prderr", "prderr, parameter_near_boundary, user-specified",
      "parameter_near_boundary", "user-specified")
  )
  expect_true("09" %in% filtered$boot_summary$run)
})

test_that("filter_bootstrap_summary input errors", {
  boot_sum <- make_boot_sum()
  expect_error(
    filter_bootstrap_summary(boot_sum, exclude_heuristics = "nope"),
    "Unknown heuristic"
  )
  expect_error(
    filter_bootstrap_summary(boot_sum, exclude_runs = 11),
    "not found"
  )
  expect_error(
    filter_bootstrap_summary(boot_sum, exclude_runs = 1:10),
    "All bootstrap runs were excluded"
  )
})

test_that("summarize_bootstrap_run() filters the returned object, not the saved RDS", {
  boot_dir <- file.path(withr::local_tempdir(), "1-boot")
  dir.create(boot_dir)
  boot_sum <- make_boot_sum()
  boot_sum$absolute_model_path <- boot_dir
  rds_path <- file.path(boot_dir, "boot_summary.RDS")
  saveRDS(boot_sum, rds_path)
  md5_before <- unname(tools::md5sum(rds_path))

  boot_run <- structure(
    list(
      absolute_model_path = boot_dir, model_type = "nmboot", yaml_md5 = "x",
      bbi_args = list(), based_on = file.path(dirname(boot_dir), "1")
    ),
    class = c(NMBOOT_MOD_CLASS, "bbi_model", "list")
  )

  res <- suppressWarnings(
    summarize_bootstrap_run(boot_run, .exclude_heuristics = "prderr", .exclude_runs = c(1, "10"))
  )
  expect_equal(res$excluded$run, c("01", "02", "05", "10"))
  expect_equal(nrow(res$boot_summary), 6)
  expect_equal(unname(unlist(res$boot_compare[1, "p50"])), median(c(3, 4, 6, 7, 8, 9)))

  # RDS is untouched, and unfiltered calls still return everything
  expect_equal(unname(tools::md5sum(rds_path)), md5_before)
  expect_equal(nrow(suppressWarnings(summarize_bootstrap_run(boot_run))$boot_summary), 10)

  expect_error(summarize_bootstrap_run(boot_run, .exclude_heuristics = character(0)))
  expect_error(summarize_bootstrap_run(boot_run, .exclude_runs = NA))
})
