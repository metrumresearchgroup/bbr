


describe("inherit_param_estimates: inherit thetas", {

  it("base case", {

    # starting record
    test_case <- get_example_record("theta-base-case")

    # Copy Thetas
    copy_thetas(
      .mod_lines = test_case$input_nmrec, .new_thetas = test_case$replacement,
      .bounds_opts = test_case$input_args$.bounds_opts
    )

    # Inspect record
    recs <- nmrec::select_records(test_case$input_nmrec, "theta")
    expect_equal(format_record(test_case$input_nmrec), test_case$result_ctl)
  })

  it("fixing theta value (df for OMEGA matrix)", {

    # starting record
    test_case <- get_example_record("theta-fix-estimate")

    # Copy Thetas
    copy_thetas(
      .mod_lines = test_case$input_nmrec, .new_thetas = test_case$replacement,
      .bounds_opts = test_case$input_args$.bounds_opts
    )

    # Inspect record
    recs <- nmrec::select_records(test_case$input_nmrec, "theta")
    expect_equal(format_record(test_case$input_nmrec), test_case$result_ctl)
  })

})

