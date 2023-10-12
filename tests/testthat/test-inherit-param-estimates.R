


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
    expect_equal(recs[[1]]$format(), test_case$result_ctl)
  })

  it("Second block for fixing theta value", {

    # starting record
    theta_rec <- get_example_record("theta-fix-estimate")

    # replacement values
    new_thetas <- example_rec_lengths(theta_rec, "theta")
    new_thetas <- new_thetas[[1]] # second block isn't used

    # Copy Thetas
    copy_thetas(
      .mod_lines = test_case$input_nmrec, .new_thetas = test_case$replacement,
      .bounds_opts = test_case$input_args$.bounds_opts
    )

    # Inspect record
    recs <- nmrec::select_records(theta_rec[[1]], "theta")
  })

})

