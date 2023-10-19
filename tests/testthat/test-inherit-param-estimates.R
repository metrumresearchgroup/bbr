
# Use the helper function below to compare outputs
# diff_record(test_case)

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
    expect_equal(format_record(test_case$input_nmrec), test_case$result_ctl)
  })

  it("with bounds - maintain_bounds", {

    # starting records
    test_case <- get_example_record("theta-bounds-maintain")

    # Copy Thetas
    copy_thetas(
      .mod_lines = test_case$input_nmrec, .new_thetas = test_case$replacement,
      .bounds_opts = test_case$input_args$.bounds_opts
    )

    # Inspect record
    expect_equal(format_record(test_case$input_nmrec), test_case$result_ctl)
  })

  it("with bounds - single_value", {

    # starting records
    test_case <- get_example_record("theta-bounds-single-value")

    # Copy Thetas
    copy_thetas(
      .mod_lines = test_case$input_nmrec, .new_thetas = test_case$replacement,
      .bounds_opts = test_case$input_args$.bounds_opts
    )

    # Inspect record
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
    expect_equal(format_record(test_case$input_nmrec), test_case$result_ctl)
  })

  it("With Priors that are explicity defined (P/PV/PD)", {

    # starting record
    test_case <- get_example_record("theta-priors-named")

    # Copy Thetas
    copy_thetas(
      .mod_lines = test_case$input_nmrec, .new_thetas = test_case$replacement,
      .bounds_opts = test_case$input_args$.bounds_opts
    )

    # Inspect record
    expect_equal(format_record(test_case$input_nmrec), test_case$result_ctl)
  })

  it("many blocks", {

    # starting record
    test_case <- get_example_record("theta-many-blocks")

    # Copy Thetas
    copy_thetas(
      .mod_lines = test_case$input_nmrec, .new_thetas = test_case$replacement,
      .bounds_opts = test_case$input_args$.bounds_opts
    )

    # Inspect record
    expect_equal(format_record(test_case$input_nmrec), test_case$result_ctl)
  })
})


describe("inherit_param_estimates: inherit omegas", {

  it("base case", {

    # starting record
    test_case <- get_example_record("omega-base-case")

    # Copy Thetas
    copy_omegas(
      .mod_lines = test_case$input_nmrec, .new_omegas = test_case$replacement,
      .bounds_opts = test_case$input_args$.bounds_opts
    )

    # Inspect record
    expect_equal(format_record(test_case$input_nmrec), test_case$result_ctl)
  })

  it("Multiple blocks with SAME(val)", {

    # starting record
    test_case <- get_example_record("omega-mixed-block1")

    # Copy Thetas
    copy_omegas(
      .mod_lines = test_case$input_nmrec, .new_omegas = test_case$replacement,
      .bounds_opts = test_case$input_args$.bounds_opts
    )

    # Inspect record
    expect_equal(format_record(test_case$input_nmrec), test_case$result_ctl)
  })

  it("Multiple blocks with SAME", {

    # starting record
    test_case <- get_example_record("omega-mixed-block2")

    # Copy Thetas
    copy_omegas(
      .mod_lines = test_case$input_nmrec, .new_omegas = test_case$replacement,
      .bounds_opts = test_case$input_args$.bounds_opts
    )

    # Inspect record
    expect_equal(format_record(test_case$input_nmrec), test_case$result_ctl)
  })

  it("With Priors that are explicity defined (P/PV/PD)", {

    # starting record
    test_case <- get_example_record("omega-priors-named")

    # Copy Thetas
    copy_omegas(
      .mod_lines = test_case$input_nmrec, .new_omegas = test_case$replacement,
      .bounds_opts = test_case$input_args$.bounds_opts
    )

    # Inspect record
    expect_equal(format_record(test_case$input_nmrec), test_case$result_ctl)
  })

  it("With Priors using the old method (P/PV/PD not used)", {

    # starting record
    test_case <- get_example_record("omega-priors-old-method")

    # Copy Thetas
    copy_omegas(
      .mod_lines = test_case$input_nmrec, .new_omegas = test_case$replacement,
      .bounds_opts = test_case$input_args$.bounds_opts
    )

    # Inspect record
    expect_equal(format_record(test_case$input_nmrec), test_case$result_ctl)
  })

  it("many blocks", {

    # starting record
    test_case <- get_example_record("omega-many-blocks")

    # Copy Thetas
    copy_omegas(
      .mod_lines = test_case$input_nmrec, .new_omegas = test_case$replacement,
      .bounds_opts = test_case$input_args$.bounds_opts
    )

    # Inspect record
    expect_equal(format_record(test_case$input_nmrec), test_case$result_ctl)
  })
})
