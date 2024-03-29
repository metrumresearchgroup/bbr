

# Note: tweaking initial estimates is subject to random sampling
# The seed must be set to ensure the same results are returned
# i.e. when testing that estimates that fall outside theta bounds, always
# return a value outside of the initial bounds, to test that it is set to one
# of the bounds.

describe("tweak_initial_estimates", {
  skip_if_old_nmrec("0.4.0")

  it("base case - tweaking initial value works", {
    test_case <- list(
      case = "base case",
      input_ctl = "
      $THETA
      (0, 2)      ; KA
      (0, 3)      ; CL
      (0, 10)     ; V2
      $OMEGA
      0.05    ; iiv CL
      0.2     ; iiv V2
      $SIGMA
      0.2 ; [P]
      "
    )

    # Create fake model
    mod_tweak <- do.call(make_fake_mod, test_case)
    on.exit(delete_models(mod_tweak, .tags = NULL, .force = TRUE))

    # Get initial estimates
    initial_est <- get_initial_est(mod_tweak, flag_fixed = FALSE)

    # Tweak initial estimates with set seed
    withr::with_seed(1234, {
      mod_tweak_new <- tweak_initial_estimates(mod_tweak, .p = 0.2)
    })

    est_tweaked <- get_initial_est(mod_tweak_new, flag_fixed = FALSE)

    # Expect all values to be tweaked
    expect_true(all(est_tweaked$thetas$init != initial_est$thetas$init))
  })

  it("theta bounds - value falls outside one of the bounds", {

    # The purpose of this test is to ensure the tweaked value is always
    # within the bounds, taking rounding into account. If it cannot be placed within
    # the bounds, the original value is used, and no tweaking will be done for that
    # option
    test_case <- list(
      case = "theta bounds; tweaked value falls outside",
      input_ctl = "
      $THETA
      (1.98, 2, 2.02) ;
      (1.999, 2, 2.001) ;
      (1.95, 2, 2.05) ;
      (0, 1); lower bound only
      (0, 3, 5); 'regular' bound
      "
    )
    # Create fake model with bounds
    mod_tweak <- do.call(make_fake_mod, test_case)
    on.exit(delete_models(mod_tweak, .tags = NULL, .force = TRUE))

    # Get initial estimates
    thetas_init <- get_initial_est(mod_tweak, flag_fixed = TRUE)$thetas

    # Test that tweaked values are always within bounds, regardless of rounding
    for(digits.i in c(1, 2, 3, 4, 5)){
      for(perc.i in c(0.1, 0.2, 0.3)){
        thetas_init$new <- tweak_thetas(thetas_init, .p = perc.i, digits = digits.i)
        # All values should be within bounds
        init_within_bounds <- (thetas_init$new < thetas_init$up) &
          (thetas_init$new > thetas_init$low)
        expect_true(all(init_within_bounds[!is.na(init_within_bounds)]))
      }
    }
  })

  it("theta bounds - no initial value (ignore)", {
    test_case <- list(
      case = "theta bounds; no initial value",
      input_ctl = "
      $THETA
      (0,,1) ; KA
      (0, 3) ; CL
      "
    )

    # Create fake model with no initial value (just bounds)
    mod_tweak <- do.call(make_fake_mod, test_case)
    on.exit(delete_models(mod_tweak, .tags = NULL, .force = TRUE))

    # Get initial estimates
    thetas_init <- get_initial_est(mod_tweak, flag_fixed = TRUE)$thetas

    # Ensure first initial value starts off as NA (not set)
    expect_true(is.na(thetas_init$init[1]))

    # Tweak initial estimates with set seed
    mod_tweak_new <- withr::with_seed(1234, {
      tweak_initial_estimates(mod_tweak, .p = 0.2, tweak = "theta")
    })
    thetas_tweaked <- get_initial_est(mod_tweak_new, flag_fixed = TRUE)$thetas

    # Ensure first initial value is still NA (not set), and othe value is tweaked
    expect_true(is.na(thetas_tweaked$init[1]))
    expect_true(thetas_init$init[2] != thetas_tweaked$init[2])
  })

  it("FIXED parameters", {
    test_case <- list(
      case = "fixed parameters",
      input_ctl = "
      $THETA
      0.65  ; KA
      0.3 FIX ; CL
      $OMEGA
      0.3
      0.5 FIX   ; iiv CL
      0.2     ; iiv V2
      $OMEGA BLOCK(2)
      0.05 FIX
      0.01 0.2
      "
    )

    # Create fake model with fixed parameters
    mod_tweak <- do.call(make_fake_mod, test_case)
    on.exit(delete_models(mod_tweak, .tags = NULL, .force = TRUE))

    # Get initial estimates
    initial_est <- get_initial_est(mod_tweak, flag_fixed = TRUE)

    # Tweak initial estimates with set seed
    mod_tweak_new <- withr::with_seed(1234, {
      tweak_initial_estimates(mod_tweak, .p = 0.2)
    })
    est_tweaked <- get_initial_est(mod_tweak_new, flag_fixed = TRUE)
    est_tweaked

    ## Check thetas
    expect_true(est_tweaked$thetas$init[2] == initial_est$thetas$init[2]) # Fixed
    expect_true(est_tweaked$thetas$init[1] != initial_est$thetas$init[1]) # Tweaked
  })
})
