
describe("tweak_initial_estimates", {
  skip_if_old_nmrec("0.3.0") # TODO: increment version

  it("base case", {

    # Create model with bounds
    mod_tweak <- copy_model_from(MOD1, "mod_tweak", "Tweak estimates", .overwrite = TRUE)
    on.exit(delete_models(mod_tweak, .tags = NULL, .force = TRUE))
    ctl <- nmrec::read_ctl(get_model_path(mod_tweak))
    initial_est <- get_initial_est(mod_tweak, flag_fixed = TRUE)
    initial_est$thetas$low <- 1.99
    initial_est$thetas$up <- 2.01
    # TODO: find a way to more easily update bounds/fixed/no init for models for testing

  })

})
