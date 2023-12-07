
get_param_inits <- function(.mod, init_only = TRUE){
  ctl <- nmrec::read_ctl(ctl_ext(.mod$absolute_model_path))

  recs <- list(
    thetas = nmrec::select_records(ctl, "theta"),
    omegas = nmrec::select_records(ctl, "omega"),
    sigmas = nmrec::select_records(ctl, "sigma")
  )


  extract_record_values <- function(.record){
    .record$parse()
    val_recs <- purrr::keep(.record$values, function(rec_opt){
      inherits(rec_opt, "nmrec_option") && !inherits(rec_opt, "nmrec_option_record_name") &&
        !inherits(rec_opt, c("nmrec_option_value")) && !inherits(rec_opt, c("nmrec_option_flag"))
    })

    # Only grab initial values for theta bounds
    if(init_only){
      val_recs <- purrr::map_chr(val_recs, function(.x){
        val <- purrr::keep(.x$values, function(x_vals){
          inherits(x_vals, "nmrec_option") && x_vals$name == "init"
        })
        val[[1]]$format()
      })
    }else{
      val_recs <- purrr::map_chr(val_recs, function(.x) .x$format())
    }

    return(val_recs)
  }

  recs <- purrr::map(recs, function(.rec) purrr::map(.rec, extract_record_values))

  return(recs)
}

describe("inherit_param_estimates: integration", {

  it("base model", {
    # errors if no based_on field
    expect_error(inherit_param_estimates(MOD1), "did not return any parent models")

    # Normal behavior
    mod2 <- copy_model_from(MOD1, "mod2", "Inherit estimates", .overwrite = TRUE) %>%
      inherit_param_estimates()
    on.exit(delete_models(mod2, .tags = NULL, .force = TRUE))

    mod1_params_final <- list(
      thetas = SUM1 %>% get_theta() %>% sprintf("%.3G", .),
      # Only grab diagonals since no block matrices used
      omegas = diag(SUM1 %>% get_omega()) %>% sprintf("%.3G", .),
      sigmas = diag(SUM1 %>% get_sigma()) %>% sprintf("%.3G", .)
    )

    mod2_inits_inherit <- get_param_inits(mod2)

    expect_equal(mod1_params_final$thetas, mod2_inits_inherit$thetas[[1]])
    expect_equal(mod1_params_final$omegas, mod2_inits_inherit$omegas[[1]])
    expect_equal(mod1_params_final$sigmas, mod2_inits_inherit$sigmas[[1]])

    # Confirm theta bounds
    mod2_params_inherit <- get_param_inits(mod2, init_only = FALSE)
    expect_equal(
      mod2_params_inherit$thetas[[1]][1:3],
      c("(0, 2.32)", "(0, 54.6)", "(0, 463)")
    )
  })

  it("base model - revert theta bounds", {
    mod2 <- copy_model_from(MOD1, "mod2", "Inherit estimates", .overwrite = TRUE) %>%
      inherit_param_estimates(.bounds = "discard")
    on.exit(delete_models(mod2, .tags = NULL, .force = TRUE))

    mod1_params_final <- list(
      thetas = SUM1 %>% get_theta() %>% sprintf("%.3G", .)
    )

    # Confirm theta bounds - parens removed where bounds are removed
    mod2_params_inherit <- get_param_inits(mod2, init_only = FALSE)
    expect_equal(mod1_params_final$thetas[1:3], mod2_params_inherit$thetas[[1]][1:3])
  })

  it("pass a different model", {
    mod2 <- copy_model_from(MOD1, "mod2", .overwrite = TRUE)
    mod3 <- copy_model_from(mod2, "mod3", .overwrite = TRUE)
    on.exit(delete_models(list(mod2, mod3), .tags = NULL, .force = TRUE))

    expect_error(inherit_param_estimates(mod3), "has not been executed")

    mod3 <- inherit_param_estimates(mod3, .parent_mod = MOD1$absolute_model_path)

    mod1_params_final <- list(
      thetas = SUM1 %>% get_theta() %>% sprintf("%.3G", .),
      # Only grab diagonals since no block matrices used
      omegas = diag(SUM1 %>% get_omega()) %>% sprintf("%.3G", .),
      sigmas = diag(SUM1 %>% get_sigma()) %>% sprintf("%.3G", .)
    )

    mod2_inits_inherit <- get_param_inits(mod3)

    expect_equal(mod1_params_final$thetas, mod2_inits_inherit$thetas[[1]])
    expect_equal(mod1_params_final$omegas, mod2_inits_inherit$omegas[[1]])
    expect_equal(mod1_params_final$sigmas, mod2_inits_inherit$sigmas[[1]])

    # Ensure model objects can also be passed
    expect_no_error(inherit_param_estimates(mod3, .parent_mod = MOD1))
  })

  it("Inheriting only some parameters", {
    mod2 <- copy_model_from(MOD1, "mod2", "Inherit estimates", .overwrite = TRUE) %>%
      inherit_param_estimates(.inherit = c("theta"))
    on.exit(delete_models(mod2, .tags = NULL, .force = TRUE))

    mod1_params_final <- list(
      thetas = SUM1 %>% get_theta() %>% sprintf("%.3G", .),
      # Only grab diagonals since no block matrices used
      omegas = diag(SUM1 %>% get_omega()) %>% sprintf("%.3G", .),
      sigmas = diag(SUM1 %>% get_sigma()) %>% sprintf("%.3G", .)
    )

    mod2_inits_inherit <- get_param_inits(mod2)

    # thetas changed
    expect_equal(mod1_params_final$thetas, mod2_inits_inherit$thetas[[1]])
    # Confirm omegas didnt change
    expect_equal(mod1_params_final$omegas, c("0.0985", "0.157"))
    expect_equal(mod2_inits_inherit$omegas[[1]], c("0.05", "0.2"))
  })

  it("fails with old method of using priors", {
    mod <- read_model(file.path(MODEL_DIR_X, "example2_saemimp"))
    mod2 <- copy_model_from(mod, "mod2", "Inherit estimates", .overwrite = TRUE)

    expect_error(
      inherit_param_estimates(mod2),
      "If you're using theta records for priors"
    )
  })
})

