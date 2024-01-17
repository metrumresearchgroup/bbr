
get_param_inits <- function(.mod, init_only = TRUE){
  ctl <- nmrec::read_ctl(get_model_path(.mod))

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

  recs <- purrr::map(recs, function(.rec) unlist(purrr::map(.rec, extract_record_values)))

  return(recs)
}

withr::with_options(list(bbr.bbi_exe_path = read_bbi_path()), {
  describe("inherit_param_estimates: integration", {
    skip_if_old_nmrec("0.3.0")
    it("base model", {
      # errors if no based_on field
      expect_error(inherit_param_estimates(MOD1), "did not return any parent models")

      # Normal behavior
      mod_est <- copy_model_from(MOD1, "mod_est", "Inherit estimates", .overwrite = TRUE) %>%
        inherit_param_estimates()
      on.exit(delete_models(mod_est, .tags = NULL, .force = TRUE))

      mod1_params_final <- list(
        thetas = SUM1 %>% get_theta() %>% sprintf("%.3G", .),
        # Only grab diagonals since no block matrices used
        omegas = diag(SUM1 %>% get_omega()) %>% sprintf("%.3G", .),
        sigmas = diag(SUM1 %>% get_sigma()) %>% sprintf("%.3G", .)
      )

      mod2_inits_inherit <- get_param_inits(mod_est)

      expect_equal(mod1_params_final$thetas, mod2_inits_inherit$thetas)
      expect_equal(mod1_params_final$omegas, mod2_inits_inherit$omegas)
      expect_equal(mod1_params_final$sigmas, mod2_inits_inherit$sigmas)

      # Confirm theta bounds
      mod2_params_inherit <- get_param_inits(mod_est, init_only = FALSE)
      expect_true(all(stringr::str_detect(
        mod2_params_inherit$thetas[1:3],
        paste0("(0, ", mod2_inits_inherit$thetas[1:3], ")")
      )))
    })

    it("base model - revert theta bounds", {
      mod_est <- copy_model_from(MOD1, "mod_est", "Inherit estimates", .overwrite = TRUE) %>%
        inherit_param_estimates(bounds = "discard")

      on.exit(delete_models(mod_est, .tags = NULL, .force = TRUE))

      mod1_params_final <- list(
        thetas = SUM1 %>% get_theta() %>% sprintf("%.3G", .)
      )

      # Confirm theta bounds - parens removed where bounds are removed
      mod2_params_inherit <- get_param_inits(mod_est, init_only = FALSE)
      expect_equal(mod1_params_final$thetas[1:3], mod2_params_inherit$thetas[1:3])
    })

    it("pass a different model", {
      mod_est <- copy_model_from(MOD1, "mod_est", .overwrite = TRUE)
      mod_est2 <- copy_model_from(mod_est, "mod_est2", .overwrite = TRUE)
      on.exit(delete_models(list(mod_est, mod_est2), .tags = NULL, .force = TRUE))

      expect_error(inherit_param_estimates(mod_est2), "has not been executed")

      mod_est2 <- inherit_param_estimates(mod_est2, .parent_mod = MOD1)

      mod1_params_final <- list(
        thetas = SUM1 %>% get_theta() %>% sprintf("%.3G", .),
        # Only grab diagonals since no block matrices used
        omegas = diag(SUM1 %>% get_omega()) %>% sprintf("%.3G", .),
        sigmas = diag(SUM1 %>% get_sigma()) %>% sprintf("%.3G", .)
      )

      mod2_inits_inherit <- get_param_inits(mod_est2)

      expect_equal(mod1_params_final$thetas, mod2_inits_inherit$thetas)
      expect_equal(mod1_params_final$omegas, mod2_inits_inherit$omegas)
      expect_equal(mod1_params_final$sigmas, mod2_inits_inherit$sigmas)
    })

    it("Inheriting only some parameters", {
      mod_est <- copy_model_from(MOD1, "mod_est", "Inherit estimates", .overwrite = TRUE) %>%
        inherit_param_estimates(inherit = c("theta"))
      on.exit(delete_models(mod_est, .tags = NULL, .force = TRUE))

      mod1_params_final <- list(
        thetas = SUM1 %>% get_theta() %>% sprintf("%.3G", .),
        # Only grab diagonals since no block matrices used
        omegas = diag(SUM1 %>% get_omega()) %>% sprintf("%.3G", .),
        sigmas = diag(SUM1 %>% get_sigma()) %>% sprintf("%.3G", .)
      )

      mod1_inits <- get_param_inits(MOD1)
      mod2_inits_inherit <- get_param_inits(mod_est)

      # thetas changed
      expect_equal(mod1_params_final$thetas, mod2_inits_inherit$thetas)
      # Confirm omegas didnt change
      expect_equal(mod1_inits$omegas, mod2_inits_inherit$omegas)
    })

    it("fails with old method of using priors", {
      mod <- read_model(file.path(MODEL_DIR_X, "example2_saemimp"))
      mod_est <- copy_model_from(mod, "mod_est", "Inherit estimates", .overwrite = TRUE)
      on.exit(delete_models(mod_est, .tags = NULL, .force = TRUE))

      expect_error(
        inherit_param_estimates(mod_est),
        "If you're using THETA records for priors"
      )

      # substitute THETA for THETAPV
      mod_lines <- readLines(get_model_path(mod_est))
      mod_lines <- stringr::str_replace(mod_lines, "THETA 4 FIX",  "THETAPV 4 FIX")

      # substitute second OMEGA block for OMEGAP
      omega_prior_block <- max(grep("; Prior OMEGA matrix", mod_lines, fixed = TRUE)) + 1
      mod_lines[omega_prior_block] <- stringr::str_replace(mod_lines[omega_prior_block], "OMEGA", "OMEGAP")

      # ensure it works now
      writeLines(mod_lines, get_model_path(mod_est))
      mod_est <- inherit_param_estimates(mod_est)

      based_on_sum <- model_summary(read_model(get_based_on(mod_est)))
      omegas <- get_omega(based_on_sum)
      mod_params_final <- list(
        thetas = based_on_sum %>% get_theta() %>% sprintf("%.3G", .),
        # Grab upper triangular matrix values since block matrix is used
        omegas = omegas[upper.tri(omegas, diag = TRUE)] %>% sprintf("%.3G", .),
        sigmas = diag(based_on_sum %>% get_sigma()) %>% sprintf("%.3G", .)
      )
      mod_inits_inherit <- get_param_inits(mod_est)

      expect_equal(mod_params_final$thetas, mod_inits_inherit$thetas)
      expect_equal(mod_params_final$omegas, mod_inits_inherit$omegas)
      expect_equal(mod_params_final$sigmas, mod_inits_inherit$sigmas)
    })

    it("works with multiple based_on models", {
      mod_est <- copy_model_from(MOD1, "mod_est", "Inherit estimates", .overwrite = TRUE) %>%
        add_based_on("../complex/acop-iov") %>%
        inherit_param_estimates()
      on.exit(delete_models(mod_est, .tags = NULL, .force = TRUE))

      expect_equal(length(get_based_on(mod_est)), 2L)

      mod1_params_final <- list(thetas = SUM1 %>% get_theta() %>% sprintf("%.3G", .))
      mod2_inits_inherit <- get_param_inits(mod_est)
      expect_equal(mod1_params_final$thetas, mod2_inits_inherit$thetas)
    })

    it("works with missing grd and shk files", {

      TEST_CASES <- list(
        list(ext = "grd", missing = NULL),
        list(ext = "shk", missing = "shrinkage_details")
      )

      for (.tc in TEST_CASES) {
        # create new model to inherit estimates from
        mod2 <- MOD1 %>% copy_model_from(basename(NEW_MOD2))
        # copy output directory (to simulate model run)
        copy_output_dir(MOD1, NEW_MOD2)
        # delete relevant file from summarized (parent) model
        fs::file_delete(build_path_from_model(mod2, glue::glue(".{.tc$ext}")))

        # create new model to copy estimates to
        mod3 <- mod2 %>% copy_model_from(basename(NEW_MOD3))

        # errors without the flag
        expect_error(
          inherit_param_estimates(mod3, .bbi_args = list()),
          glue::glue("[Nn]o file present at.*2/2\\.{.tc$ext}")
        )

        # works correctly with flag added
        args_list <- list()
        args_list[[as.character(glue::glue("no_{.tc$ext}_file"))]] <- TRUE
        expect_no_error(
          mod3 <- inherit_param_estimates(mod3, .bbi_args = args_list)
        )

        delete_models(list(mod2, mod3), .tags = NULL, .force = TRUE)
      }
    })
  })

})
