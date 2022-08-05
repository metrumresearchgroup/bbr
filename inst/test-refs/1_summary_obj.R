structure(list(
  absolute_model_path = "/data/home/kylem/src/github/metrumresearchgroup/bbr/inst/model/nonmem/basic/1",
  run_details = list(
    version = "7.4.4", run_start = NA_real_,
    run_end = "Mon Nov  8 11:35:08 EST 2021", estimation_time = 5.28,
    covariance_time = 1.29, postprocess_time = 0.46, cpu_time = 6.816,
    function_evaluations = 447L, significant_digits = 3.2,
    problem_text = "LEM PK model 1 cmt base", mod_file = NA_real_,
    estimation_method = "First Order Conditional Estimation with Interaction",
    data_set = "../../../../extdata/acop.csv", number_of_subjects = 39L,
    number_of_obs = 741L, number_of_data_records = 779L,
    output_files_used = c(
      "1.lst", "1.cpu", "1.ext", "1.grd",
      "1.shk"
    )
  ), run_heuristics = list(
    covariance_step_aborted = FALSE,
    large_condition_number = FALSE, eigenvalue_issues = FALSE,
    correlations_not_ok = FALSE, parameter_near_boundary = FALSE,
    hessian_reset = TRUE, has_final_zero_gradient = FALSE,
    minimization_terminated = FALSE, eta_pval_significant = FALSE,
    prderr = FALSE
  ), parameters_data = list(list(
    method = "TABLE NO.     1: First Order Conditional Estimation with Interaction: Goal Function=MINIMUM VALUE OF OBJECTIVE FUNCTION: Problem=1 Subproblem=0 Superproblem1=0 Iteration1=0 Superproblem2=0 Iteration2=0",
    estimates = list(theta = c(
      2.31716, 54.6151, 462.514,
      -0.0820082, 4.17959
    ), omega = c(0.0985328, 0, 0.156825), sigma = 1L), std_err = list(theta = c(
      0.087208, 3.37551,
      30.2623, 0.0561272, 1.37796
    ), omega = c(
      0.0203449, 1e+10,
      0.0272401
    ), sigma = 1e+10), random_effect_sd = list(omega = c(
      0.313899,
      0, 0.396011
    ), sigma = 1L), random_effect_sdse = list(
      omega = c(0.0324068, 1e+10, 0.0343931), sigma = 1e+10
    ),
    fixed = list(theta = c(0L, 0L, 0L, 0L, 0L), omega = c(
      0L,
      1L, 0L
    ), sigma = 1L)
  )), parameter_names = list(theta = c(
    "THETA1",
    "THETA2", "THETA3", "THETA4", "THETA5"
  ), omega = c(
    "OMEGA(1,1)",
    "OMEGA(2,1)", "OMEGA(2,2)"
  ), sigma = "SIGMA(1,1)"), ofv = list(
    list(
      method = "First Order Conditional Estimation with Interaction",
      ofv_no_constant = 2583.311, constant_to_ofv = 1361.86690620932,
      ofv_with_constant = 3945.17780715107
    )
  ), condition_number = list(
    list(
      method = "First Order Conditional Estimation with Interaction",
      condition_number = NA_real_
    )
  ), shrinkage_details = list(
    list(list(
      sub_pop = 1L, eta_bar = c(0.00197362, -0.00620906), ebv_bar_se = c(0.0409479, 0.0612968), pval = c(
        0.961558,
        0.919316
      ), eta_sd = c(17.4695, 2.07276), eps_sd = 4.08455,
      ebv_sd = c(18.5033, 2.56445), num_subjects = c(
        39L,
        39L
      ), eta_vr = c(31.8872, 4.10256), eps_vr = 8.00227,
      ebv_vr = c(33.5829, 5.06314)
    ))
  ), success = TRUE
), class = c(
  "bbi_nonmem_summary",
  "bbi_model", "list"
))
