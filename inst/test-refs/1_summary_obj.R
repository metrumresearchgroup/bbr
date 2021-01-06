structure(list(absolute_model_path = "/data/home/sethg/bbr/inst/model/nonmem/basic/1"
, run_details = list(
  version = "7.4.4", run_start = "-999999999",
  run_end = "Wed Dec  2 11:10:39 EST 2020", estimation_time = 1.96,
  cpu_time = 2.555, function_evaluations = 366L, significant_digits = 3.2,
  problem_text = "LEM PK model 1 cmt base", mod_file = "-999999999",
  estimation_method = "First Order Conditional Estimation with Interaction",
  data_set = "../../../../extdata/acop.csv", number_of_patients = 40L,
  number_of_obs = 760L, number_of_data_records = 799L, output_files_used = c(
    "1.lst",
    "1.cpu", "1.ext", "1.grd", "1.shk"
  )
), run_heuristics = list(
  covariance_step_aborted = FALSE, large_condition_number = FALSE,
  correlations_not_ok = FALSE, parameter_near_boundary = FALSE,
  hessian_reset = FALSE, has_final_zero_gradient = FALSE, minimization_terminated = FALSE,
  eta_pval_significant = FALSE, prderr = FALSE
), parameters_data = list(
  list(
    method = "TABLE NO.     1: First Order Conditional Estimation with Interaction: Goal Function=MINIMUM VALUE OF OBJECTIVE FUNCTION: Problem=1 Subproblem=0 Superproblem1=0 Iteration1=0 Superproblem2=0 Iteration2=0",
    estimates = list(theta = c(
      2.31034, 54.9596, 464.659,
      -0.0805722, 4.1303
    ), omega = c(0.0964407, 0, 0.153571), sigma = 1L), std_err = list(theta = c(
      0.086147, 3.32914,
      29.6177, 0.0555149, 1.35989
    ), omega = c(
      0.0200146, 1e+10,
      0.026733
    ), sigma = 1e+10), random_effect_sd = list(omega = c(
      0.310549,
      0, 0.391882
    ), sigma = 1L), random_effect_sdse = list(
      omega = c(0.0322245, 1e+10, 0.0341085), sigma = 1e+10
    ),
    fixed = list(theta = c(0L, 0L, 0L, 0L, 0L), omega = c(
      0L,
      1L, 0L
    ), sigma = 1L)
  )
), parameter_names = list(theta = c(
  "THETA1",
  "THETA2", "THETA3", "THETA4", "THETA5"
), omega = c(
  "OMEGA(1,1)",
  "OMEGA(2,1)", "OMEGA(2,2)"
), sigma = "SIGMA(1,1)"), ofv = list(
  list(
    method = "First Order Conditional Estimation with Interaction",
    ofv_no_constant = 2636.846, constant_to_ofv = 1396.7865704711,
    ofv_with_constant = 4033.63234080018
  )
), condition_number = list(
  list(
    method = "First Order Conditional Estimation with Interaction",
    condition_number = -999999999L
  )
), shrinkage_details = list(
  list(list(
    sub_pop = 1L, eta_bar = c(0.00179673, -0.00738995), ebv_bar_se = c(0.0399941, 0.0599224), pval = c(
      0.964167,
      0.901849
    ), eta_sd = c(17.5115, 2.05962), eps_sd = 4.09105,
    ebv_sd = c(18.5247, 2.54114), num_subjects = c(40L, 40L), eta_vr = c(31.9565, 4.07682), eps_vr = 8.01472, ebv_vr = c(
      33.6177,
      5.0177
    )
  ))
)), class = c("bbi_nonmem_summary", "list"))
