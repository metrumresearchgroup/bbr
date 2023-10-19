
list(
  case = "omega covariance block",
  input_ctl = "
$OMEGA BLOCK(4)
0.2
0.01 0.2
0.01 0.01 0.2
0.01 0.01 0.01 0.2",
replacement = make_matrix(
  n = 4,
  values = c(0.2, 0.01, 0.2, 0.01, 0.01, 0.2, 0.01, 0.01, 0.01, 0.2) + 1, # add 1 to original
  block_loc = block(4)
),
input_args = list(.bounds_opts = "single_value"),
result_ctl = "
$OMEGA BLOCK(4)
1.2
1.01 1.2
1.01 1.01 1.2
1.01 1.01 1.01 1.2"
)

