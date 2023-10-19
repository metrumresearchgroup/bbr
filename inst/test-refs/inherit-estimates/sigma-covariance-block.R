
list(
  case = "sigma covariance block",
  input_ctl = "
$SIGMA BLOCK(2)
0.1 ; [P]
0.02 0.4 ; [P]",
replacement = make_matrix(
  n = 2,
  values = c(0.1, 0.02, 0.4) + 1, # add 1 to original
  block_loc = block(2)
),
input_args = list(.bounds_opts = "maintain_bounds"),
result_ctl = "
$SIGMA BLOCK(2)
1.1 ; [P]
1.02 1.4 ; [P]"
)

