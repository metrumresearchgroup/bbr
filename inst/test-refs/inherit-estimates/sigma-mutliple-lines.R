
list(
  case = "sigma multiple lines",
  input_ctl = "
$SIGMA
0.04
10",
replacement = make_matrix(
  n = 2,
  values = c(0.04, 10) + 1, # add 1 to original
  block_loc = c(rep(block(1), 2))
),
input_args = list(.bounds_opts = "maintain_bounds"),
result_ctl = "
$SIGMA
0.04
10"
)

