
list(
  case = "omega base case",
  input_ctl = "
$OMEGA
0.05    ; iiv CL
0.2     ; iiv V2",
replacement = make_matrix(
  n = 2,
  values = c(0.05, 0.2) + 1, # add 1 to original
  block_loc = c(rep(block(1), 2))
),
input_args = list(.bounds_opts = "maintain_bounds"),
result_ctl = "
$OMEGA
1.05    ; iiv CL
1.2     ; iiv V2"
)

