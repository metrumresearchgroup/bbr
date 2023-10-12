
list(
  input_ctl = "$PROBLEM sigma base case
$SIGMA
0.2 ; [P]",
replacement = make_matrix(
  n = 1,
  values = 0.2 + 1, # add 1 to original
  block_loc = block(1)
),
input_args = list(.bounds_opts = "maintain_bounds"),
result_ctl = "$PROBLEM sigma base case
$SIGMA
1.2 ; [P]"
)

