
list(
  input_ctl = "$PROBLEM omega mixed block
$OMEGA
(0.04) ;1. CL VAR
(0.04) ;2. V VAR

;IOV
$OMEGA BLOCK(1) 0.03; interoccasion var in CL
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME",
replacement = make_matrix(
  n = 5,
  values = c(c(0.04, 0.04), 0.03, 0.03, 0.03) + 1, # add 1 to original
  block_loc = c(rep(block(1), 5))
),
input_args = list(.bounds_opts = "maintain_bounds"),
result_ctl = "$PROBLEM omega mixed block
$OMEGA
(1.04) ;1. CL VAR
(1.04) ;2. V VAR

;IOV
$OMEGA BLOCK(1) 1.03; interoccasion var in CL
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME"
)
