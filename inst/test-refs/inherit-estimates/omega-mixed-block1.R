
list(
  case = "omega mixed block 1",
  input_ctl = "
$ABBR REPLACE ETA(OCC_CL) = ETA(,3:32)
$ABBR REPLACE ETA(OCC_V)  = ETA(,33:62)

$OMEGA
0.05    ; iiv CL
0.2     ; iiv V

$OMEGA BLOCK(1) 0.2 ; iov CL
$OMEGA BLOCK(1) SAME(29)
$OMEGA BLOCK(1) 0.2 ; iov V
$OMEGA BLOCK(1) SAME(29)",
replacement = make_matrix(
  n = 62,
  values = c(c(0.05, 0.2), 0.2, rep(0.2, 29), 0.2, rep(0.2, 29)) + 1, # add 1 to original
  block_loc = c(rep(block(1), 2), block(1), rep(block(1), 29), block(1), rep(block(1), 29))
),
input_args = list(.bounds_opts = "maintain_bounds"),
result_ctl = "
$ABBR REPLACE ETA(OCC_CL) = ETA(,3:32)
$ABBR REPLACE ETA(OCC_V)  = ETA(,33:62)

$OMEGA
1.05    ; iiv CL
1.2     ; iiv V

$OMEGA BLOCK(1) 1.2 ; iov CL
$OMEGA BLOCK(1) SAME(29)
$OMEGA BLOCK(1) 1.2 ; iov V
$OMEGA BLOCK(1) SAME(29)"
)
