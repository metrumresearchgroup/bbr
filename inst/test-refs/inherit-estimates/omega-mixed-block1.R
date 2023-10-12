
list(
  input_ctl = "$ABBR REPLACE ETA(OCC_CL) = ETA(,3:32)
$ABBR REPLACE ETA(OCC_V)  = ETA(,33:62)

$OMEGA
0.05    ; iiv CL (This is line 29 in the ctl file)
0.2     ; iiv V

$OMEGA BLOCK(1) 0.2 ; iov CL
$OMEGA BLOCK(1) SAME(29)
$OMEGA BLOCK(1) 0.2 ; iov V
$OMEGA BLOCK(1) SAME(29)",
replacement = make_matrix(
  n = 6,
  values = c(c(0.05, 0.2), 0.2, 0.05, 0.2, 0.05) + 1, # add 1 to original
  block_loc = c(rep(block(1), 6))
),
input_args = list(.bounds_opts = "maintain_bounds"),
result_ctl = "$ABBR REPLACE ETA(OCC_CL) = ETA(,3:32)
$ABBR REPLACE ETA(OCC_V)  = ETA(,33:62)

$OMEGA
1    ; iiv CL
2     ; iiv V

$OMEGA BLOCK(1) 0.2 ; iov CL
$OMEGA BLOCK(1) SAME(29)
$OMEGA BLOCK(1) 0.2 ; iov V
$OMEGA BLOCK(1) SAME(29)"
)
