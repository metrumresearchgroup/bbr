

# I dont think the $ABBR blocks will be relevant, but including them for now
# since they reference ETA values

list(
  case = "omega many blocks",
  input_ctl = "
$ABBR REPLACE ETA(OCC_F1)=ETA(7,8,9, 10, 11, 12, 13, 14, 15, 16, 17, 18)
$ABBR REPLACE ETA(OCC_D1)=ETA(19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)

$OMEGA BLOCK(2)
0.1                               ; [P] CL 1
0.01 0.1                          ; [P] V3 2

$OMEGA
0 FIX
0 FIX

$OMEGA BLOCK(2)
0.1                               ; [P] KA 5
0.01 0.12                         ; [P] D1 6

$OMEGA BLOCK(1) 0.1             ; IOV-F1 7
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME

$OMEGA BLOCK(1) 0 FIX             ; IOV-D1
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME",
replacement = make_matrix(
  n = 30,
  values = c(c(0.1, 0.01, 0.1), c(0, 0), c(0.1, 0.01, 0.12), rep(0.1, 12), rep(0, 12)) + 1,  # add 1 to original
  block_loc = c(block(2), rep(block(1), 2), block(2), rep(block(1), 12), rep(block(1), 12))
),
input_args = list(.bounds_opts = "maintain_bounds"),
result_ctl = "
$ABBR REPLACE ETA(OCC_F1)=ETA(7,8,9, 10, 11, 12, 13, 14, 15, 16, 17, 18)
$ABBR REPLACE ETA(OCC_D1)=ETA(19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)

$OMEGA BLOCK(2)
1.1                               ; [P] CL 1
1.01 1.1                          ; [P] V3 2

$OMEGA
1 FIX
1 FIX

$OMEGA BLOCK(2)
1.1                               ; [P] KA 5
1.01 1.12                         ; [P] D1 6

$OMEGA BLOCK(1) 1.1             ; IOV-F1 7
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME

$OMEGA BLOCK(1) 1 FIX             ; IOV-D1
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME"
)

