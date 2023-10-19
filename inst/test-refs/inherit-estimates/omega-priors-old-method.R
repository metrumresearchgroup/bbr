
# The second OMEGA matrix includes the priors. The PRIOR block indicates
# there is a total of 4 ETA values

list(
  case = "omega priors old method (P/PV not specified)",
  input_ctl = "
$PRIOR NWPRI NTHETA=11, NETA=4, NTHP=0, NETP=4, NPEXP=1

$OMEGA BLOCK(4)
0.5  ;[p]
0.001  ;[f]
0.5  ;[p]
0.001 ;[f]
0.001 ;[f]
0.5  ;[p]
0.001 ;[f]
0.001 ;[f]
0.001 ;[f]
0.5 ;[p]

; Prior OMEGA matrix
$OMEGA BLOCK(4)
0.01 FIX
0.0  0.01
0.0 0.0 0.01
0.0 0.0 0.0 0.01",
replacement = make_matrix(
  n = 4,
  values = c(0.5, 0.001, 0.5, 0.001, 0.001, 0.5, 0.001, 0.001, 0.001, 0.5) + 1,  # add 1 to original
  block_loc = block(4)
),
input_args = list(.bounds_opts = "maintain_bounds"),
result_ctl = "
$PRIOR NWPRI NTHETA=11, NETA=4, NTHP=0, NETP=4, NPEXP=1

$OMEGA BLOCK(4)
1.5  ;[p]
1.001  ;[f]
1.5  ;[p]
1.001 ;[f]
1.001 ;[f]
1.5  ;[p]
1.001 ;[f]
1.001 ;[f]
1.001 ;[f]
1.5 ;[p]

; Prior OMEGA matrix
$OMEGA BLOCK(4)
0.01 FIX
0.0  0.01
0.0 0.0 0.01
0.0 0.0 0.0 0.01"
)
