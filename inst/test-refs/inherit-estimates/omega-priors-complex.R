
list(
  case = "omega priors complex",
  input_ctl = "
$PRIOR NWPRI NTHETA=6, NETA=4, NEPS=2, NTHP=6, NETP=4, NEPP=0

$OMEGA
0.09          ;[P] KOUThealthy

$OMEGA BLOCK (3)
    0.09          ;[P] KOUTdmd
    0.01          ;[P] cov21
    0.09          ;[P] KINdmd
    0.01          ;[P] cov31
    0.01          ;[P] cov32
    0.09          ;[P] SCALAR

$OMEGAP
  0.09 FIX

$OMEGAP BLOCK(3)
  0.09 FIX
  0.01 0.09
  0.01 0.01 0.09

$OMEGAPD (1 FIX)(3 FIX); df for each $omega block",
replacement = make_matrix(
  n = 4,
  values = c(c(0.09), c(0.09, 0.01, 0.09, 0.01, 0.01, 0.09)) + 1, # add 1 to original
  block_loc = c(block(1), block(3))
),
input_args = list(.bounds_opts = "maintain_bounds"),
result_ctl = "
$PRIOR NWPRI NTHETA=6, NETA=4, NEPS=2, NTHP=6, NETP=4, NEPP=0

$OMEGA
1.09          ;[P] KOUThealthy

$OMEGA BLOCK (3)
    1.09          ;[P] KOUTdmd
    1.01          ;[P] cov21
    1.09          ;[P] KINdmd
    1.01          ;[P] cov31
    1.01          ;[P] cov32
    1.09          ;[P] SCALAR

$OMEGAP
  0.09 FIX

$OMEGAP BLOCK(3)
  0.09 FIX
  0.01 0.09
  0.01 0.01 0.09

$OMEGAPD (1 FIX)(3 FIX); df for each $omega block"
)
