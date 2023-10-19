
# $SIGMAP for SIGMA prior
# $SIGMAPD for degrees of freedom (or dispersion factor) for SIGMA prior

list(
  case = "sigma priors with named records",
  input_ctl = "
$PRIOR NWPRI NTHETA=6, NETA=4, NEPS=4, NTHP=6, NETP=4, NEPP=0

$SIGMA
0.09          ;[P] KOUThealthy

$SIGMA BLOCK (3)
    0.09          ;[P] KOUTdmd
    0.01          ;[P] cov21
    0.09          ;[P] KINdmd
    0.01          ;[P] cov31
    0.01          ;[P] cov32
    0.09          ;[P] SCALAR

$SIGMAP
  0.09 FIX

$SIGMAP BLOCK(3)
  0.09 FIX
  0.01 0.09
  0.01 0.01 0.09

$SIGMAPD (1 FIX)(3 FIX); df for each $sigma block",
replacement = make_matrix(
  n = 4,
  values = c(c(0.09), c(0.09, 0.01, 0.09, 0.01, 0.01, 0.09)) + 1, # add 1 to original
  block_loc = c(block(1), block(3))
),
input_args = list(.bounds_opts = "maintain_bounds"),
result_ctl = "
$PRIOR NWPRI NTHETA=6, NETA=4, NEPS=4, NTHP=6, NETP=4, NEPP=0

$SIGMA
1.09          ;[P] KOUThealthy

$SIGMA BLOCK (3)
    1.09          ;[P] KOUTdmd
    1.01          ;[P] cov21
    1.09          ;[P] KINdmd
    1.01          ;[P] cov31
    1.01          ;[P] cov32
    1.09          ;[P] SCALAR

$SIGMAP
  0.09 FIX

$SIGMAP BLOCK(3)
  0.09 FIX
  0.01 0.09
  0.01 0.01 0.09

$SIGMAPD (1 FIX)(3 FIX); df for each $sigma block"
)
