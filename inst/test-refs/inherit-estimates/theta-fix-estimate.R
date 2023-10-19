
# In this example, only the OMEGAs have a prior distribution, the THETAS do not.
# PRIOR block is included for consistency, though it is also needed to filter out
# the second THETA block (using `$PRIOR NTHETA=11`)

# Here, the second THETA block is treated as a prior (filtered out), despite the
# fact it's actually used to define the degrees of freedom of the OMEGA priors
# (equivalent to $OMEGAPD)

list(
  case = "theta fix",
  input_ctl = "
$PRIOR NWPRI NTHETA=11, NETA=4, NTHP=0, NETP=4, NPEXP=1

$THETA
( 0.7 ) ;[LCLM]
( 0.7 ) ;[LCLF]
( 2 )   ;[CLAM]
( 2.0);[CLAF]
( 0.7 ) ;[LV1M]
( 0.7 ) ;[LV1F]
( 2.0 )   ;[V1AM]
( 2.0 )   ;[V1AF]
( 0.7 ) ;[MU_3]
(  0.7 );[MU_4]
( 0.3 )     ;[SDSL]

; Degrees of freedom to OMEGA prior matrix:
$THETA 4 FIX",
replacement = seq(1, 11, by = 1),
input_args = list(.bounds_opts = "maintain_bounds"),
result_ctl = "
$PRIOR NWPRI NTHETA=11, NETA=4, NTHP=0, NETP=4, NPEXP=1

$THETA
( 1 ) ;[LCLM]
( 2 ) ;[LCLF]
( 3 )   ;[CLAM]
( 4);[CLAF]
( 5 ) ;[LV1M]
( 6 ) ;[LV1F]
( 7 )   ;[V1AM]
( 8 )   ;[V1AF]
( 9 ) ;[MU_3]
(  10 );[MU_4]
( 11 )     ;[SDSL]

; Degrees of freedom to OMEGA prior matrix:
$THETA 4 FIX"
)
