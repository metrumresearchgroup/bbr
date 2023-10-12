
list(
  input_ctl = "$PROBLEM theta priors complex
$PRIOR NWPRI NTHETA=6, NETA=4, NEPS=2, NTHP=6, NETP=4, NEPP=0
$THETA
-0.574 ;[1/year]
5.67 ;[meters/year]
1.03 ;[na]
0.24 ;[years]
-11.36 ;[na]
5.1 ;[na]

$THETAP ;prior mode for thetas
-0.676 FIX
5.8 FIX
0.921 FIX
0.390 FIX
-11.4 FIX
5 FIX

$THETAPV BLOCK(6) ;var-cov for prior on thetas --> uncertainty for thetas
10 FIX
0  10
0  0  10
0  0  0  0.0625
0  0  0  0  10
0  0  0  0  0  2",
replacement = seq(1, 6, by = 1),
input_args = list(.bounds_opts = "maintain_bounds"),
result_ctl = "$PROBLEM theta priors complex
$PRIOR NWPRI NTHETA=6, NETA=4, NEPS=2, NTHP=6, NETP=4, NEPP=0
$THETA
1 ;[1/year]
2 ;[meters/year]
3 ;[na]
4 ;[years]
5 ;[na]
6 ;[na]

$THETAP ;prior mode for thetas
-0.676 FIX
5.8 FIX
0.921 FIX
0.390 FIX
-11.4 FIX
5 FIX

$THETAPV BLOCK(6) ;var-cov for prior on thetas --> uncertainty for thetas
10 FIX
0  10
0  0  10
0  0  0  0.0625
0  0  0  0  10
0  0  0  0  0  2"
)
