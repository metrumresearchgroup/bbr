
# The second THETA matrix includes the priors. The PRIOR block indicates
# there is a total of 14 THETA values

list(
  case = "theta priors old method (P/PV not specified)",
  input_ctl = "
$PRIOR NWPRI NTHETA=14 NETA=4 NEPS=2 NTHP=2 NETP=0 NPEXP=1

;Initial estimates of theta
$THETA
(0,2.4,100)
(-1,1.0,4)
(0,21)
(0,5.4)
(0,7.0)
(0,83)
(0,2.7)
(0,4.9)
(0,0.31)
(0,0.021,.3)
(0,11)
(0,87)
(0,2.6,100)
(-1,0.71,4)

; Prior data for THETA(1)&(2)
$THETA
3 FIX
1.4115 FIX",
replacement = seq(1, 14, by = 1),
input_args = list(.bounds_opts = "maintain_bounds"),
result_ctl = "
$PRIOR NWPRI NTHETA=14 NETA=4 NEPS=2 NTHP=2 NETP=0 NPEXP=1

;Initial estimates of theta
$THETA
(0,1,100)
(-1,2,4)
(0, 3,21)
(0, 4,5.4)
(0, 5,7.0)
(0, 6,83)
(0, 7,2.7)
(0, 8,4.9)
(0, 9,0.31)
(0,10,.3)
(0, 11,11)
(0, 12,87)
(0,13,100)
(-1,14,4)

; Prior data for THETA(1)&(2)
$THETA
3 FIX
1.4115 FIX"
)
