
list(
  case = "theta many blocks",
  input_ctl = "
$THETA
(0.65)  ; KA
$THETA
(0.3)  ; CL
$THETA
(0.2) ; V2
$THETA
(0.02)  ; RUVp
$THETA
(1)     ; RUVa",
replacement = c(1,2,3,4,5),
input_args = list(.bounds_opts = "maintain_bounds"),
result_ctl = "
$THETA
(1)  ; KA
$THETA
(2)  ; CL
$THETA
(3) ; V2
$THETA
(4)  ; RUVp
$THETA
(5)     ; RUVa"
)

