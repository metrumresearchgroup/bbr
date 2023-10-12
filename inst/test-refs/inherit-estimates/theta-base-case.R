
list(
  input_ctl = "$PROBLEM theta base case
$THETA
(0.65)  ; KA
(0.3)  ; CL
(0.2) ; V2
(0.02)  ; RUVp
(1)     ; RUVa",
replacement = c(1,2,3,4,5),
input_args = list(.bounds_opts = "maintain_bounds"),
result_ctl = "$PROBLEM theta base case
$THETA
(1)  ; KA
(2)  ; CL
(3) ; V2
(4)  ; RUVp
(5)     ; RUVa"
)

