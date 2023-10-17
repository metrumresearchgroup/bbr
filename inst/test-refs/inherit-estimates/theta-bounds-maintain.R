
list(
  case = "theta bounds maintain",
  input_ctl = "
$THETA
(0, 2)      ; KA
(0, 3)      ; CL
(0, 10)     ; V2
(0.02)      ; RUVp
(1)         ; RUVa
(0, 0.5, 1) ; F",
replacement = c(1, 2, 3, 4, 5, 6),
input_args = list(.bounds_opts = "maintain_bounds"),
result_ctl = "
$THETA
(0, 1, 2)      ; KA
(0, 2, 3)      ; CL
(0, 3, 10)     ; V2
(4)      ; RUVp
(5)         ; RUVa
(0, 6, 1) ; F"
)

