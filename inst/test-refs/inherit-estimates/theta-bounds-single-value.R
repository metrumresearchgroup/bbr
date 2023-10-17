
list(
  case = "theta bounds replace with single value",
  input_ctl = "
$THETA
(0, 2)      ; KA
(0, 3)      ; CL
(0, 10)     ; V2
(0.02)      ; RUVp
(1)         ; RUVa
(0, 0.5, 1) ; F",
replacement = c(1, 2, 3, 4, 5, 6),
input_args = list(.bounds_opts = "single_value"),
result_ctl = "
$THETA
(1)      ; KA
(2)      ; CL
(3)     ; V2
(4)      ; RUVp
(5)         ; RUVa
(6) ; F"
)

