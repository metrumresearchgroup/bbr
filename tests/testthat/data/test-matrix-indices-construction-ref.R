# single omega block
tibble::tribble(
  ~is_diag,
  1,
  0,
  1,
  0,
  0,
  1
)

# diagonal matrix
tibble::tribble(
  ~is_diag,
  1,
  1,
  1
)

# diagonal matrix then block
# $OMEGA 0.04
# $OMEGA BLOCK(2)
# 0.1  ; [P] CL
# 0.01 ; [R] V~CL
# 0.04 ; [P] CL
tibble::tribble(
  ~is_diag,
  1,
  1,
  0,
  1
)

# diagonal matrix then block
# $OMEGA 0.04
# $OMEGA BLOCK(2)
# 0.1
# 0.01 0.04
# 0.01 0.01 0.04
# $OMEGA 0.04
tibble::tribble(
  ~is_diag, ~ want,
  1,        "1,1",
  1,        "2,2",
  0,        "3,2",
  1,        "3,3",
  0,        "4,2",
  0,        "4,3",
  1,        "4,4",
  1,        "5,5"
)
