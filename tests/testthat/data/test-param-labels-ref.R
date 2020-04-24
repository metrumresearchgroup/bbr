

# testing block
BLOCK_REF <- list(
  c(TRUE),
  c(TRUE, FALSE, TRUE),
  c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE),
  c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE),
  c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)
)





# references for parsing omega and sigma matrices
MAT_REF <- list(

  # single omega block
  ref_block3 = tibble::tribble(
    ~is_diag,  ~ ref,
    TRUE,      "(1,1)",
    FALSE,     "(2,1)",
    TRUE,      "(2,2)",
    FALSE,     "(3,1)",
    FALSE,     "(3,2)",
    TRUE,      "(3,3)"
  ),

  # diagonal matrix
  ref_diag3 = tibble::tribble(
    ~is_diag, ~ ref,
    TRUE,     "(1,1)",
    TRUE,     "(2,2)",
    TRUE,     "(3,3)"
  ),

  # diagonal matrix then block
  # $OMEGA 0.04
  # $OMEGA BLOCK(2)
  # 0.1  ; [P] CL
  # 0.01 ; [R] V~CL
  # 0.04 ; [P] CL
  ref_mix1 = tibble::tribble(
    ~is_diag,  ~ ref,
    TRUE,      "(1,1)",
    TRUE,      "(2,2)",
    FALSE,     "(3,2)",
    TRUE,      "(3,3)"
  ),

  # diagonal matrix then block
  # $OMEGA 0.04
  # $OMEGA BLOCK(2)
  # 0.1
  # 0.01 0.04
  # 0.01 0.01 0.04
  # $OMEGA 0.04
  ref_mix2 = tibble::tribble(
    ~is_diag, ~ ref,
    TRUE,     "(1,1)",
    TRUE,     "(2,2)",
    FALSE,    "(3,2)",
    TRUE,     "(3,3)",
    FALSE,    "(4,2)",
    FALSE,    "(4,3)",
    TRUE,     "(4,4)",
    TRUE,     "(5,5)"
  )
)
