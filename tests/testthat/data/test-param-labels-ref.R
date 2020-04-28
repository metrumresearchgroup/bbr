
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


############ Katherine stuff

KAT_OLD_OMEGA_1 <- "
$OMEGA
;---------------------------------
0.026    ; [P] CL
0.25     ; [P] KA
0.16     ; [P] F1
$OMEGA BLOCK(1) 0.04                ; [P] IOV_{KA}
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) 0.04                ; [P] IOV_{F1}
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA
0.04    ; [P] IIVonEPS
"

KAT_NEW_OMEGA_1 <- "
$OMEGA BLOCK(1) 0.1383   ; [P] CL
$OMEGA BLOCK(1) 0 FIX     ; [P] V2
$OMEGA BLOCK(1) 0.2377   ; [P] KA_KTR
$OMEGA BLOCK(1) 0 FIX    ; [P] Q3
$OMEGA BLOCK(1) 0 FIX    ; [P] V3
$OMEGA BLOCK(1) 0.0478   ; [P] F1
$OMEGA BLOCK(1) 0.4702   ;   [P] IOV_{KA}
$OMEGA BLOCK(1) SAME(2)
$OMEGA BLOCK(1) 0.3651     ; [P] IOV_{F1}
$OMEGA BLOCK(1) SAME (2)
$OMEGA
0.1972    ; [P] IIVonEPS
"

# suppressPackageStartupMessages(library(dplyr))
# kold1 <- param_labels(KAT_OLD_OMEGA_1) %>% apply_indices()
# knew1 <- param_labels(KAT_NEW_OMEGA_1) %>% apply_indices()
# kpdf <- full_join(kold1, knew1, by = "names") %>% select(names, label.x, label.y, type.x, type.y, everything())
# kpdf
