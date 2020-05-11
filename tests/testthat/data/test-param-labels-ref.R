library(rbabylon)

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



#######################3
# label parsing

PARAM_BLOCK_REF <- list(
  PEX_BLOCK3 = list(
    ctl = "
      $OMEGA BLOCK(3)
      .1          ; [P] 5 P1NPF
      .01 .1      ; [P] 6 CTFX
      .01 .01 .1  ; [P] 7 LSF",
    omega = block(3),
    sigma = NULL,
    ref = structure(list(names = c("OMEGA(1,1)", "OMEGA(2,1)", "OMEGA(2,2)", "OMEGA(3,1)", "OMEGA(3,2)", "OMEGA(3,3)"),
                         label = c("5 P1NPF", "", "6 CTFX", "", "", "7 LSF"),
                         type = c("[P]", "[A]", "[P]", "[A]", "[A]", "[P]"),
                         param_type = c("OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA")),
                    row.names = c(NA, -6L), class = c("tbl_df", "tbl", "data.frame"))
  ),
  ### .tc <- PARAM_REF[["PEX_BLOCK3"]]
  ### dput(.tc$ctl %>% param_labels() %>% apply_indices(.omega = .tc$omega, .sigma = .tc$sigma))
  PEX_BLOCK32 = list(
    ctl = "
      $OMEGA BLOCK(3)
      .1          ;[P] 5 P1NPF
      .01 .1      ;[P] 6 CTFX
      .01 .01 .1  ;[P] 7 LSF
      $OMEGA BLOCK(2)
      .1          ;[P] 8 FAKE1
      .01 .1      ;[P] 9 FAKE2",
    omega = c(block(3), block(2)),
    sigma = NULL,
    ref = structure(list(names = c("OMEGA(1,1)", "OMEGA(2,1)", "OMEGA(2,2)", "OMEGA(3,1)", "OMEGA(3,2)", "OMEGA(3,3)", "OMEGA(4,4)", "OMEGA(5,4)", "OMEGA(5,5)"),
                         label = c("5 P1NPF", "", "6 CTFX", "", "", "7 LSF", "8 FAKE1", "", "9 FAKE2"),
                         type = c("[P]", "[A]", "[P]", "[A]", "[A]", "[P]", "[P]", "[A]", "[P]"),
                         param_type = c("OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA")),
                    row.names = c(NA, -9L), class = c("tbl_df", "tbl", "data.frame"))
  ),

  PEX_BLOCK32S = list(
    ctl = "
      $OMEGA BLOCK (3)
      .1          ;[P] 5 P1NPF
      .01 .1      ;[P] 6 CTFX
      .01 .01 .1  ;[P] 7 LSF
      $OMEGA BLOCK(2)
      .1          ;[P] 8 FAKE1
      .01 .1      ;[P] 9 FAKE2
      $OMEGA BLOCK (1) 0.04 ; [P] IOV_{KA}
      $OMEGA BLOCK(1) SAME",
    omega = c(block(3), block(2), TRUE, TRUE),
    sigma = NULL,
    ref = structure(list(names = c("OMEGA(1,1)", "OMEGA(2,1)", "OMEGA(2,2)", "OMEGA(3,1)", "OMEGA(3,2)", "OMEGA(3,3)", "OMEGA(4,4)", "OMEGA(5,4)", "OMEGA(5,5)", "OMEGA(6,6)", "OMEGA(7,7)"),
                         label = c("5 P1NPF", "", "6 CTFX", "", "", "7 LSF", "8 FAKE1", "", "9 FAKE2", "IOV_{KA}", "IOV_{KA}"),
                         type = c("[P]", "[A]", "[P]", "[A]", "[A]", "[P]", "[P]", "[A]", "[P]", "[P]", "[P]"),
                         param_type = c("OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA")),
                    row.names = c(NA, -11L), class = c("tbl_df", "tbl", "data.frame"))
  ),

  PEX_KAT_DBL2 = list(
    ctl = "
      $OMEGA BLOCK(2) 0.1 0.01 0.1
      $OMEGA BLOCK(2)
      0.1
      0.01  0.1",
    omega = c(
      block(2),
      block(2)
    ),
    sigma = NULL,
    ref = structure(list(names = c("OMEGA(1,1)", "OMEGA(2,1)", "OMEGA(2,2)", "OMEGA(3,3)", "OMEGA(4,3)", "OMEGA(4,4)"),
                         label = c("", "", "", "", "", ""),
                         type = c("[A]", "[A]", "[A]", "[A]", "[A]", "[A]"),
                         param_type = c("OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA")),
                    row.names = c(NA, -6L), class = c("tbl_df", "tbl", "data.frame"))
  ),
  PEX_KAT_DBL2S = list(
    ctl = "
      $OMEGA BLOCK(2)
      0.1
      0.01  0.1
      $OMEGA BLOCK(2) SAME",
    omega = c(
      block(2),
      block(2)
    ),
    sigma = NULL,
    ref = structure(list(names = c("OMEGA(1,1)", "OMEGA(2,1)", "OMEGA(2,2)", "OMEGA(3,3)", "OMEGA(4,3)", "OMEGA(4,4)"),
                         label = c("", "", "", "", "", ""),
                         type = c("[A]", "[A]", "[A]", "[A]", "[A]", "[A]"),
                         param_type = c("OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA")),
                    row.names = c(NA, -6L), class = c("tbl_df", "tbl", "data.frame"))
  ),

  PEX_KAT_PKPD = list(
    ctl = "
      $OMEGA BLOCK(3)
      0.926  ;[P] CL
      -0.176 ;[R] CL-V2
      0.831  ;[P] V2
      0.0787 ;[R] CL-CLDX
      -0.296 ;[R] CL-V2
      0.47   ;[P] CLDX

      $OMEGA
      2 ; KA

      $SIGMA BLOCK(2)
      0.0287
      -0.0114  0.0444

      $SIGMA BLOCK(2)
      5
      1 6",
    omega = c(block(3), TRUE),
    sigma = c(block(2), block(2)),
    ref = structure(list(names = c("OMEGA(1,1)", "OMEGA(2,1)", "OMEGA(2,2)",
                                   "OMEGA(3,1)", "OMEGA(3,2)", "OMEGA(3,3)", "OMEGA(4,4)", "SIGMA(1,1)",
                                   "SIGMA(2,1)", "SIGMA(2,2)", "SIGMA(3,3)", "SIGMA(4,3)", "SIGMA(4,4)"),
                         label = c("CL", "CL-V2", "V2", "CL-CLDX", "CL-V2", "CLDX", "KA", "", "", "", "", "", ""),
                         type = c("[P]", "[R]", "[P]", "[R]", "[R]", "[P]", "[A]", "[A]", "[A]", "[A]", "[A]", "[A]", "[A]"),
                         param_type = c("OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA",
                                        "SIGMA", "SIGMA", "SIGMA", "SIGMA", "SIGMA", "SIGMA")),
                    row.names = c(NA, -13L), class = c("tbl_df", "tbl", "data.frame"))
  ),

  PEX_KAT_ALL = list(
    ctl = "
      $OMEGA BLOCK (3)
      ;---------------------------------
      0.026                 ; [P] CL
      0.01   0.25           ; [P] KA
      0.01   0.01   0.16     ; [P] F1
      $OMEGA BLOCK(3) SAME

      $OMEGA
      0.04  ; IC50

      $OMEGA BLOCK(2) 0.1 0.01 0.1
      $OMEGA BLOCK(2)
      0.1
      0.01  0.1

      $OMEGA BLOCK(1) 0.04                ; [P] IOV_{KA}
      $OMEGA BLOCK(1) SAME
      $OMEGA BLOCK(1) SAME
      $OMEGA BLOCK(1) 0.04                ; [P] IOV_{F1}
      $OMEGA BLOCK(1) SAME (2)
      $OMEGA
      0.04    ; [P] IIVonEPS",
    omega = c(
      block(3),
      block(3),
      TRUE,
      block(2),
      block(2),
      rep(TRUE, 7)
    ),
    sigma = NULL,
    ref = structure(list(names = c("OMEGA(1,1)", "OMEGA(2,1)", "OMEGA(2,2)",
                                   "OMEGA(3,1)", "OMEGA(3,2)", "OMEGA(3,3)", "OMEGA(4,4)", "OMEGA(5,4)",
                                   "OMEGA(5,5)", "OMEGA(6,4)", "OMEGA(6,5)", "OMEGA(6,6)", "OMEGA(7,7)",
                                   "OMEGA(8,8)", "OMEGA(9,8)", "OMEGA(9,9)", "OMEGA(10,10)", "OMEGA(11,10)",
                                   "OMEGA(11,11)", "OMEGA(12,12)", "OMEGA(13,13)", "OMEGA(14,14)",
                                   "OMEGA(15,15)", "OMEGA(16,16)", "OMEGA(17,17)", "OMEGA(18,18)"),
                         label = c("CL", "", "KA", "", "", "F1", "CL", "", "KA", "", "", "F1", "IC50", "",
                                   "", "", "", "", "", "IOV_{KA}", "IOV_{KA}",
                                   "IOV_{KA}", "IOV_{F1}", "IOV_{F1}", "IOV_{F1}", "IIVonEPS"),
                         type = c("[P]", "[A]", "[P]", "[A]", "[A]", "[P]", "[P]", "[A]", "[P]", "[A]", "[A]", "[P]", "[A]",
                                  "[A]", "[A]", "[A]", "[A]", "[A]", "[A]", "[P]", "[P]", "[P]", "[P]", "[P]", "[P]", "[P]"),
                         param_type = c("OMEGA", "OMEGA", "OMEGA", "OMEGA",
                                    "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA",
                                    "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA",
                                    "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA",
                                    "OMEGA")),
                    row.names = c(NA, -26L), class = "data.frame")
  ),

  PEX_SAM1 = list(
    ctl = "
      $OMEGA
      .1   ;1 LSCF
      .01  ;2 PLE

      $OMEGA BLOCK(2)
      .1       ;3 C501
      .01 .1   ;4 C501

      $OMEGA BLOCK(3)
      .1          ;5 P1NPF
      .01 .1      ;6 CTFX
      .01 .01 .1  ;7 LSF

      $OMEGA
      .1  ;8 AP1NP
      .1  ;9 ACTX

      $OMEGA BLOCK(3)
      .1          ;10 ISL0
      .01 .1      ;11 ACTX
      .01 .01 .1  ;12 ISL0

      $SIGMA
      2.00E-02  ;1 P1NP
      4.00E-02  ;2 CTX
      3.00E-02  ;3 BMDLS",
    omega = c(TRUE, TRUE, block(2), block(3), TRUE, TRUE, block(3)),
    sigma = NULL,
    ref = structure(list(names = c("OMEGA(1,1)", "OMEGA(2,2)", "OMEGA(3,3)",
                                   "OMEGA(4,3)", "OMEGA(4,4)", "OMEGA(5,5)", "OMEGA(6,5)", "OMEGA(6,6)",
                                   "OMEGA(7,5)", "OMEGA(7,6)", "OMEGA(7,7)", "OMEGA(8,8)", "OMEGA(9,9)",
                                   "OMEGA(10,10)", "OMEGA(11,10)", "OMEGA(11,11)", "OMEGA(12,10)",
                                   "OMEGA(12,11)", "OMEGA(12,12)", "SIGMA(1,1)", "SIGMA(2,2)", "SIGMA(3,3)"),
                         label = c("1 LSCF", "2 PLE", "3 C501", "", "4 C501", "5 P1NPF", "", "6 CTFX", "", "", "7 LSF",
                                   "8 AP1NP", "9 ACTX", "10 ISL0", "", "11 ACTX", "", "", "12 ISL0", "1 P1NP", "2 CTX", "3 BMDLS"),
                         type = c("[A]", "[A]", "[A]", "[A]", "[A]", "[A]", "[A]", "[A]", "[A]", "[A]", "[A]", "[A]",
                                  "[A]", "[A]", "[A]", "[A]", "[A]", "[A]", "[A]", "[A]", "[A]", "[A]"),
                         param_type = c("OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA",
                                        "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA",
                                        "OMEGA", "OMEGA", "OMEGA", "OMEGA", "SIGMA", "SIGMA", "SIGMA")),
                    row.names = c(NA, -22L), class = "data.frame")
  ),

  PEX_DBL_CMT = list( ## parses "correctly" but as expected only takes first label
    ctl = "
      $SIGMA 1 FIXED

      $THETA
      3.5 ;31.5 1 CL
      5.5 ;244.2 ; 2 V2
      -0.61 ;0.49; 3 Q
      2.7 ; 14.16; 4 V3
      1 ; 1.05 ; 5 KA
      0.75 FIX ; 6 weight on CL
      1.0 FIX ; 7 weight on V
      0.09 ; 8 prop res err LLOQ=0.02
      0.3; 9  fractional change in prop res err LLOQ=0.25
      (1)  ; 10 low dose effect on Ka

      $OMEGA BLOCK(2)
      0.4 ;0.0237 ; CL
      0.02 0.4 ; V2

      $OMEGA BLOCK(2)
      0.4 ;0.0117 ; Q
      0.04 0.4 ; 0.879 ; V3

      $OMEGA
      0.4 ; 0.4367 ; KA",
    omega = c(block(2), block(2), TRUE),
    sigma = NULL,
    ref = structure(list(names = c("THETA1", "THETA2", "THETA3", "THETA4", "THETA5", "THETA6", "THETA7", "THETA8", "THETA9", "THETA10",
                                   "OMEGA(1,1)", "OMEGA(2,1)", "OMEGA(2,2)", "OMEGA(3,3)", "OMEGA(4,3)", "OMEGA(4,4)", "OMEGA(5,5)", "SIGMA(1,1)"),
                         label = c("31.5 1 CL", "244.2", "0.49", "14.16", "1.05", "6 weight on CL", "7 weight on V",
                                   "8 prop res err LLOQ=0.02", "9  fractional change in prop res err LLOQ=0.25", "10 low dose effect on Ka",
                                   "0.0237", "", "V2", "0.0117", "", "0.879", "0.4367", ""),
                         unit = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""),
                         type = c("", "", "", "", "", "", "", "", "", "", "[A]", "[A]", "[A]", "[A]", "[A]", "[A]", "[A]", "[A]"),
                         param_type = c("THETA", "THETA", "THETA", "THETA", "THETA", "THETA", "THETA", "THETA", "THETA", "THETA",
                                        "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA", "OMEGA", "SIGMA")),
                    row.names = c(NA, -18L), class = c("tbl_df", "tbl", "data.frame"))
  )

)

#### visual references
# PEX_BLOCK3
# # A tibble: 6 x 4
# names      label   type  param_type
# <chr>      <chr>   <chr> <chr>
# 1 OMEGA(1,1) 5 P1NPF [P]   OMEGA
# 2 OMEGA(2,1) ""      [A]   OMEGA
# 3 OMEGA(2,2) 6 CTFX  [P]   OMEGA
# 4 OMEGA(3,1) ""      [A]   OMEGA
# 5 OMEGA(3,2) ""      [A]   OMEGA
# 6 OMEGA(3,3) 7 LSF   [P]   OMEGA

# PEX_BLOCK32
# # A tibble: 9 x 4
# names      label   type  param_type
# <chr>      <chr>   <chr> <chr>
# 1 OMEGA(1,1) 5 P1NPF [P]   OMEGA
# 2 OMEGA(2,1) ""      [A]   OMEGA
# 3 OMEGA(2,2) 6 CTFX  [P]   OMEGA
# 4 OMEGA(3,1) ""      [A]   OMEGA
# 5 OMEGA(3,2) ""      [A]   OMEGA
# 6 OMEGA(3,3) 7 LSF   [P]   OMEGA
# 7 OMEGA(4,4) 8 FAKE1 [P]   OMEGA
# 8 OMEGA(5,4) ""      [A]   OMEGA
# 9 OMEGA(5,5) 9 FAKE2 [P]   OMEGA


# PEX_BLOCK32S
# # A tibble: 11 x 4
# names      label    type  param_type
# <chr>      <chr>    <chr> <chr>
# 1 OMEGA(1,1) 5 P1NPF  [P]   OMEGA
# 2 OMEGA(2,1) ""       [A]   OMEGA
# 3 OMEGA(2,2) 6 CTFX   [P]   OMEGA
# 4 OMEGA(3,1) ""       [A]   OMEGA
# 5 OMEGA(3,2) ""       [A]   OMEGA
# 6 OMEGA(3,3) 7 LSF    [P]   OMEGA
# 7 OMEGA(4,4) 8 FAKE1  [P]   OMEGA
# 8 OMEGA(5,4) ""       [A]   OMEGA
# 9 OMEGA(5,5) 9 FAKE2  [P]   OMEGA
# 10 OMEGA(6,6) IOV_{KA} [P]   OMEGA
# 11 OMEGA(7,7) ""       [A]   OMEGA


# PEX_KAT_DBL2 and PEX_KAT_DBL2S
# # A tibble: 6 x 4
# names      label type  param_type
# <chr>      <chr> <chr> <chr>
# 1 OMEGA(1,1) ""    [A]   OMEGA
# 2 OMEGA(2,1) ""    [A]   OMEGA
# 3 OMEGA(2,2) ""    [A]   OMEGA
# 4 OMEGA(3,3) ""    [A]   OMEGA
# 5 OMEGA(4,3) ""    [A]   OMEGA
# 6 OMEGA(4,4) ""    [A]   OMEGA


# PEX_KAT_PKPD
# # A tibble: 13 x 4
# names      label   type  param_type
# <chr>      <chr>   <chr> <chr>
# 1 OMEGA(1,1) CL      [P]   OMEGA
# 2 OMEGA(2,1) CL-V2   [R]   OMEGA
# 3 OMEGA(2,2) V2      [P]   OMEGA
# 4 OMEGA(3,1) CL-CLDX [R]   OMEGA
# 5 OMEGA(3,2) CL-V2   [R]   OMEGA
# 6 OMEGA(3,3) CLDX    [P]   OMEGA
# 7 OMEGA(4,4) KA      [A]   OMEGA
# 8 SIGMA(1,1) ""      [A]   SIGMA
# 9 SIGMA(2,1) ""      [A]   SIGMA
# 10 SIGMA(2,2) ""      [A]   SIGMA
# 11 SIGMA(3,3) ""      [A]   SIGMA
# 12 SIGMA(4,3) ""      [A]   SIGMA
# 13 SIGMA(4,4) ""      [A]   SIGMA


# PEX_KAT_ALL
# # A tibble: 26 x 4
#      names       label    type     param_type
#      <chr>       <chr>    <chr>    <chr>
# 1    OMEGA(1,1)       CL  [P]      OMEGA
# 2    OMEGA(2,1)           [A]      OMEGA
# 3    OMEGA(2,2)       KA  [P]      OMEGA
# 4    OMEGA(3,1)           [A]      OMEGA
# 5    OMEGA(3,2)           [A]      OMEGA
# 6    OMEGA(3,3)       F1  [P]      OMEGA
# 7    OMEGA(4,4)       CL  [P]      OMEGA
# 8    OMEGA(5,4)           [A]      OMEGA
# 9    OMEGA(5,5)       KA  [P]      OMEGA
# 10   OMEGA(6,4)           [A]      OMEGA
# 11   OMEGA(6,5)           [A]      OMEGA
# 12   OMEGA(6,6)       F1  [P]      OMEGA
# 13   OMEGA(7,7)     IC50  [A]      OMEGA
# 14   OMEGA(8,8)           [A]      OMEGA
# 15   OMEGA(9,8)           [A]      OMEGA
# 16   OMEGA(9,9)           [A]      OMEGA
# 17 OMEGA(10,10)           [A]      OMEGA
# 18 OMEGA(11,10)           [A]      OMEGA
# 19 OMEGA(11,11)           [A]      OMEGA
# 20 OMEGA(12,12) IOV_{KA}  [P]      OMEGA
# 21 OMEGA(13,13) IOV_{KA}  [P]      OMEGA
# 22 OMEGA(14,14) IOV_{KA}  [P]      OMEGA
# 23 OMEGA(15,15) IOV_{F1}  [P]      OMEGA
# 24 OMEGA(16,16) IOV_{F1}  [P]      OMEGA
# 25 OMEGA(17,17) IOV_{F1}  [P]      OMEGA
# 26 OMEGA(18,18) IIVonEPS  [P]      OMEGA


# PEX_SAM1
# # A tibble: 22 x 4
#      names       label    type     param_type
#      <chr>       <chr>    <chr>    <chr>
# 1    OMEGA(1,1)  1 LSCF  [A]      OMEGA
# 2    OMEGA(2,2)   2 PLE  [A]      OMEGA
# 3    OMEGA(3,3)  3 C501  [A]      OMEGA
# 4    OMEGA(4,3)          [A]      OMEGA
# 5    OMEGA(4,4)  4 C501  [A]      OMEGA
# 6    OMEGA(5,5) 5 P1NPF  [A]      OMEGA
# 7    OMEGA(6,5)          [A]      OMEGA
# 8    OMEGA(6,6)  6 CTFX  [A]      OMEGA
# 9    OMEGA(7,5)          [A]      OMEGA
# 10   OMEGA(7,6)          [A]      OMEGA
# 11   OMEGA(7,7)   7 LSF  [A]      OMEGA
# 12   OMEGA(8,8) 8 AP1NP  [A]      OMEGA
# 13   OMEGA(9,9)  9 ACTX  [A]      OMEGA
# 14 OMEGA(10,10) 10 ISL0  [A]      OMEGA
# 15 OMEGA(11,10)          [A]      OMEGA
# 16 OMEGA(11,11) 11 ACTX  [A]      OMEGA
# 17 OMEGA(12,10)          [A]      OMEGA
# 18 OMEGA(12,11)          [A]      OMEGA
# 19 OMEGA(12,12) 12 ISL0  [A]      OMEGA
# 20   SIGMA(1,1)  1 P1NP  [A]      SIGMA
# 21   SIGMA(2,2)   2 CTX  [A]      SIGMA
# 22   SIGMA(3,3) 3 BMDLS  [A]      SIGMA


# PEX_DBL_CMT ## parses "correctly" but as expected only takes first label
# # A tibble: 17 x 5
# names      label                                          unit  type  param_type
# <chr>      <chr>                                          <chr> <chr> <chr>
# 1 THETA1     31.5 1 CL                                      ""    ""    THETA
# 2 THETA2     244.2                                          ""    ""    THETA
# 3 THETA3     0.49                                           ""    ""    THETA
# 4 THETA4     14.16                                          ""    ""    THETA
# 5 THETA5     1.05                                           ""    ""    THETA
# 6 THETA6     6 weight on CL                                 ""    ""    THETA
# 7 THETA7     7 weight on V                                  ""    ""    THETA
# 8 THETA8     8 prop res err LLOQ=0.02                       ""    ""    THETA
# 9 THETA9     9  fractional change in prop res err LLOQ=0.25 ""    ""    THETA
# 10 OMEGA(1,1) 0.0237                                         ""    [A]   OMEGA
# 11 OMEGA(2,1) ""                                             ""    [A]   OMEGA
# 12 OMEGA(2,2) V2                                             ""    [A]   OMEGA
# 13 OMEGA(3,3) 0.0117                                         ""    [A]   OMEGA
# 14 OMEGA(4,3) ""                                             ""    [A]   OMEGA
# 15 OMEGA(4,4) 0.879                                          ""    [A]   OMEGA
# 16 OMEGA(5,5) 0.4367                                         ""    [A]   OMEGA
# 17 SIGMA(1,1) ""                                             ""    [A]   SIGMA

