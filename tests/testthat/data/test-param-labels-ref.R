
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


# OMEGA BLOCK(3)
PEX_BLOCK3 <- "
$OMEGA BLOCK(3)
.1          ; [P] 5 P1NPF
.01 .1      ; [P] 6 CTFX
.01 .01 .1  ; [P] 7 LSF
"

PEX_BLOCK3 %>% param_labels() %>% apply_indices(.omega = block(3))
# # A tibble: 6 x 4
# names      label   type  param_type
# <chr>      <chr>   <chr> <chr>
# 1 OMEGA(1,1) 5 P1NPF [P]   OMEGA
# 2 OMEGA(2,1) ""      [A]   OMEGA
# 3 OMEGA(2,2) 6 CTFX  [P]   OMEGA
# 4 OMEGA(3,1) ""      [A]   OMEGA
# 5 OMEGA(3,2) ""      [A]   OMEGA
# 6 OMEGA(3,3) 7 LSF   [P]   OMEGA


PEX_BLOCK32 <- "
$OMEGA BLOCK(3)
.1          ;[P] 5 P1NPF
.01 .1      ;[P] 6 CTFX
.01 .01 .1  ;[P] 7 LSF
$OMEGA BLOCK(2)
.1          ;[P] 8 FAKE1
.01 .1      ;[P] 9 FAKE2
"
diag_vec <- c(block(3), block(2))
PEX_BLOCK32 %>% param_labels() %>% apply_indices(.omega = diag_vec)
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


PEX_BLOCK32S <- "
$OMEGA BLOCK(3)
.1          ;[P] 5 P1NPF
.01 .1      ;[P] 6 CTFX
.01 .01 .1  ;[P] 7 LSF
$OMEGA BLOCK(2)
.1          ;[P] 8 FAKE1
.01 .1      ;[P] 9 FAKE2
$OMEGA BLOCK(1) 0.04                ; [P] IOV_{KA}
$OMEGA BLOCK(1) SAME
"
diag_vec <- c(block(3), block(2), T, T)
PEX_BLOCK32S %>% param_labels() %>% apply_indices(.omega = diag_vec)
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


PEX_KAT_DBL2 <- "
$OMEGA BLOCK(2) 0.1 0.01 0.1
$OMEGA BLOCK(2)
0.1
0.01  0.1
"
diag_vec <- c(
  block(2),
  block(2)
)
PEX_KAT_DBL2 %>% param_labels() %>% apply_indices(.omega = diag_vec)
# # A tibble: 6 x 4
# names      label type  param_type
# <chr>      <chr> <chr> <chr>
# 1 OMEGA(1,1) ""    [A]   OMEGA
# 2 OMEGA(2,1) ""    [A]   OMEGA
# 3 OMEGA(2,2) ""    [A]   OMEGA
# 4 OMEGA(3,3) ""    [A]   OMEGA
# 5 OMEGA(4,3) ""    [A]   OMEGA
# 6 OMEGA(4,4) ""    [A]   OMEGA


# from Katherine (slack?)
PEX_KAT_ALL <- "
$OMEGA BLOCK(3)
;---------------------------------
0.026                 ; [P] CL
0.01   0.25           ; [P] KA
0.01   0.01   0.16     ; [P] F1
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
0.04    ; [P] IIVonEPS
"
# note that is looks right, but it's by accident because `SAME (2)` gets parsed to two params `SAME` and `2`
diag_vec <- c(
  block(3),
  TRUE,
  block(2),
  block(2),
  rep(T, 7)
)
PEX_KAT_ALL %>% param_labels() %>% apply_indices(.omega = diag_vec)
# # A tibble: 19 x 4
# names        label    type  param_type
# <chr>        <chr>    <chr> <chr>
# 1 OMEGA(1,1)   CL       [P]   OMEGA
# 2 OMEGA(2,1)   ""       [A]   OMEGA
# 3 OMEGA(2,2)   KA       [P]   OMEGA
# 4 OMEGA(3,1)   ""       [A]   OMEGA
# 5 OMEGA(3,2)   ""       [A]   OMEGA
# 6 OMEGA(3,3)   F1       [P]   OMEGA
# 7 OMEGA(4,4)   IC50     [A]   OMEGA
# 8 OMEGA(5,5)   ""       [A]   OMEGA
# 9 OMEGA(6,5)   ""       [A]   OMEGA
# 10 OMEGA(6,6)   ""       [A]   OMEGA
# 11 OMEGA(7,7)   ""       [A]   OMEGA
# 12 OMEGA(8,7)   ""       [A]   OMEGA
# 13 OMEGA(8,8)   ""       [A]   OMEGA
# 14 OMEGA(9,9)   IOV_{KA} [P]   OMEGA
# 15 OMEGA(10,10) ""       [A]   OMEGA
# 16 OMEGA(11,11) ""       [A]   OMEGA
# 17 OMEGA(12,12) IOV_{F1} [P]   OMEGA
# 18 OMEGA(13,13) ""       [A]   OMEGA
# 19 OMEGA(14,14) IIVonEPS [P]   OMEGA


# from Katherine: PK-PD model where I has different sigma blocks - one for the PK data and one for the PD data
PEX_KAT_PKPD <- "
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
1 6
"
PEX_KAT_PKPD %>% param_labels() %>%
  apply_indices(
    .omega = c(block(3), TRUE),
    .sigma = c(block(2), block(2))
  )
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


# ############ stuff Katherine slacked me a few days ago
# # I have a few questions about this:
# # 1. what is `$OMEGA BLOCK(1) SAME(2)`? Is it 2 params or 1?
# # 2. are `SAME(2)` and `SAME (2)` (with the space) both valid?
# # 3. are these two blocks supposed to parse to the same thing?
# KAT_OLD_OMEGA_1 <- "
# $OMEGA
# ;---------------------------------
# 0.026    ; [P] CL
# 0.25     ; [P] KA
# 0.16     ; [P] F1
# $OMEGA BLOCK(1) 0.04                ; [P] IOV_{KA}
# $OMEGA BLOCK(1) SAME
# $OMEGA BLOCK(1) SAME
# $OMEGA BLOCK(1) 0.04                ; [P] IOV_{F1}
# $OMEGA BLOCK(1) SAME
# $OMEGA BLOCK(1) SAME
# $OMEGA
# 0.04    ; [P] IIVonEPS
# "
# KAT_OLD_OMEGA_1 %>% param_labels() %>% apply_indices()
# # # A tibble: 10 x 4
# # names        label    type  param_type
# # <chr>        <chr>    <chr> <chr>
# # 1 OMEGA(1,1)   CL       [P]   OMEGA
# # 2 OMEGA(2,2)   KA       [P]   OMEGA
# # 3 OMEGA(3,3)   F1       [P]   OMEGA
# # 4 OMEGA(4,4)   IOV_{KA} [P]   OMEGA
# # 5 OMEGA(5,5)   ""       [A]   OMEGA
# # 6 OMEGA(6,6)   ""       [A]   OMEGA
# # 7 OMEGA(7,7)   IOV_{F1} [P]   OMEGA
# # 8 OMEGA(8,8)   ""       [A]   OMEGA
# # 9 OMEGA(9,9)   ""       [A]   OMEGA
# # 10 OMEGA(10,10) IIVonEPS [P]   OMEGA
#
#
# KAT_NEW_OMEGA_1 <- "
# $OMEGA BLOCK(1) 0.1383   ; [P] CL
# $OMEGA BLOCK(1) 0 FIX     ; [P] V2
# $OMEGA BLOCK(1) 0.2377   ; [P] KA_KTR
# $OMEGA BLOCK(1) 0 FIX    ; [P] Q3
# $OMEGA BLOCK(1) 0 FIX    ; [P] V3
# $OMEGA BLOCK(1) 0.0478   ; [P] F1
# $OMEGA BLOCK(1) 0.4702   ;   [P] IOV_{KA}
# $OMEGA BLOCK(1) SAME(2)
# $OMEGA BLOCK(1) 0.3651     ; [P] IOV_{F1}
# $OMEGA BLOCK(1) SAME (2)
# $OMEGA
# 0.1972    ; [P] IIVonEPS
# "
# KAT_NEW_OMEGA_1 %>% param_labels() %>% apply_indices()
# # # A tibble: 10 x 4
# # names        label    type  param_type
# # <chr>        <chr>    <chr> <chr>
# # 1 OMEGA(1,1)   CL       [P]   OMEGA
# # 2 OMEGA(2,2)   V2       [P]   OMEGA
# # 3 OMEGA(3,3)   KA_KTR   [P]   OMEGA
# # 4 OMEGA(4,4)   Q3       [P]   OMEGA
# # 5 OMEGA(5,5)   V3       [P]   OMEGA
# # 6 OMEGA(6,6)   F1       [P]   OMEGA
# # 7 OMEGA(7,7)   IOV_{KA} [P]   OMEGA
# # 8 OMEGA(8,8)   IOV_{F1} [P]   OMEGA
# # 9 OMEGA(9,9)   ""       [A]   OMEGA
# # 10 OMEGA(10,10) IIVonEPS [P]   OMEGA
#
# suppressPackageStartupMessages(library(dplyr))
# dplyr::full_join(
#   KAT_OLD_OMEGA_1 %>% param_labels() %>% apply_indices(),
#   KAT_NEW_OMEGA_1 %>% param_labels() %>% apply_indices(),
#   by = "names"
# ) %>%
#   select(names, label.x, label.y, type.x, type.y)
# # # A tibble: 10 x 5
# # names        label.x  label.y  type.x type.y
# # <chr>        <chr>    <chr>    <chr>  <chr>
# # 1 OMEGA(1,1)   CL       CL       [P]    [P]
# # 2 OMEGA(2,2)   KA       V2       [P]    [P]
# # 3 OMEGA(3,3)   F1       KA_KTR   [P]    [P]
# # 4 OMEGA(4,4)   IOV_{KA} Q3       [P]    [P]
# # 5 OMEGA(5,5)   ""       V3       [A]    [P]
# # 6 OMEGA(6,6)   ""       F1       [A]    [P]
# # 7 OMEGA(7,7)   IOV_{F1} IOV_{KA} [P]    [P]
# # 8 OMEGA(8,8)   ""       IOV_{F1} [A]    [P]
# # 9 OMEGA(9,9)   ""       ""       [A]    [A]
# # 10 OMEGA(10,10) IIVonEPS IIVonEPS [P]    [P]


############ Katherine stuff ^



# from Devin: Double comments
PEX_DBL_CMT <- "
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
0.4 ; 0.4367 ; KA
"
PEX_DBL_CMT %>% param_labels() %>%
  apply_indices(.omega = c(block(2), block(2), TRUE))
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

## parses "correctly" but as expected only takes first label


# Sam: from TAK_TAK0203F
PEX_TAK1 <- "
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
3.00E-02  ;3 BMDLS
"
diag_vec <- c(TRUE, TRUE, block(2), block(3), TRUE, TRUE, block(3))
PEX_TAK1 %>% param_labels() %>%
  apply_indices(.omega = diag_vec) %>%
  select(names, label) %>% as.data.frame()

# # A tibble: 22 x 2
#           names      label
#           <chr>      <chr>
# 1    OMEGA(1,1)     1 LSCF
# 2    OMEGA(2,2)      2 PLE
# 3    OMEGA(3,3)     3 C501
# 4    OMEGA(4,3)
# 5    OMEGA(4,4)     4 C501
# 6    OMEGA(5,5)    5 P1NPF
# 7    OMEGA(6,5)
# 8    OMEGA(6,6)     6 CTFX
# 9    OMEGA(7,5)
# 10   OMEGA(7,6)
# 11   OMEGA(7,7)      7 LSF
# 12   OMEGA(8,8)    8 AP1NP
# 13   OMEGA(9,9)     9 ACTX
# 14 OMEGA(10,10)    10 ISL0
# 15 OMEGA(11,10)
# 16 OMEGA(11,11)    11 ACTX
# 17 OMEGA(12,10)
# 18 OMEGA(12,11)
# 19 OMEGA(12,12)    12 ISL0
# 20   SIGMA(1,1)     1 P1NP
# 21   SIGMA(2,2)      2 CTX
# 22   SIGMA(3,3)    3 BMDLS
