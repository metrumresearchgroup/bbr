
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
kold1 <- param_labels(KAT_OLD_OMEGA_1) %>% apply_indices() ##### SAME get discarded
#kold1 <- param_labels(KAT_OLD_OMEGA_1) %>% apply_indices(.omega = )

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
knew1 <- param_labels(KAT_NEW_OMEGA_1) %>% apply_indices()

suppressPackageStartupMessages(library(dplyr))
kpdf <- full_join(kold1, knew1, by = "names") %>% select(names, label.x, label.y, type.x, type.y, everything())
kpdf



# OMEGA BLOCK(3)
PEX_BLOCK3 <- "
$OMEGA BLOCK(3)
.1          ;5 P1NPF
.01 .1      ;6 CTFX
.01 .01 .1  ;7 LSF
"

PEX_BLOCK3 %>% param_labels() %>% apply_indices(block(3))



PEX_BLOCK32 <- "
$OMEGA BLOCK(3)
.1          ;[P] 5 P1NPF
.01 .1      ;[P] 6 CTFX
.01 .01 .1  ;[P] 7 LSF
$OMEGA BLOCK(2)
.1          ;[P] 8 FAKE1
.01 .1      ;[P] 9 FAKE2
"

PEX_BLOCK32 %>% param_labels() %>% apply_indices(c(block(3), block(2)))
# Warning messages:
#   1: Unknown or uninitialised column: 'label'.
# 2: In .f(.x[[i]], ...) : NAs introduced by coercion
# 3: In .f(.x[[i]], ...) : NAs introduced by coercion
# 4: Unknown or uninitialised column: 'label'.




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
