$SIZES LIM1=16000 LVR=64
$PROBLEM PK model 1 cmt base with IOV on CL

$INPUT ID TIME MDV EVID DV AMT SEX WT ETN OCC
$DATA ../acop_iov.csv IGNORE=@
$SUBROUTINES ADVAN2 TRANS2

$ABBR DERIV2 = NOCOMMON
$ABBR REPLACE ETA(OCC_CL) = ETA(,3:32)
$ABBR REPLACE ETA(OCC_V)  = ETA(,33:62)

$PK
ET=1
IF(ETN.EQ.3) ET=1.3
KA = THETA(1)
CL = THETA(2) * ((WT/70)**0.75) * EXP(ETA(1) + ETA(OCC_CL))
V  = THETA(3) * EXP(ETA(2) + ETA(OCC_V))
SC=V


$THETA
(0, 2)  ; KA
(0, 3)  ; CL
(0, 10) ; V
(0.02)  ; RUVp
(1)     ; RUVa

$OMEGA
0.05    ; iiv CL
0.2     ; iiv V  

$OMEGA BLOCK(1) 0.2 ; iov CL
$OMEGA BLOCK(1) SAME(29)
$OMEGA BLOCK(1) 0.2 ; iov V
$OMEGA BLOCK(1) SAME(29)

$SIGMA	
1 FIX

$ERROR
IPRED = F
IRES = DV-IPRED
W = IPRED*THETA(4) + THETA(5)
IF (W.EQ.0) W = 1
IWRES = IRES/W
Y= IPRED+W*ERR(1)

$EST METHOD=1 INTERACTION MAXEVAL=9999 SIG=3 PRINT=1 NOABORT POSTHOC
$COV