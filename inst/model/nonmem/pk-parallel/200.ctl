$PROBLEM From bbr: see 107.yaml for details

$INPUT C NUM ID TIME SEQ CMT EVID AMT DV AGE WT EGFR ALB

$DATA ../../../../extdata/acop_adjusted.csv IGNORE@

$SUBROUTINE ADVAN13 TOL=5
$MODEL COMP(DEPOT)
	     COMP(CENTRAL)
	     COMP(PERPH)

$PK

;log transformed PK parms

V2WT = LOG(WT/70)
CLWT = LOG(WT/70)*0.75
CLEGFR = LOG(EGFR/90)*THETA(6)
CLAGE = LOG(AGE/35)*THETA(7)
V3WT = LOG(WT/70)
QWT  = LOG(WT/70)*0.75
CLALB = LOG(ALB/4.5)*THETA(8)


KA   = EXP(THETA(1)+ETA(1))
V2   = EXP(THETA(2)+V2WT+ETA(2))
CL   = EXP(THETA(3)+CLWT+CLEGFR+CLAGE+CLALB+ETA(3))
V3   = EXP(THETA(4)+V3WT)
Q    = EXP(THETA(5)+QWT)

S2 = V2/1000 ; dose in mcg, conc in mcg/mL


$DES

DADT(1) = -KA*A(1)
DADT(2) = KA*A(1) -(CL/V2)*A(2) -(Q/V2)*A(2) + (Q/V3)*A(3)
DADT(3) =                        (Q/V2)*A(2) - (Q/V3)*A(3)


$ERROR
IPRED = F
Y=IPRED*(1+EPS(1))

$THETA  ; log values
(0.5)   ;  1 KA (1/hr) - 1.5
(3.5)   ;  2 V2 (L) - 60
(1)     ;  3 CL (L/hr) - 3.5
(4)     ;  4 V3 (L) - 70
(2)     ;  5 Q  (L/hr) - 4
(1)     ;  6 CLEGFR~CL ()
(1)     ;  7 AGE~CL ()
(0.5)   ;  8 ALB~CL ()

$OMEGA BLOCK(3)
0.2   ;ETA(KA)
0.01 0.2   ;ETA(V2)
0.01 0.01 0.2   ;ETA(CL)

$SIGMA
0.05     ; 1 pro error

$EST MAXEVAL=10 METHOD=1 INTER SIGL=6 NSIG=3 PRINT=1 MSFO=./200.msf
$COV PRINT=E
$TABLE NUM CL V2 Q V3 KA ETAS(1:LAST) IPRED NPDE CWRES NOPRINT ONEHEADER FILE=200.tab
$TABLE NUM CL V2 Q V3 KA ETAS(1:LAST) NOAPPEND NOPRINT ONEHEADER FILE=200par.tab
