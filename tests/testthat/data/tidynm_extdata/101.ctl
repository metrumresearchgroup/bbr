$PROB RUN# 101 - fit of Phase I data - full model
$INPUT C ID SUBJ TIME SEQ CMT EVID AMT DV AGE WT CRCL ALB BMI AAG
       SCR AST ALT HT CP TAFD TAD LDOS MDV BLQ PHASE
$DATA ../analysis1.csv IGNORE=(C='C', BLQ=1)
$SUB ADVAN13 TOL=9

$MODEL
 COMP=(COMP1,DEFDOSE)
 COMP=(COMP2,DEFOBS)
 COMP=(COMP3)

$PK

;log transformed PK parms

  V2WT  = LOG(WT/70)
  CLWT = LOG(WT/70)*0.75
  V3WT  = LOG(WT/70)
  QWT = LOG(WT/70)*0.75


; CRCL effect on CL
  RFN=0
  IF(CRCL.LT.80) RFN=LOG(CRCL/80)*THETA(6) ; RF effect only wneh CRCL < 80 ml/min/1.73m^2

; Child-Pugh (CP) category on CL
  LIV=0
  IF(CP.EQ.3)LIV=THETA(7) ; liver function only important for bad liver (CP=3)

  KA   = EXP(THETA(1)+ETA(1))
  V2   = EXP(THETA(2)+V2WT+ETA(2))
  CL   = EXP(THETA(3)+CLWT+RFN+LIV+ETA(3))
  V3   = EXP(THETA(4)+V3WT)
  Q    = EXP(THETA(5)+QWT)

  K=CL/V2
  K12=KA
  K23=Q/V2
  K32=Q/V3
  S2 = V2/1000 ; dose in mcg, conc in mcg/mL

$DES
 CONC=A(2)/V2
 DADT(1)=-KA*A(1)
 DADT(2)= KA*A(1) - K23*A(2) + K32*A(3) - K*A(2)
 DADT(3)= K23*A(2) - K32*A(3)


$ERROR
 IPRED = F
 Y=IPRED*(1+EPS(1))+EPS(2)

$THETA
(0.5)    ;  [1/hr] KA
(3.5)    ;  [L]    V1
(1)      ;  [L/hr] CL
(4)      ;  [L] V2
(2)      ;  [L/hr] Q
(0.4)    ;  [f] Renal
(0.01)   ;  [f] Child-Pugh LOGD
; dev comment - the LOGD ^ was added to test parsing, not by the real model

$OMEGA BLOCK(3)
        0.2        ; [P]  Ka
        0.003
        0.04        ; [P]  CL
        0.002
        0.001
        0.05        ; [P]  V2

$SIGMA
0.05     ; [P] Prop
2        ; [A] Add

$EST MAXEVAL=9999 METHOD=1 INTER SIGL=9 NSIG=3 PRINT=1 MSFO=./101.msf
$COV UNC
$TABLE ID TIME CMT EVID AMT TAFD TAD DV IPRED CWRESI NOPRINT ONEHEADER FILE=./101.tab
$TABLE ID TIME EVID AGE WT CRCL CL V2 KA ETA1 ETA2 ETA3 NOAPPEND NOPRINT ONEHEADER FILE=./101par.tab

