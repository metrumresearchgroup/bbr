
list(
  input_ctl = "$PROBLEM theta bounds and value
$THETA
(-10, 2.1, 10)    ; CL LOGD 1
(-10, 3.3, 10)    ; V2 LOGD 2
(-10, 1.4, 10)    ; Q LOGD 3
(-10, 4, 10)      ; V3 LOGD 4
(-10, 1.3, 10)    ; KA LOGD 7
(-10, 10)         ; D1 LOGD 8
(0.75 FIX)        ; WT_CL 9
(1 FIX)           ; WT_VC 10
(0.75 FIX)        ; WT_Q 11
(1 FIX)           ; WT_VP 12
(-1)              ; FED_KA 13
(1)               ; FED_D1 14
(-0.1)            ; RACE_OTHER 15
(-0.3)            ; RACE_CHN 16
(-0.3)            ; RACE_JPN 17
(0.4)             ; EGFR_CL 18
(-0.3)            ; CTPA_CL 19
(-0.3)            ; CTPB_CL 20",
replacement = seq(1, 18, by = 1),
input_args = list(.bounds_opts = "maintain_bounds"),
result_ctl = "$PROBLEM theta bounds and value
$THETA
(-10, 1, 10)    ; CL LOGD 1
(-10, 2, 10)    ; V2 LOGD 2
(-10, 3, 10)    ; Q LOGD 3
(-10, 4, 10)      ; V3 LOGD 4
(-10, 4, 10)    ; KA LOGD 7
(-10, 6, 10)         ; D1 LOGD 8
(7 FIX)        ; WT_CL 9
(8 FIX)           ; WT_VC 10
(9 FIX)        ; WT_Q 11
(10 FIX)           ; WT_VP 12
(11)              ; FED_KA 13
(12)               ; FED_D1 14
(13)            ; RACE_OTHER 15
(14)            ; RACE_CHN 16
(15)            ; RACE_JPN 17
(16)             ; EGFR_CL 18
(17)            ; CTPA_CL 19
(18)            ; CTPB_CL 20"
)

