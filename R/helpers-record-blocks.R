

# THETA -------------------------------------------------------------------

theta_block_lst <- list(
  "base case" = "$THETA
(0, 2)  ; KA
(0, 3)  ; CL
(0, 10) ; V2
(0.02)  ; RUVp
(1)     ; RUVa",

"priors (P/PV not specified)" = "$THETA
( 0.7 ) ;[LCLM]
( 0.7 ) ;[LCLF]
( 2 )   ;[CLAM]
( 2.0);[CLAF]
( 0.7 ) ;[LV1M]
( 0.7 ) ;[LV1F]
( 2.0 )   ;[V1AM]
( 2.0 )   ;[V1AF]
( 0.7 ) ;[MU_3]
(  0.7 );[MU_4]
( 0.3 )     ;[SDSL]

$THETA 4 FIX",

"priors complex" = "$THETA
-0.574219127214817 ;[1/year] KOUT LOGD
5.67154052944669 ;[meters/year] KIN LOGD
1.03568637508119 ;[na] KCOV LOGD
0.241769673098703 ;[years] MTIME LOGD
-11.3632385211334 ;[na] Scalar LOGD
5.11272713931817 ;[na] EXPON LOGD

$THETAP ;prior mode for thetas
  -0.676 FIX
  5.8 FIX
  0.921 FIX
  0.390 FIX
  -11.4 FIX
  5 FIX

$THETAPV BLOCK(6) ;var-cov for prior on thetas --> uncertainty for thetas     *uncertainty
  10 FIX
  0  10
  0  0  10
  0  0  0  0.0625
  0  0  0  0  10
  0  0  0  0  0  2"
)

theta_blocks <- theta_block_lst %>% tibble::as_tibble() %>%
  tidyr::pivot_longer(everything(), names_to = ".prob", values_to = ".block")

# OMEGA -------------------------------------------------------------------

omega_block_lst <- list(
  "base case" = "$OMEGA
0.05    ; iiv CL
0.2     ; iiv V2",

"multiple mixed block 1" = "$OMEGA
0.05    ; iiv CL
0.2     ; iiv V

$OMEGA BLOCK(1) 0.2 ; iov CL
$OMEGA BLOCK(1) SAME(29)
$OMEGA BLOCK(1) 0.2 ; iov V
$OMEGA BLOCK(1) SAME(29)",

"multiple mixed block 2" = "$OMEGA
(0.04) ;1. CL VAR
(0.04) ;2. V VAR
;IOV
$OMEGA BLOCK(1)  0.04; interoccasion var in CL
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME",

"priors (P/PV not specified)" = "$OMEGA BLOCK(4)
0.5  ;[p]
0.001  ;[f]
0.5  ;[p]
0.001 ;[f]
0.001 ;[f]
0.5  ;[p]
0.001 ;[f]
0.001 ;[f]
0.001 ;[f]
0.5 ;[p]

$OMEGA BLOCK(4)
0.01 FIX
0.0  0.01
0.0 0.0 0.01
0.0 0.0 0.0 0.01",

"priors complex" = "$OMEGA
0.09          ;[P] KOUThealthy

$OMEGA BLOCK (3)
    0.09          ;[P] KOUTdmd
    0.01          ;[P] cov21
    0.09          ;[P] KINdmd
    0.01          ;[P] cov31
    0.01          ;[P] cov32
    0.09          ;[P] SCALAR

$OMEGAP
  0.09 FIX

$OMEGAP BLOCK(3)
  0.09 FIX
  0.01 0.09
  0.01 0.01 0.09

$OMEGAPD (1 FIX)(3 FIX); df for each $omega block",

"many blocks" = "$OMEGA BLOCK(2)
0.1                               ; [P] CL 1
0.01 0.1                          ; [P] V3 2
;0.01 0.01 0.1                     ;
;0.01 0.01 0.01 0.1                ;

$OMEGA
0 FIX
0 FIX

$OMEGA BLOCK(2)
0.1                               ; [P] KA 5
0.01 0.12                         ; [P] D1 6

$OMEGA  BLOCK(1)  0.1                               ; IOV-F1 7
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1)  SAME
$OMEGA BLOCK(1)  SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME"
)

omega_blocks <- omega_block_lst %>% tibble::as_tibble() %>%
  tidyr::pivot_longer(everything(), names_to = ".prob", values_to = ".block")

# SIGMA -------------------------------------------------------------------

sigma_block_lst <- list(
  "base case" = "$SIGMA
0.2 ; [P]",

"base case fixed" = "$SIGMA
(1.0 FIXED)",

"base case fixed 2" = "$SIGMA
1 FIX",

"multiple lines" = "$SIGMA
0.04
10",

"priors complex" = ""
)

sigma_blocks <- sigma_block_lst %>% tibble::as_tibble() %>%
  tidyr::pivot_longer(everything(), names_to = ".prob", values_to = ".block")

# Combination -------------------------------------------------------------


combine_block_lst <- list(
  "combine base" = paste(theta_block_lst$`base case`,
                         omega_block_lst$`base case`,
                         sigma_block_lst$`base case`,
                         sep = "\n\n")
)

combine_blocks <- combine_block_lst %>% tibble::as_tibble() %>%
  tidyr::pivot_longer(everything(), names_to = ".prob", values_to = ".block")




# Create nmrec records ----------------------------------------------------


all_blocks <- dplyr::bind_rows(
  theta_blocks %>% dplyr::mutate(.record_type = "theta"),
  omega_blocks %>% dplyr::mutate(.record_type = "omega"),
  sigma_blocks %>% dplyr::mutate(.record_type = "sigma"),
  combine_blocks %>% dplyr::mutate(.record_type = "combine")
) %>% dplyr::mutate(.prob_id = 1:n()) %>%
  dplyr::relocate(".record_type", ".prob_id")


make_fake_ctl <- function(
    .prob = "PK model 1 cmt base",
    .block = NULL
){

  template_lines <- glue::glue("$PROBLEM {.prob}

$INPUT ID TIME MDV EVID DV AMT  SEX WT ETN NUM
$DATA ../../../../extdata/acop.csv IGNORE=@
IGNORE(ID.EQ.2)

$SUBROUTINES ADVAN2 TRANS2
$PK
ET=1
IF(ETN.EQ.3) ET=1.3
KA = THETA(1)
CL = THETA(2)*((WT/70)**0.75)* EXP(ETA(1))
V = THETA(3)*EXP(ETA(2))
SC=V

{.block}

$OMEGA
0.05    ; iiv CL
0.2     ; iiv V2

$SIGMA
1 FIX

$ERROR
IPRED = F
IRES = DV-IPRED
W = IPRED*THETA(4) + THETA(5)
IF (W.EQ.0) W = 1
IWRES = IRES/W
Y= IPRED+W*ERR(1)
FAKE=1 ; for testing

$EST METHOD=1 INTERACTION MAXEVAL=9999 SIG=3 PRINT=5 NOABORT POSTHOC
$COV
$TABLE NUM IPRED NPDE CWRES NOPRINT ONEHEADER FILE=1.tab
")

  # Return nmrec object
  return(nmrec::parse_ctl(template_lines))
}


# Create nmrec records
PARSED_BLOCKS <- all_blocks %>% dplyr::mutate(
  ctl = purrr::map2(.prob, .block, ~ make_fake_ctl(.x, .y))
)


get_example_record <- function(
    .prob_name = NULL,
    .id = NULL,
    .record = c("any", "theta", "omega", "sigma", "combine"),
    .pull_record = TRUE,
    .record_blocks_df = PARSED_BLOCKS
){

  .record <- match.arg(.record)
  records <- .record_blocks_df

  if(.record != "any"){
    records <- records %>% dplyr::filter(.record_type == .record)
  }

  if(!is.null(.id)){
    checkmate::assert_true(all(.id %in% .record_blocks_df$.prob_id))
    records <- records %>% dplyr::filter(.prob_id %in% .id)
  }

  if(!is.null(.prob_name)){
    records <- records %>% dplyr::filter(grepl(.prob_name, .prob))
  }

  cli::cli_div(theme = list(span.emph = list(color = "red"), span.code = list(color = "blue")))

  if(isTRUE(.pull_record) && nrow(records) == 1){
    records %>% dplyr::pull("ctl")
  }else if(isTRUE(.pull_record) && nrow(records) < 1 && !is.null(.prob_name)){
    msg <- glue::glue("No records matching {.emph '{{.prob_name}}'} for {.code {{.record}}} records",
                      .open = "{{", .close = "}}")
    cli::cli_abort(c("x" = msg))
  }else if(isTRUE(.pull_record) && nrow(records) > 1 && !is.null(.prob_name)){
    msg <- glue::glue("Multiple records matching {.emph '{{.prob_name}}'} for {.code {{.record}}} records",
                      .open = "{{", .close = "}}")
    cli::cli_warn(c("!" = msg))
    return(records)
  }else{
    return(records)
  }
}



nmrec::select_records(PARSED_BLOCKS$ctl[[1]], "theta")
