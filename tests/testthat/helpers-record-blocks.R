

#' Function for making an example matrix for replacement
#'
#' @param n size of the matrix (n x n)
#' @param values vector of values to set. All others will be 0.
#' @param block_loc vector indicating the location of the values. Should be the
#'        same length as `values` and created with `block()`
#'
#' @example
#' \dontrun{
#' make_matrix(
#'  n = 30,
#'  values = c(c(0.1, 0.01, 0.1), c(0, 0), c(0.1, 0.01, 0.12), rep(0.1, 12), rep(0, 12)) + 1,
#'  block_loc = c(block(2), rep(block(1), 2), block(2), rep(block(1), 12), rep(block(1), 12))
#' )
#' }
#'
make_matrix <- function(n, values, block_loc) {

  if(length(values) != length(block_loc)){
    stop("`values` and `block_loc` should have the same length")
  }

  index_strings <- build_matrix_indices(block_loc)

  # matrix setup and spec
  mat <- matrix(0, n, n)
  mat_spec <- purrr::map2_dfr(index_strings, values, function(index_str, val) {
    indices <- as.numeric(unlist(strsplit(gsub("\\(|\\)", "", index_str), ",")))
    return(tibble::tibble(row=indices[1], col=indices[2], value = val))
  })

  # Iterate through the rows of mat_spec and assign values to the matrix
  for (i in 1:nrow(mat_spec)) {
    mat[mat_spec$row[i], mat_spec$col[i]] <- mat_spec$value[i]
  }

  return(mat)
}

# THETA -------------------------------------------------------------------

theta_block_lst <- list(
  "base case" = "$THETA
(0, 2)  ; KA
(0, 3)  ; CL
(0, 10) ; V2
(0.02)  ; RUVp
(1)     ; RUVa",

"fix theta block" = "$THETA
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

  template_lines <- glue::glue("$PROBLEM {.prob}\n\n{.block}") %>%
    as.character() %>% strsplit("\n") %>% unlist()

  ctl <- nmrec::parse_ctl(template_lines)

  return(ctl)
}


# Create nmrec records
PARSED_BLOCKS <- all_blocks %>% dplyr::mutate(
  ctl = purrr::map2(.prob, .block, ~ make_fake_ctl(.x, .y))
)


get_example_record <- function(
    .prob_name = NULL,
    .id = NULL,
    .record_name = c("any", "theta", "omega", "sigma", "combine"),
    .pull_record = TRUE,
    .record_blocks_df = PARSED_BLOCKS
){

  .record_name <- match.arg(.record_name)
  records <- .record_blocks_df

  if(.record_name != "any"){
    records <- records %>% dplyr::filter(.record_type == .record_name)
  }

  if(!is.null(.id)){
    checkmate::assert_true(all(.id %in% .record_blocks_df$.prob_id))
    records <- records %>% dplyr::filter(.prob_id %in% .id)
  }

  if(!is.null(.prob_name)){
    records <- records %>% dplyr::filter(grepl(.prob_name, .prob, fixed = TRUE))
  }

  cli::cli_div(theme = list(span.emph = list(color = "red"), span.code = list(color = "blue")))

  if(isTRUE(.pull_record) && nrow(records) == 1){
    records %>% dplyr::pull("ctl")
  }else if(isTRUE(.pull_record) && nrow(records) < 1 && !is.null(.prob_name)){
    msg <- glue::glue("No records matching {.emph '{{.prob_name}}'} for {.code {{.record_name}}} records",
                      .open = "{{", .close = "}}")
    cli::cli_abort(c("x" = msg))
  }else if(isTRUE(.pull_record) && nrow(records) > 1 && !is.null(.prob_name)){
    msg <- glue::glue("Multiple records matching {.emph '{{.prob_name}}'} for {.code {{.record_name}}} records",
                      .open = "{{", .close = "}}")
    cli::cli_warn(c("!" = msg))
    return(records)
  }else{
    return(records)
  }
}


#' Helper function for getting record lengths
#'
#' Helpful for setting arbitrary replacements of the same length
example_rec_lengths <- function(
    .record,
    .record_name = "theta",
    .return_replacement = TRUE
){
  rec <- nmrec::select_records(.record[[1]], .record_name)

  rec_lengths <- purrr::map(rec, function(rec_block){
    rec_block$parse()
    if(.record_name != "omega"){
      val_recs <- purrr::keep(rec_block$values, function(rec_opt){
        inherits(rec_opt, "nmrec_option") && !inherits(rec_opt, "nmrec_option_record_name")
      })
    }else{
      val_recs <- purrr::keep(omega_rec$values, function(rec_opt){
        inherits(rec_opt, "nmrec_option") && !inherits(rec_opt, c("nmrec_option_record_name")) &&
          !inherits(rec_opt, c("nmrec_option_value"))
      })
    }
    length(val_recs)
  })

  if(isTRUE(.return_replacement)){
    return(purrr::map(rec_lengths, \(len) seq(1:len)))
  }else{
    return(rec_lengths)
  }
}
