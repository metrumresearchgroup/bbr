NONMEM_ARGS = list(
  nonmem_version = list(type = "character", flag="--nmVersion"),
  threads = list(type = "numeric", flag="--threads"),
  overwrite = list(type = "logical", flag="--overwrite"),
  debug = list(type = "logical", flag="--debug"),
  json = list(type = "logical", flag="--json"),
  no_cor_file = list(type = "logical", flag="--no-cor-file"),
  no_cov_file = list(type = "logical", flag="--no-cov-file"),
  no_ext_file = list(type = "logical", flag="--no-ext-file"),
  no_grd_file = list(type = "logical", flag="--no-grd-file"),
  no_shk_file = list(type = "logical", flag="--no-shk-file"),
  preview = list(type = "logical", flag="--preview"),
  verbose = list(type = "logical", flag="--verbose")
)
