CACHE_ENV <- new.env(parent = emptyenv())
CACHE_ENV$bbi_exe_paths <- list()

NONMEM_ARGS = list(
  cache_dir = list(type = "character", flag="--cacheDir"),
  cache_exe = list(type = "character", flag="--cacheExe"),
  clean_lvl = list(type = "numeric", flag="--cleanLvl"),
  config = list(type = "character", flag="--config"),
  copy_lvl = list(type = "numeric", flag="--copyLvl"),
  debug = list(type = "logical", flag="--debug"),
  delay = list(type = "numeric", flag="--delay"),
  git = list(type = "logical", flag="--git"),
  gitignore_lvl = list(type = "numeric", flag="--gitignoreLvl"),
  json = list(type = "logical", flag="--json"),
  mpi_exec_path = list(type = "character", flag="--mpiExecPath"),
  nm_version = list(type = "character", flag="--nmVersion"),
  no_cor_file = list(type = "logical", flag="--no-cor-file"),
  no_cov_file = list(type = "logical", flag="--no-cov-file"),
  no_ext_file = list(type = "logical", flag="--no-ext-file"),
  no_grd_file = list(type = "logical", flag="--no-grd-file"),
  no_shk_file = list(type = "logical", flag="--no-shk-file"),
  nodes = list(type = "numeric", flag="--nodes"),
  output_dir = list(type = "character", flag="--outputDir"),
  overwrite = list(type = "logical", flag="--overwrite"),
  parafile = list(type = "character", flag="--parafile"),
  parallel = list(type = "logical", flag="--parallel"),
  preview = list(type = "logical", flag="--preview"),
  saveConfig = list(type = "logical", flag="--saveConfig"),
  save_exe = list(type = "character", flag="--saveExe"),
  threads = list(type = "numeric", flag="--threads"),
  timeout = list(type = "numeric", flag="--timeout"),
  verbose = list(type = "logical", flag="--verbose")
)
