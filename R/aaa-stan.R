STANMOD_SUFFIX <- ".stan"
STANDATA_R_SUFFIX <- "-standata.R"
STANDATA_JSON_SUFFIX <- "-standata.json"
STANINIT_SUFFIX <- "-init.R"
STANARGS_SUFFIX <- "-stanargs.R"
STAN_OUTDIR_SUFFIX <- "-output"

STAN_MODEL_REQ_FILES <- c(
  STANMOD_SUFFIX,
  STANDATA_R_SUFFIX,
  STANINIT_SUFFIX
)

STAN_RESERVED_ARGS <- c(
  "data",
  "init",
  "output_dir"
)

STANCFG_DATA_MD5 <- "standata_script_md5"
STANCFG_INIT_MD5 <- "init_script_md5"
STANCFG_ARGS_MD5 <- "stanargs_md5"

STAN_BBI_VERSION_STRING <- "STAN"

############
# SCAFFOLDS
############

STANMOD_SCAFFOLD_STRING <- "data{}

parameters{}

model{}"

STANMOD_SCAFFOLD_MD5 <- "d7d6e3b78a009985f977e068e09caea1"


STANDATA_SCAFFOLD_STRING <- "make_standata <- function(.dir) {
  # read in any input data
  in_data <- readr::read_csv(file.path(.dir, '..', 'my_data.csv'))

  # do any transformations

  # return the list that can be passed to cmdstanr
}"

STANDATA_SCAFFOLD_MD5 <- "7cec129a8b1f124af1ca102feb64805a"


STANINIT_SCAFFOLD_STRING <- "make_init <- function(data) {
  # return list of initial estimates that can be passed to cmdstanr
}"

STANINIT_SCAFFOLD_MD5 <- "fcd70e5f4e7b5a88b173fe0a34cba3fd"

STAN_SCAFFOLD_MD5_VEC <- c(
  STANMOD_SCAFFOLD_MD5,
  STANDATA_SCAFFOLD_MD5,
  STANINIT_SCAFFOLD_MD5
)
