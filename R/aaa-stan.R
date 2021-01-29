STANMOD_SCAFFOLD_STRING <- "data{}

parameters{}

model{}"

STANMOD_SCAFFOLD_MD5 <- "d7d6e3b78a009985f977e068e09caea1"


STANDATA_SCAFFOLD_STRING <- "standata <- function() {
  # read in any input data

  # do any transformations

  # return the list that can be passed to cmdstanr
}"

STANDATA_SCAFFOLD_MD5 <- "5ae61eeedc4ef564d041f2d18a7e5fb5"


STANINIT_SCAFFOLD_STRING <- "init <- function() {
  # return list of initial estimates that can be passed to cmdstanr
}"

STANINIT_SCAFFOLD_MD5 <- "afb27da800b0fbca9105851f94a1e29d"

STAN_SCAFFOLD_MD5_VEC <- c(
  STANMOD_SCAFFOLD_MD5,
  STANDATA_SCAFFOLD_MD5,
  STANINIT_SCAFFOLD_MD5
)
