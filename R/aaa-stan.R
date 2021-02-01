STANMOD_SCAFFOLD_STRING <- "data{}

parameters{}

model{}"

STANMOD_SCAFFOLD_MD5 <- "d7d6e3b78a009985f977e068e09caea1"


STANDATA_SCAFFOLD_STRING <- "standata <- function(.dir) {
  # read in any input data
  in_data <- readr::read_csv(file.path(.dir, '..', 'my_data.csv'))

  # do any transformations

  # return the list that can be passed to cmdstanr
}"

STANDATA_SCAFFOLD_MD5 <- "45f2b82bf31010ef018da0827c5314a0"


STANINIT_SCAFFOLD_STRING <- "init <- function() {
  # return list of initial estimates that can be passed to cmdstanr
}"

STANINIT_SCAFFOLD_MD5 <- "afb27da800b0fbca9105851f94a1e29d"

STAN_SCAFFOLD_MD5_VEC <- c(
  STANMOD_SCAFFOLD_MD5,
  STANDATA_SCAFFOLD_MD5,
  STANINIT_SCAFFOLD_MD5
)
