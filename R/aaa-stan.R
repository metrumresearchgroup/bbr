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
