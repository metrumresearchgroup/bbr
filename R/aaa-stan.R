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

############
# SCAFFOLDS
############

STANMOD_SCAFFOLD_STRING <- "//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] y;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real mu;
  real<lower=0> sigma;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  y ~ normal(mu, sigma);
}"

STANMOD_SCAFFOLD_MD5 <- "cb1c31e0f34cd0c196b64b6cd5492669"


STANDATA_SCAFFOLD_STRING <- "# Create Stan data
#
# This function must return the list that will be passed to `data` argument
#   of `cmdstanr::sample()`
#
# The `.dir` argument represents the absolute path to the directory containing
#   this file. This is useful for building file paths to the input files you will
#   load. Note: you _don't_ need to pass anything to this argument, you only use
#   it within the function. `bbr` will pass in the correct path when it calls
#   `make_standata()` under the hood.
make_standata <- function(.dir) {
  # read in any input data
  in_data <- readr::read_csv(file.path(.dir, '..', 'my_data.csv'))
  # do any transformations
  # return the list that can be passed to cmdstanr
}"

STANDATA_SCAFFOLD_MD5 <- "44721f8445919647cc59ecc3ecc44072"


STANINIT_SCAFFOLD_STRING <- "# Create Stan initial values
#
# This function must return something that can be passed to the `init` argument
#   of `cmdstanr::sample()`. There are several options; see `?cmdstanr::sample`
#   for details.
#
# The `.data` represents the list returned from `make_standata()` for this model.
#   This is provided in case any of your initial values are dependant on some
#   aspect of the data (e.g. the number of rows). Note: you _don't_ need to pass
#   anything to this argument, you only use it within the function. `bbr` will
#   pass in the correct data when it calls `make_init()` under the hood.
make_init <- function(.data) {
  # return list of initial estimates that can be passed to cmdstanr
}"

STANINIT_SCAFFOLD_MD5 <- "72b7704cd334fbecf8d7b8ace7138629"

STAN_SCAFFOLD_MD5_VEC <- c(
  STANMOD_SCAFFOLD_MD5,
  STANDATA_SCAFFOLD_MD5,
  STANINIT_SCAFFOLD_MD5
)
