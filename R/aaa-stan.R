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


STANDATA_SCAFFOLD_STRING <- "make_standata <- function(.dir) {
  # read in any input data
  in_data <- readr::read_csv(file.path(.dir, '..', 'my_data.csv'))
  # do any transformations
  # return the list that can be passed to cmdstanr
}"

STANDATA_SCAFFOLD_MD5 <- "7cec129a8b1f124af1ca102feb64805a"


STANINIT_SCAFFOLD_STRING <- "make_init <- function(.data) {
  # return list of initial estimates that can be passed to cmdstanr
}"

STANINIT_SCAFFOLD_MD5 <- "3a4f8bf61a135cb8c514f4814b729c14"

STAN_SCAFFOLD_MD5_VEC <- c(
  STANMOD_SCAFFOLD_MD5,
  STANDATA_SCAFFOLD_MD5,
  STANINIT_SCAFFOLD_MD5
)
