init <- function(data) {
  function() {
    list(
      emax = runif(1, 40, 100),
      ec50Hat = exp(rnorm(1, log(100), 1)),
      gamma = runif(1, 0.25, 3),
      sigma = exp(rnorm(1, log(10), 1)),
      omegaEc50 = exp(rnorm(1, log(0.2), 1)),
      eta = rep(0.0, data$nSubjects)
    )

  }
}
