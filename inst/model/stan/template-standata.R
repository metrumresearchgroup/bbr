make_standata <- function(.dir) {

  ## get data file

  xdata <- file.path(.dir, "..", "..", "..", "extdata", "fxa.data.csv") %>%
    readr::read_csv() %>%
    select(subject, {{cov}}, cobs, fxa.inh.obs)

  ## create data set
  data <- with(
    xdata,
    list(
      nSubjects = max(subject),
      nObs = nrow(xdata),
      subject = subject,
      {{cov}} = {{cov}},
      cObs = cobs,
      fxa = fxa.inh.obs,
      cmin = 0,
      cmax = 1600,
      nsim = 201
    )
  )
}
