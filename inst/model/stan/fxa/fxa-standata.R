make_standata <- function(.dir) {
  ## get data file
  xdata <- readr::read_csv(file.path(.dir, "..", "..", "..", "extdata", "fxa.data.csv"))

  ## create data set
  data <- with(
    xdata,
    list(
      nSubjects = max(subject),
      nObs = nrow(xdata),
      subject = subject,
      cObs = cobs,
      fxa = fxa.inh.obs,
      cmin = 0,
      cmax = 1600,
      nsim = 201
    )
  )
}
