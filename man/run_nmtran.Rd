% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run-nmtran.R
\name{run_nmtran}
\alias{run_nmtran}
\title{Interface for running \code{NM-TRAN} on model objects}
\usage{
run_nmtran(
  .mod,
  .bbi_args = NULL,
  .config_path = NULL,
  run_dir = tempdir(),
  clean = TRUE
)
}
\arguments{
\item{.mod}{A \code{bbi_nonmem_model} object.}

\item{.bbi_args}{A named list specifying arguments to pass to \code{NM-TRAN}.
Similar to the \code{.bbi_args} argument defined in \code{\link[=submit_model]{submit_model()}}, though here
only \code{prdefault}, \code{tprdefault}, and \code{maxlim} arguments are passed to \code{NM-TRAN}.
\code{nm_version} is also supported and specifies which \code{NM-TRAN} executable to use.
See \code{\link[=print_bbi_args]{print_bbi_args()}} for more details.}

\item{.config_path}{Path to a bbi configuration file. If \code{NULL}, the
default, will attempt to use a \code{bbi.yaml} in the same directory as the
model.}

\item{run_dir}{Directory to run \code{NM-TRAN} in. Only relevant if \code{clean = FALSE}.}

\item{clean}{Logical (\code{T}/\code{F}). If \code{FALSE}, don't delete the temporary directory
containing the \code{NM-TRAN} run.}
}
\value{
An S3 object of class \code{nmtran_process}
}
\description{
Function to run \code{NM-TRAN} on a model object to validate its control stream
for correct coding before submission. The \code{NM-TRAN} dataset (\code{FDATA}) and
other \code{NONMEM} artifacts can be further inspected by keeping the run directory
around.
}
\details{
\code{NM-TRAN} is a preprocessor for \code{NONMEM} that translates user-specified
control stream data and instructions into a form executable by \code{NONMEM}.

Note that \code{nmtran_presort} is run ahead of \code{NM-TRAN} for \verb{NONMEM 7.4} and later
\itemize{
\item \code{nmtran_presort} is a supplementary utility that preprocesses the control
stream to ensure it is in the correct format for \code{NM-TRAN}.
}
}
\examples{
\dontrun{

mod <- read_model(file.path(MODEL_DIR, 1))
run_nmtran(mod, .bbi_args = list(nm_version = "nm74gf"))

# Save the run directory for manual inspection
run_nmtran(mod, clean = FALSE, run_dir = getwd())

}

}
