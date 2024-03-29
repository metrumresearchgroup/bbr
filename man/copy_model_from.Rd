% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/copy-model-from.R
\name{copy_model_from}
\alias{copy_model_from}
\alias{copy_model_from.bbi_nonmem_model}
\title{Create new model by copying existing model}
\usage{
copy_model_from(
  .parent_mod,
  .new_model = NULL,
  .description = NULL,
  .based_on_additional = NULL,
  .add_tags = NULL,
  .star = NULL,
  .inherit_tags = FALSE,
  .update_model_file = TRUE,
  .overwrite = FALSE
)

\method{copy_model_from}{bbi_nonmem_model}(
  .parent_mod,
  .new_model = NULL,
  .description = NULL,
  .based_on_additional = NULL,
  .add_tags = NULL,
  .star = NULL,
  .inherit_tags = FALSE,
  .update_model_file = TRUE,
  .overwrite = FALSE
)
}
\arguments{
\item{.parent_mod}{Model to copy from}

\item{.new_model}{Path to the new model, either absolute or relative to the
path to \code{.parent_mod}. Represents an absolute model path, which is the path
to the YAML file and model file, both without extension, and the output
directory (once the model is run). Numeric values will be coerced to
character. If \code{NULL}, the default, will try to increment to the next integer
in the destination directory. See examples for usage.}

\item{.description}{Character scalar description of new model run. This will
be stored in the yaml (and can be viewed later in \code{run_log()}).}

\item{.based_on_additional}{Character vector of path(s) to other models that
this model was "based on." These are used to reconstuct model developement
and ancestry. \strong{Paths must be relative to \code{.new_model} path.} Note that
the \code{.parent_model} will automatically be added to the \code{based_on} field, so
no need to include that here.}

\item{.add_tags}{Character vector with any new tags(s) to be added to
\verb{\{.new_model\}.yaml}}

\item{.star}{Boolean, marks model to indicate special interest level.}

\item{.inherit_tags}{If \code{FALSE}, the default, new model will only have any
tags passed to \code{.add_tags} argument. If \code{TRUE} inherit any tags from
\code{.parent_mod}, with any tags passed to \code{.add_tags} appended.}

\item{.update_model_file}{\strong{Only relevant to NONMEM models.} If \code{TRUE}, the
default, update the newly created model file. If \code{FALSE}, new model file
will be an exact copy of its parent. For a NONMEM model, this currently
means only the \verb{$PROBLEM} line in the new control stream will be updated to
read \verb{See \{.new_model\}.yaml. Created by bbr.}.}

\item{.overwrite}{If \code{FALSE}, the default,  function will error if a model
file already exists at specified \code{.new_model} path. If \code{TRUE} any existing
file at \code{.new_model} will be overwritten silently.}
}
\description{
Create new model by copying existing model. Useful for iterating during model
development. Also fills \code{based_on} field by default, for constructing model
ancestry. See \href{../articles/using-based-on.html}{"Using based_on field" vignette} for details.
}
\section{Methods (by class)}{
\itemize{
\item \code{copy_model_from(bbi_nonmem_model)}: \code{.parent_mod} takes a \code{bbi_nonmem_model} object to use as a basis for the copy.

}}
\examples{
\dontrun{
parent <- read_model("/foo/parent")

# create model file at /bar/child.ctl and YAML at /bar/child.yaml
copy_model_from(parent, "/bar/child", "child model with absolute path")

# create model file at /foo/child.ctl and YAML at /foo/child.yaml
copy_model_from(parent, "child", "relative to parent model path")

mod1 <- read_model("/path/to/1")

# create model file at /path/to/2.ctl and YAML at /path/to/2.yaml
copy_model_from(mod1, "increments to next integer by default")

# create model file at /path/to/3.ctl and YAML at /path/to/3.yaml
copy_model_from(mod1, 3, "numeric input works")

# create model file at /path/to/100.1.ctl and YAML at /path/to/100.1.yaml
copy_model_from(mod1, 100.1, "a period is okay")
}
}
\seealso{
\code{\link[=new_model]{new_model()}}, \code{\link[=update_model_id]{update_model_id()}}
}
