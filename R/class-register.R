
mod_class_registry <- new.env(parent = emptyenv())
sum_class_registry <- new.env(parent = emptyenv())

select_registry <- function(class_type) {
  switch(class_type,
         model = mod_class_registry,
         summary = sum_class_registry,
         stop("unknown class type: ", class_type))
}

# SHARED: register_model_type() is used by bbr.bayes, so any changes here should
# be compatible with its use there.

#' Register a model type to class mapping
#'
#' Define how to map a model type to model or summary object names when the
#' default mapping of `{type}` to `bbi_{type}_model` or `bbi_{type}_summary`
#' won't do. The primary use for this function is registering custom parent
#' types.
#'
#' @details
#'
#' [create_model_object()] and [create_summary_object()] are responsible for
#' creating model and summary objects for different model types (e.g.,
#' "bbi_nonmem_model" and "bbi_nonmem_summary" for "nonmem" model types). These
#' functions get the class name from `model_type_to_classes()`, which by default
#' constructs the class from the model type (e.g., "nonmem" is mapped to
#' `c("bbi_nonmem_model", ...)`).
#'
#' That default value doesn't work if you want to define custom parents. For
#' example, `bbr.bayes` package wants to map "nmbayes" model type to
#' `c("bbi_nmbayes_model", "bbi_nonmem_model", ...)` so that `bbi_nmbayes_model`
#' objects can fall back to `.bbi_nonmem_model` methods where appropriate. In
#' these cases, a package can call `register_model_type()` within its
#' [.onLoad()] to "register" the custom classes.
#'
#' @examples
#' \dontrun{
#' register_model_type("foo",
#'                     c("bbi_foo_model", "bbi_nonmem_model"),
#'                     "model")
#' register_model_type("foo",
#'                     c("bbi_foo_summary", "bbi_nonmem_summary"),
#'                     "summary")
#' }
#'
#' @param model_type Model type, as recorded in the model YAML file.
#' @param classes One or more classes to use for `model_type`.
#' @param class_type The kind of object, "model" or "summary".
#' @keywords internal
register_model_type <- function(model_type,
                                classes,
                                class_type = c("model", "summary")) {
  checkmate::assert_character(classes)
  class_type <- match.arg(class_type)
  assign(model_type, classes, envir = select_registry(class_type))
}

model_type_to_classes <- function(model_type,
                                  class_type = c("model", "summary")) {
  class_type <- match.arg(class_type)
  get0(model_type, envir = select_registry(class_type)) %||%
    as.character(glue("bbi_{model_type}_{class_type}"))
}
