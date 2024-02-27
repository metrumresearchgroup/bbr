#' @family `NONMEM` model extensions
#' @section Notes on `NONMEM` model extensions:
#'
#' Before executing the model, `bbi` first copies the control stream file to a
#' subdirectory. Whether a relative path to the data file in the control stream
#' is adjusted for this change in directory depends on the control stream's file
#' extension:
#'
#'   * For a model with a `.mod` extension, `bbi` automatically adjusts a
#'     relative data path to make it relative to the _execution_ directory,
#'     following the behavior of `PsN`.
#'
#'   * For a model with a `.ctl` extension, `bbi` does _not_ adjust the data
#'     path. A relative data path in the control stream must be specified
#'     **one level deeper** (i.e. with an additional `../`) to account for the
#'     model being executed in a subdirectory.
