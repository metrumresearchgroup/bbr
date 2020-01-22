.onLoad <- function(libname, pkgname) {
   if (is.null(getOption("rbabylon.bbi_exe_path"))) {
     options("rbabylon.bbi_exe_path" = "bbi")
   }
}
