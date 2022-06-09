Rcpp::loadModule("movenet_module", TRUE)

.onLoad <- function(libname, pkgname){

  load_config("ScotEID")

}
