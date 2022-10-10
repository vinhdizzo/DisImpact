## library(devtools)
## library(roxygen2)
## setwd('DisImpact')
## document()
## setwd('vignettes')
## knitr::knit("Scaling-DI-Calculations.Rmd.orig", output = "Scaling-DI-Calculations.Rmd") # Manually build one of the vignettes here in order to pass CRAN https://ropensci.org/blog/2019/12/08/precompute-vignettes/
## setwd('..') # back to DisImpact
## devtools::check(cran=TRUE)
## devtools::build()
## devtools::build(vignettes=FALSE)
## setwd('..')
## install('DisImpact')
## tinytest::build_install_test('./DisImpact') # from outside
## install.packages("../DisImpact_0.0.9000.tar.gz", repos = NULL, type = "source") # install
## tinytest::test_package('DisImpact', at_home = TRUE)

.onAttach <- function(libname, pkgname) {
  # https://r-pkgs.org/Code.html#sec-code-r-landscape
  
  # Need data.table >= 1.14.3
  if(requireNamespace("data.table", quietly=TRUE) &&
    !("env" %in% names(as.list(args(utils::getFromNamespace("[.data.table", "data.table")))))) {
    packageStartupMessage("`di_iterate_dt` requires a new version of data.table.  Please execute the following to update data.table to the current development version:\n\n    detach('package:DisImpact', unload=TRUE) # unload package\n\n    data.table::update.dev.pkg()\n\nIf this doesn't work, try:\n\n    install.packages('data.table', repo = 'https://Rdatatable.gitlab.io/data.table') # Select 'Yes' if asked to install from source.\n\nIf the previous doesn't work, you can manually download the package (.tar.gz, .zip, or .tgz) at https://rdatatable.gitlab.io/data.table/web/packages/data.table/index.html and install it yourself.")
  }

  invisible()
}
