if ( requireNamespace("tinytest", quietly=TRUE) ){
  home <- (Sys.getenv('USERNAME') %in% c('vnguyen216', 'vinh')) # flag if at home; run tests involving parallel=TRUE at home only and not on CRAN
  
  if ("env" %in% names(as.list(args(utils::getFromNamespace("[.data.table", "data.table"))))) { # run test only if most recent version of data.table with dt[, env] functionality; https://stackoverflow.com/a/63023801
    tinytest::test_package('DisImpact', at_home = home)
  }
}
