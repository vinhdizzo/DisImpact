if ( requireNamespace("tinytest", quietly=TRUE) ){
  home <- (Sys.getenv('USERNAME') %in% c('vnguyen216', 'vinh')) # flag if at home; run tests involving parallel=TRUE at home only and not on CRAN
  
  if (packageVersion('data.table') >= '1.14.3') { # run test only if most recent version of data.table with dt[, env] functionality
    tinytest::test_package('DisImpact', at_home = home)
  }
}
