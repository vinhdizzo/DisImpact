#' Fake data on student equity
#'
#' Data randomly generated to illustrate the use of the package.
#'
#' @docType data
#'
#' @usage data(student_equity)
#'
#' @keywords datasets
#'
#' @examples
#' data(student_equity)
"student_equity"
## # Data parameters
## true.p <- c(0.4, 0.5, 0.3, 0.2, 0.7, 0.6)
## nPerGroup <- c(100, 500, 1000, 2000, 3000, 3400)
## nGroups <- length(nPerGroup); nGroups
## nEachCohort <- sum(nPerGroup); nEachCohort
## nCohorts <- 2

## # Generate toy data
## library(devtools)
## library(dplyr)
## set.seed(1)
## student_equity <- tibble(Cohort=rep(2017:2018, each=nEachCohort)
##                  , Ethnicity=rep(rep(c('Native American', 'Multi-Ethnicity', 'Black', 'Hispanic', 'Asian', 'White'), times=nPerGroup), 2)
##                  , Transfer=c(lapply(1:nGroups, function(i) sample(0:1, size=nPerGroup[i], replace=TRUE, prob=c(1-true.p[i], true.p[i]))) %>% unlist
##                               , lapply(1:nGroups, function(i) sample(0:1, size=nPerGroup[i], replace=TRUE, prob=c(1-true.p[i]*1.05, true.p[i]*1.05))) %>% unlist
##                               )
##                  , Gender=sample(x=c('Female', 'Male'), size=nCohorts*sum(nPerGroup), replace=TRUE)
##                  )

## # Export data set to ./data
## devtools::use_data(student_equity, overwrite=TRUE)

