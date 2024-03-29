#' Fake data on student equity
#'
#' Data randomly generated to illustrate the use of the package.
#' @format A data frame with 20,000 rows:
#' \describe{
#'   \item{Ethnicity}{ethnicity (one of: \code{Asian}, \code{Black}, \code{Hispanic}, \code{Multi-Ethnicity}, \code{Native American}, \code{White}).}
#'   \item{Gender}{gender (one of: \code{Male}, \code{Female}, \code{Other}).}
#'   \item{Cohort}{year student first enrolled in any credit course at the institution (one of: \code{2017}, \code{2018}).}
#'   \item{Transfer}{1 or 0 indicating whether or not a student transferred within 2 years of first enrollment (\code{Cohort}).}
#'   \item{Cohort_Math}{year student first enrolled in a math course at the institution; could be \code{NA} if the student have not attempted math.}
#'   \item{Math}{1 or 0 indicating whether or not a student completed transfer-level math within 1 year of their first math attempt (\code{Cohort_Math}); could be \code{NA} if the student have not attempted math.}
#'   \item{Cohort_English}{year student first enrolled in a math course at the institution; could be \code{NA} if the student have not attempted math.}
#'   \item{English}{1 or 0 indicating whether or not a student completed transfer-level English within 1 year of their first math attempt (\code{Cohort_English}); could be \code{NA} if the student have not attempted English.}
#'   \item{Ed_Goal}{student's educational goal (one of: \code{Deg/Transfer}, \code{Other}).}
#'   \item{College_Status}{student's educational status (one of: \code{First-time College},  \code{Other}).}
#'   \item{Student_ID}{student's unique identifier.}
#'   \item{EthnicityFlag_Asian}{1 (yes) or 0 (no) indicating whether or not a student self-identifies as Asian.}
#'   \item{EthnicityFlag_Black}{1 (yes) or 0 (no) indicating whether or not a student self-identifies as Black.}
#'   \item{EthnicityFlag_Hispanic}{1 (yes) or 0 (no) indicating whether or not a student self-identifies as Hispanic.}
#'   \item{EthnicityFlag_NativeAmerican}{1 (yes) or 0 (no) indicating whether or not a student self-identifies as Native American.}
#'   \item{EthnicityFlag_PacificIslander}{1 (yes) or 0 (no) indicating whether or not a student self-identifies as Pacific Islander.}
#'   \item{EthnicityFlag_White}{1 (yes) or 0 (no) indicating whether or not a student self-identifies as White.}
#'   \item{EthnicityFlag_Carribean}{1 (yes) or 0 (no) indicating whether or not a student self-identifies as Carribean.}
#'   \item{EthnicityFlag_EastAsian}{1 (yes) or 0 (no) indicating whether or not a student self-identifies as East Asian.}
#'   \item{EthnicityFlag_SouthEastAsian}{1 (yes) or 0 (no) indicating whether or not a student self-identifies as Southeast Asian.}
#'   \item{EthnicityFlag_SouthWestAsianNorthAfrican}{1 (yes) or 0 (no) indicating whether or not a student self-identifies as Southwest Asian / North African (SWANA).}
#'   \item{EthnicityFlag_AANAPI}{1 (yes) or 0 (no) indicating whether or not a student self-identifies as Asian-American or Native American Pacific Islander (AANAPI).}
#'   \item{EthnicityFlag_Unknown}{1 (yes) or 0 (no) indicating whether or not a student self-identifies as Unknown.}
#'   \item{EthnicityFlag_TwoorMoreRaces}{1 (yes) or 0 (no) indicating whether or not a student self-identifies as two or more races.}
#' }
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
##                  , Math=ifelse(Transfer==1, 1, sample(0:1, size=length(Transfer), replace=TRUE, prob=c(0.5, 0.5)))
##                  , English=ifelse(Transfer==1, 1, sample(0:1, size=length(Transfer), replace=TRUE, prob=c(0.6, 0.4)))
##                  , Gender=sample(x=c('Female', 'Male', 'Other'), size=nCohorts*sum(nPerGroup), replace=TRUE, prob=c(0.49, 0.49, 0.02))
##                  , Ed_Goal=sample(x=c('Deg/Transfer', 'Other'), size=nCohorts*sum(nPerGroup), replace=TRUE, prob=c(0.7, 0.3))
##                  , College_Status=sample(x=c('First-time College', 'Other'), size=nCohorts*sum(nPerGroup), replace=TRUE, prob=c(0.8, 0.2))
##                  ) %>%
##   mutate(
##     Math=ifelse(Math==0, sample(c(NA, 0), size=length(Transfer), replace=TRUE, prob=c(0.3, 0.7)), Math)
##   , English=ifelse(English==0, sample(c(NA, 0), size=length(Transfer), replace=TRUE, prob=c(0.2, 0.8)), English)
##   , Cohort_Math=ifelse(is.na(Math), NA, Cohort + sample(c(0, 1, 2), size=length(Transfer), replace=TRUE, prob=c(0.5, 0.3, 0.2)))
##   , Cohort_English=ifelse(is.na(English), NA, Cohort + sample(c(0, 1, 2), size=length(Transfer), replace=TRUE, prob=c(0.6, 0.3, 0.1)))
##   , Student_ID=100000 + row_number()
##   ) %>%
##   select(Ethnicity, Gender, Cohort, Transfer, Cohort_Math, Math, Cohort_English, English, everything()) %>%
## as.data.frame

## # Import some sample multi-ethnicity data
## library(readr)
## d_multi_eth <- read_csv('../Multi-Ethnicity Data/Results/Multi-Ethnicity.csv')

## # Append this multi-ethnicity data
## set.seed(1000)
## student_equity <- student_equity %>%
##   group_by(Ethnicity) %>%
##   mutate(random_id=sample(n())) %>%
##   ungroup %>%
##   left_join(
##     d_multi_eth %>%
##     group_by(Ethnicity) %>%
##     mutate(random_id=sample(n())) %>%
##     ungroup
##   ) %>%
##   select(-random_id) %>%
##   group_by(Ethnicity) %>% 
##   mutate_at(.vars=vars(starts_with('EthnicityFlag')), .funs=function(x) ifelse(is.na(x), sample(x[!is.na(x)], size=n(), replace=TRUE), x)) %>%
##   ungroup %>%
##   # Fudge math success data to illustrate multi-ethnicity
##   mutate(Math=ifelse(EthnicityFlag_PacificIslander==0, Math, sample(x=1:0, size=n(), replace=TRUE, prob=c(0.30, 0.70)))
##        ## , Math=ifelse(EthnicityFlag_SouthEastAsian==0, Math, sample(x=1:0, size=n(), replace=TRUE, prob=c(0.50, 0.50)))
##        ## , Math=ifelse(EthnicityFlag_Carribean==0, Math, sample(x=1:0, size=n(), replace=TRUE, prob=c(0.20, 0.80)))
##        , Math=ifelse(Math==0, sample(c(NA, 0), size=length(Transfer), replace=TRUE, prob=c(0.2, 0.8)), Math)
##        , Cohort_Math=ifelse(is.na(Math), NA, Cohort + sample(c(0, 1, 2), size=length(Transfer), replace=TRUE, prob=c(0.5, 0.3, 0.2)))
##                      ) %>% 
##   as.data.frame

## # Export data set to ./data
## ##devtools::use_data(student_equity, overwrite=TRUE) ## deprecated
## usethis::use_data(student_equity, overwrite=TRUE)
## openxlsx::write.xlsx(x=student_equity, file='~/Downloads/student_equity.xlsx')

## # Parquet files: external data in ./inst/extdata
## # File used by tinytest
## library(arrow)
## write_parquet(x=student_equity, sink='./inst/extdata/student_equity.parquet')
