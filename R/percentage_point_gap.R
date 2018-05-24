## library(devtools)
## library(roxygen2)
## setwd('DisImpact')
## document()
## devtools::build()
## setwd('..')
## install('DisImpact')

##' Calculate the margin of error (MOE) for the percentage point gap (PPG) method.
##'
##' @title Margin of error for the PPG
##' @param n Sample size for the group of interest.
##' @param proportion (Optional) The proportion of successes for the group of interest.  If specified, then the proportion is used in the MOE formula.  Otherwise, a default proportion of 0.50 is used (conservative and yields the maximum MOE).
##' @param min_moe The minimum MOE returned even if the sample size is large.  Defaults to 0.03.
##' @return The margin of error for the PPG given the specified sample size.
##' @examples
##' ppg_moe(n=800)
##' ppg_moe(n=c(200, 800, 1000, 2000))
##' ppg_moe(n=800, proportion=0.20)
##' ppg_moe(n=800, proportion=0.20, min_moe=0)
##' ppg_moe(n=c(200, 800, 1000, 2000), min_moe=0.01)
##' @references California Community Colleges Chancellor's Office (2017).  \href{http://extranet.cccco.edu/Portals/1/TRIS/Research/Analysis/PercentagePointGapMethod2017.pdf}{Percentage Point Gap Method}.
##' @export
ppg_moe <- function(n, proportion, min_moe=0.03) {
  if (missing(proportion)) {
    return(pmax(1.96 * sqrt(0.25/n), min_moe))
  }
  else {
    return(pmax(1.96 * sqrt(proportion * (1-proportion)/n), min_moe))
  }
}
##' Calculate disproportionate impact per the percentage point gap (PPG) method.
##'
##' This function determines disproportionate impact based on the percentage point gap (PPG) method, as described in \href{http://extranet.cccco.edu/Portals/1/TRIS/Research/Analysis/PercentagePointGapMethod2017.pdf}{this} reference from the California Community Colleges Chancellor's Office.  It assumes that a higher rate is good ("success").  For rates that are deemed negative (eg, rate of drop-outs, high is bad), then consider looking at the converse of the non-success (eg, non drop-outs, high is good) instead in order to leverage this function properly.  Note that the margin of error (MOE) is calculated using using 1.96*sqrt(0.25^2/n), with a \code{min_moe} used as the minimum by default.
##' @title Calculate disproportionate impact per the percentage point gap (PPG) method.
##' @param success A vector of success indicators (\code{1}/\code{0} or \code{TRUE}/\code{FALSE}) or an unquoted reference (name) to a column in \code{data} if it is specified.
##' @param group A vector of group names of the same length as \code{success} or an unquoted reference (name) to a column in \code{data} if it is specified.
##' @param cohort (Optional) A vector of cohort names of the same length as \code{success} or an unquoted reference (name) to a column in \code{data} if it specified.  disproportionate impact is calculated for every group within each cohort.  When \code{cohort} is not specified, then the analysis assumes a single cohort.
##' @param reference Either \code{'overall'} (default), \code{'hpg'} (highest performing group), a single proportion (eg, 0.50), or a vector of proportions.  Reference is used as a point of comparison for disproportionate impact for each group.  When \code{cohort} is specified:\cr
##'   1. \code{'overall'} will use the overall success rate of each cohort group as the reference;\cr
##'   2. \code{'hpg'} will use the highest performing group in each cohort as reference;\cr
##'   3. the specified proportion will be used for all cohorts;\cr
##'   4. the specified vector of proportions will refer to the reference point for each cohort in alphabetical order (so the number of proportions should equal to the number of unique cohorts).\cr
##' @param data (Optional) A data frame containing the variables of interest.  If \code{data} is specified, then \code{success}, \code{group}, and \code{cohort} will be searched within it.
##' @param min_moe The minimum margin of error (MOE) to be used in the calculation of disproportionate impact and is passed to \link{ppg_moe}.  Defaults to \code{0.03}.
##' @param use_prop_in_moe A logical value indicating whether or not the MOE formula should use the observed success rates (\code{TRUE}).  Defaults to \code{FALSE}, which uses 0.50 as the proportion in the MOE formula.  If \code{TRUE}, the success rates are passed to the \code{proportion} argument of \link{ppg_moe}.
##' @return A data frame consisting of: cohort (if used), group, n (sample size), success (number of successes for the cohort-group), pct (proportion of successes for the cohort-group), reference (reference used in DI calculation), moe (margin of error), pct.lo (lower 95\% confidence interval for pct), pct.hi (upper 95\% confidence interval for pct), and di.indicator (1 if there is disproportionate impact).
##' @examples
##' library(dplyr)
##' data(student_equity)
##' # Vector
##' di_ppg(success=student_equity$Transfer
##'   , group=student_equity$Ethnicity) %>% as.data.frame
##' # Tidy and column reference
##' di_ppg(success=Transfer, group=Ethnicity, data=student_equity) %>%
##'   as.data.frame
##' # Cohort
##' di_ppg(success=Transfer, group=Ethnicity, cohort=Cohort
##'  , data=student_equity) %>%
##'   as.data.frame
##' # With custom reference (single)
##' di_ppg(success=Transfer, group=Ethnicity, reference=0.54
##'   , data=student_equity) %>%
##'   as.data.frame
##' # With custom reference (multiple)
##' di_ppg(success=Transfer, group=Ethnicity, cohort=Cohort
##'   , reference=c(0.5, 0.55), data=student_equity) %>%
##'   as.data.frame
##' # min_moe
##' di_ppg(success=Transfer, group=Ethnicity, data=student_equity
##'   , min_moe=0.02) %>%
##'   as.data.frame
##' # use_prop_in_moe
##' di_ppg(success=Transfer, group=Ethnicity, data=student_equity
##'   , min_moe=0.02
##'   , use_prop_in_moe=TRUE) %>%
##'   as.data.frame
##' @references California Community Colleges Chancellor's Office (2017).  \href{http://extranet.cccco.edu/Portals/1/TRIS/Research/Analysis/PercentagePointGapMethod2017.pdf}{Percentage Point Gap Method}.
##' @export
##' @import dplyr rlang
di_ppg <- function(success, group, cohort, reference=c('overall', 'hpg'), data, min_moe=0.03, use_prop_in_moe=FALSE) {
  ## require(magrittr)
  ## require(dplyr)
  ## require(rlang)

  if (!missing(data)) {
    eq_success <- enquo(success)
    success <- data %>% ungroup %>% mutate(success=!!eq_success) %>% select(success) %>% unlist
    eq_group <- enquo(group)
    group <- data %>% ungroup %>% mutate(group=!!eq_group) %>% select(group) %>% unlist
  }
  # Check if success is binary or logical and that there are no NA's
  stopifnot(success %in% c(1, 0))

  # Check if cohort is specified
  if (missing(cohort)) {
    cohort <- 1
    remove_cohort <- TRUE
  } else {
    remove_cohort <- FALSE
    if (!missing(data)) {
      eq_cohort <- enquo(cohort)
      cohort <- data %>% ungroup %>% mutate(cohort=!!eq_cohort) %>% select(cohort) %>% unlist
    }
  }

  # Check if reference is specified: overall, hpg, or user-defined references
  if (!is.numeric(reference)) {
    reference <- match.arg(reference)
    reference_type <- reference
    reference_numeric <- 1
  } else {
    # expecting vector of length 1 or length equal to unique cohort
    stopifnot(length(reference) == length(unique(cohort)))
    reference_type <- 'custom'
    reference_numeric <- reference
    if (length(reference) > 1) {
      dReference <- data_frame(cohort=sort(unique(cohort))
                             , reference
                               )
    }
  }

  # Calculate
  df <- data_frame(cohort, group, success)
  dResults <- df %>%
    group_by(cohort, group) %>%
    summarize(n=n(), success=sum(success), pct=success/n) %>%
    ungroup %>%
    arrange(cohort, group)
  if (!exists('dReference')) {
    dResults <- dResults %>% 
      group_by(cohort) %>%
      mutate(reference=case_when(
               reference_type=='overall' ~ sum(success) / sum(n)
             , reference_type=='hpg' ~ max(pct)
             , reference_type=='custom' ~ reference_numeric
             )) %>%
      ungroup
  } else {
    dResults <- dResults %>%
      left_join(dReference)
  }
  pct <- moe <- pct.hi <- NULL # to resolve CRAN NOTE: no visible binding for global variable
  dResults <- dResults %>% 
    mutate(moe=case_when(use_prop_in_moe ~ ppg_moe(n=n, proportion=pct, min_moe=min_moe)
                         , !use_prop_in_moe ~ ppg_moe(n=n, min_moe=min_moe)
                         )
         , pct.lo=pct - moe
         , pct.hi=pct + moe
         , di.indicator=ifelse(pct.hi < reference, 1, 0)
           ) %>%
    arrange(cohort, group)
  
  if (remove_cohort) {
    dResults$cohort <- NULL
  }
  
  return(dResults)
}
