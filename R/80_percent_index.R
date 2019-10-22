##' Calculate disproportionate impact per the 80\% index method.
##'
##' This function determines disproportionate impact based on the 80\% index method, as described in \href{https://www.cccco.edu/-/media/CCCCO-Website/About-Us/Divisions/Digital-Innovation-and-Infrastructure/Network-Operations/Accountability/Files/GUIDELINES-FOR-MEASURING-DISPROPORTIONATE-IMPACT-IN-EQUITY-PLANS.ashx}{this} reference from the California Community Colleges Chancellor's Office.  It assumes that a higher rate is good ("success").  For rates that are deemed negative (eg, rate of drop-outs, high is bad), then consider looking at the converse of the non-success (eg, non drop-outs, high is good) instead in order to leverage this function properly.
##' @title Calculate disproportionate impact per the 80\% index
##' @param success A vector of success indicators (\code{1}/\code{0} or \code{TRUE}/\code{FALSE}) or an unquoted reference (name) to a column in \code{data} if it is specified.  It could also be a vector of counts, in which case \code{weight} should also be specified (group size).
##' @param group A vector of group names of the same length as \code{success} or an unquoted reference (name) to a column in \code{data} if it is specified.
##' @param cohort (Optional) A vector of cohort names of the same length as \code{success} or an unquoted reference (name) to a column in \code{data} if it specified.  disproportionate impact is calculated for every group within each cohort.  When \code{cohort} is not specified, then the analysis assumes a single cohort.
##' @param weight (Optional) A vector of case weights of the same length as \code{success} or an unquoted reference (name) to a column in \code{data} if it specified.  If \code{success} consists of counts instead of success indicators (1/0), then \code{weight} should also be specified to indicate the group size.
##' @param data (Optional) A data frame containing the variables of interest.  If \code{data} is specified, then \code{success}, \code{group}, and \code{cohort} will be searched within it.
##' @return A data frame consisting of: cohort (if used), group, n (sample size), success (number of successes for the cohort-group), pct (proportion of successes for the cohort-group), di_80_index (ratio of pct to the max pct for each cohort), and di_indicator (1 if \code{di_80_index} < 0.80).
##' @examples
##' library(dplyr)
##' data(student_equity)
##' di_80_index(success=Transfer, group=Ethnicity, data=student_equity) %>%
##'   as.data.frame
##' @references California Community Colleges Chancellor's Office (2014).  \href{https://www.cccco.edu/-/media/CCCCO-Website/About-Us/Divisions/Digital-Innovation-and-Infrastructure/Network-Operations/Accountability/Files/GUIDELINES-FOR-MEASURING-DISPROPORTIONATE-IMPACT-IN-EQUITY-PLANS.ashx}{Guidelines for Measuring Disproportionate Impact in Equity Plans}.
##' @export
##' @import dplyr rlang
di_80_index <- function(success, group, cohort, weight, data) {
  if (!missing(data)) {
    eq_success <- enquo(success)
    success <- data %>% ungroup %>% mutate(success=!!eq_success) %>% select(success) %>% unlist
    eq_group <- enquo(group)
    group <- data %>% ungroup %>% mutate(group=!!eq_group) %>% select(group) %>% unlist
  }
  # Check if success is binary or logical and that there are no NA's
  #stopifnot(success %in% c(1, 0))
  stopifnot(!is.na(success), success>=0) # can be counts

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

  # Check if weight is specified
  if (missing(weight)) {
    weight <- rep(1, length(success))
  } else {
    if (!missing(data)) {
      eq_weight <- enquo(weight)
      weight <- data %>% ungroup %>% mutate(weight=!!eq_weight) %>% select(weight) %>% unlist
    } # else: weight should be defined
  }
  
  # Calculate
  df <- data_frame(cohort, group, success, weight)
  pct <- reference <- NULL # to resolve CRAN NOTE: no visible binding for global variable
  dResults <- df %>%
    group_by(cohort, group) %>%
    summarize(n=sum(weight), success=sum(success), pct=success/n) %>%
    ungroup %>%
    group_by(cohort) %>% 
    mutate(reference=max(pct), di_80_index=pct/reference, di_indicator=ifelse(di_80_index < 0.80, 1, 0)) %>% 
    ungroup %>%
    arrange(cohort, group)

  if (remove_cohort) {
    dResults$cohort <- NULL
  }

  return(dResults)
}
