##' Calculate disproportionate impact per the proportionality index (PI) method.
##'
##' This function determines disproportionate impact based on the proportionality index (PI) method, as described in \href{https://www.cccco.edu/-/media/CCCCO-Website/Files/DII/guidelines-for-measuring-disproportionate-impact-in-equity-plans-tfa-ada.pdf}{this} reference from the California Community Colleges Chancellor's Office.  It assumes that a higher rate is good ("success").  For rates that are deemed negative (eg, rate of drop-outs, high is bad), then consider looking at the converse of the non-success (eg, non drop-outs, high is good) instead in order to leverage this function properly.
##' @title Calculate disproportionate impact per the proportionality index (PI) method.
##' @param success A vector of success indicators (\code{1}/\code{0} or \code{TRUE}/\code{FALSE}) or an unquoted reference (name) to a column in \code{data} if it is specified.  It could also be a vector of counts, in which case \code{weight} should also be specified (group size).
##' @param group A vector of group names of the same length as \code{success} or an unquoted reference (name) to a column in \code{data} if it is specified.
##' @param cohort (Optional) A vector of cohort names of the same length as \code{success} or an unquoted reference (name) to a column in \code{data} if it is specified.  disproportionate impact is calculated for every group within each cohort.  When \code{cohort} is not specified, then the analysis assumes a single cohort.
##' @param weight (Optional) A vector of case weights of the same length as \code{success} or an unquoted reference (name) to a column in \code{data} if it is specified.  If \code{success} consists of counts instead of success indicators (1/0), then \code{weight} should also be specified to indicate the group size.
##' @param data (Optional) A data frame containing the variables of interest.  If \code{data} is specified, then \code{success}, \code{group}, and \code{cohort} will be searched within it.
##' @param di_prop_index_cutoff A numeric value between 0 and 1 that is used to determine disproportionate impact if the proportionality index falls below this threshold; defaults to 0.80.
##' @return A data frame consisting of:
##' \itemize{
##'   \item \code{cohort} (if used),
##'   \item \code{group},
##'   \item \code{n} (sample size),
##'   \item \code{success} (number of successes for the cohort-group),
##'   \item \code{pct_success} (proportion of successes attributed to the group within the cohort),
##'   \item \code{pct_group} (proportion of sample attributed to the group within the cohort),
##'   \item \code{di_prop_index} (ratio of pct_success to pct_group),
##'   \item \code{di_indicator} (1 if \code{di_prop_index < di_prop_index_cutoff}), and
##'   \item \code{success_needed_not_di} (the number of additional successes needed in order to no longer be considered disproportionately impacted as compared to the reference), and
##'   \item \code{success_needed_full_parity} (the number of additional successes needed in order to achieve full parity with the reference).
##' }
##' When \code{di_prop_index < 1}, then there are signs of disproportionate impact.
##' @examples
##' library(dplyr)
##' data(student_equity)
##' di_prop_index(success=Transfer, group=Ethnicity, data=student_equity) %>%
##'   as.data.frame
##' @references California Community Colleges Chancellor's Office (2014).  \href{https://www.cccco.edu/-/media/CCCCO-Website/About-Us/Divisions/Digital-Innovation-and-Infrastructure/Network-Operations/Accountability/Files/GUIDELINES-FOR-MEASURING-DISPROPORTIONATE-IMPACT-IN-EQUITY-PLANS.ashx}{Guidelines for Measuring Disproportionate Impact in Equity Plans}.
##' @export
##' @import dplyr
##' @importFrom rlang !! enquo
di_prop_index <- function(success, group, cohort, weight, data, di_prop_index_cutoff=0.80) {
  if (!missing(data)) {
    eq_success <- enquo(success)
    success <- data %>% ungroup %>% mutate(success=!!eq_success) %>% select(success) %>% unlist
    eq_group <- enquo(group)
    group <- data %>% ungroup %>% mutate(group=!!eq_group) %>% select(group) %>% unlist
  }
  # Check if success is binary or logical and that there are no NA's
  #stopifnot(success %in% c(1, 0))
  stopifnot(!is.na(success), success>=0) # can be counts
  stopifnot(di_prop_index_cutoff >= 0, di_prop_index_cutoff <= 1)

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
  df <- tibble(cohort, group, success, weight)
  pct_success <- pct_group <- di_indicator <- NULL # to resolve CRAN NOTE: no visible binding for global variable
  dResults <- df %>%
    group_by(cohort, group) %>%
    summarize(n=sum(weight), success=sum(success)) %>%
    ungroup %>%
    group_by(cohort) %>% 
    mutate(pct_success=success/sum(success)
         , pct_group=n/sum(n)
         , di_prop_index=pct_success/pct_group
         , di_indicator=ifelse(di_prop_index < di_prop_index_cutoff, 1, 0) %>% coalesce(0)
           ) %>%
    # Following derived using the formula: (need + group_success) / (need + all_group_success) / pct_group = di_prop_index_cutoff, solve for need
    mutate(success_needed_not_di=ifelse(di_indicator==1, ceiling((sum(success) * pct_group * di_prop_index_cutoff - success) / (1 - pct_group * di_prop_index_cutoff)), 0)
           , success_needed_full_parity=ifelse(di_prop_index < 1, ceiling((sum(success) * pct_group * 1 - success) / (1 - pct_group * 1)), 0)
    ) %>% 
    ungroup %>%
    arrange(cohort, group)

  if (remove_cohort) {
    dResults$cohort <- NULL
  }

  return(dResults)
}
