##' Calculate disproportionate impact per the 80\% index method.
##'
##' This function determines disproportionate impact based on the 80\% index method, as described in \href{https://www.cccco.edu/-/media/CCCCO-Website/Files/DII/guidelines-for-measuring-disproportionate-impact-in-equity-plans-tfa-ada.pdf}{this} reference from the California Community Colleges Chancellor's Office.  It assumes that a higher rate is good ("success").  For rates that are deemed negative (eg, rate of drop-outs, high is bad), then consider looking at the converse of the non-success (eg, non drop-outs, high is good) instead in order to leverage this function properly.
##' @title Calculate disproportionate impact per the 80\% index
##' @param success A vector of success indicators (\code{1}/\code{0} or \code{TRUE}/\code{FALSE}) or an unquoted reference (name) to a column in \code{data} if it is specified.  It could also be a vector of counts, in which case \code{weight} should also be specified (group size).
##' @param group A vector of group names of the same length as \code{success} or an unquoted reference (name) to a column in \code{data} if it is specified.
##' @param cohort (Optional) A vector of cohort names of the same length as \code{success} or an unquoted reference (name) to a column in \code{data} if it is specified.  disproportionate impact is calculated for every group within each cohort.  When \code{cohort} is not specified, then the analysis assumes a single cohort.
##' @param weight (Optional) A vector of case weights of the same length as \code{success} or an unquoted reference (name) to a column in \code{data} if it is specified.  If \code{success} consists of counts instead of success indicators (1/0), then \code{weight} should also be specified to indicate the group size.
##' @param data (Optional) A data frame containing the variables of interest.  If \code{data} is specified, then \code{success}, \code{group}, and \code{cohort} will be searched within it.
##' @param di_80_index_cutoff A numeric value between 0 and 1 that is used to determine disproportionate impact if the index comparing the success rate of the current group to the reference group falls below this threshold; defaults to 0.80.
##' @param reference_group The reference group value in \code{group} that each group should be compared to in order to determine disproportionate impact.  By default (\code{=NA}), the group with the highest success rate is used as reference.
##'  @param check_valid_reference Check whether \code{reference_group} is a valid value; defaults to \code{TRUE}.  This argument exists to be used in \link{di_iterate} as when iterating DI calculations, there may be some scenarios where a specified reference group does not contain any students.
##' @return A data frame consisting of:
##' \itemize{
##'   \item \code{cohort} (if used),
##'   \item \code{group},
##'   \item \code{n} (sample size),
##'   \item \code{success} (number of successes for the cohort-group),
##'   \item \code{pct} (proportion of successes for the cohort-group),
##'   \item \code{reference_group} (the reference group used to compare and determine disproportionate impact),
##'   \item \code{reference} (the reference rate used for comparison, corresponding to reference_group),
##'   \item \code{di_80_index} (ratio of pct to the reference), and
##'   \item \code{di_indicator} (1 if \code{di_80_index < di_80_index_cutoff}).
##' }
##' @examples
##' library(dplyr)
##' data(student_equity)
##' di_80_index(success=Transfer, group=Ethnicity, data=student_equity) %>%
##'   as.data.frame
##' @references California Community Colleges Chancellor's Office (2014).  \href{https://www.cccco.edu/-/media/CCCCO-Website/About-Us/Divisions/Digital-Innovation-and-Infrastructure/Network-Operations/Accountability/Files/GUIDELINES-FOR-MEASURING-DISPROPORTIONATE-IMPACT-IN-EQUITY-PLANS.ashx}{Guidelines for Measuring Disproportionate Impact in Equity Plans}.
##' @export
##' @import dplyr
##' @importFrom rlang !! enquo
di_80_index <- function(success, group, cohort, weight, data, di_80_index_cutoff=0.80, reference_group=NA, check_valid_reference=TRUE) {
  if (!missing(data)) {
    eq_success <- enquo(success)
    success <- data %>% ungroup %>% mutate(success=!!eq_success) %>% select(success) %>% unlist
    eq_group <- enquo(group)
    group <- data %>% ungroup %>% mutate(group=!!eq_group) %>% select(group) %>% unlist
  }
  # Check if success is binary or logical and that there are no NA's
  #stopifnot(success %in% c(1, 0))
  stopifnot(!is.na(success), success>=0) # can be counts
  stopifnot(di_80_index_cutoff >= 0, di_80_index_cutoff <= 1)
  #missing_reference_group <- missing(reference_group)
  missing_reference_group <- is.na(reference_group) # default to NA instead of not specified for use in di_iterate function
  
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

  # Check if reference_group is valid
  if (check_valid_reference) {
    if (!is.na(reference_group)) {
      stopifnot(reference_group %in% unique(group))
    }
  }
  
  # Calculate
  df <- tibble(cohort, group, success, weight)
  pct <- reference <- NULL # to resolve CRAN NOTE: no visible binding for global variable
  dResults <- df %>%
    group_by(cohort, group) %>%
    summarize(n=sum(weight), success=sum(success), pct=success/n) %>%
    ungroup %>%
    group_by(cohort) %>% 
    mutate(reference_group=ifelse(missing_reference_group, group[pct==max(pct)], reference_group)
         , reference=ifelse(missing_reference_group, max(pct), pct[group == reference_group])
         , di_80_index=pct/reference
         , di_indicator=ifelse(di_80_index < di_80_index_cutoff, 1, 0)
         , di_indicator=ifelse(is.nan(di_80_index), 0, di_indicator) # di_80_index is NaN when the reference rate is zero; in this case, there is no DI
           ) %>% 
    ungroup %>%
    arrange(cohort, group)

  if (remove_cohort) {
    dResults$cohort <- NULL
  }

  return(dResults)
}
