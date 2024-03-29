##' Calculate disproportionate impact via the percentage point gap (PPG), proportionality index, and 80\% index methods for a "long" and summarized data set with many success variables and disaggregation variables, where the success counts and disaggregation groups are stored in a single column or variable for each.
##' 
##' Iteratively calculate disproportionate impact via the percentage point gap (PPG), proportionality index, and 80\% index methods for all combinations of \code{success_vars}, \code{group_vars}, and \code{cohort_vars}, for each combination of subgroups specified by \code{scenario_repeat_by_vars}.
##' @title Iteratively calculate disproportionate impact using multiple methods for a long and summarized data set
##' @param data A data frame for which to iterate DI calculations for a set of variables.
##' @param num_var A variable name (character value) from \code{data} where the variable stores success counts (the numerator in success rates).  Success rates are calculated by aggregating \code{num_var} and \code{denom_var} for each unique combination of values in \code{disagg_var_col}, \code{group_var_col}, \code{disagg_var_col_2}, \code{group_var_col_2}, \code{cohort_var_col}, and \code{summarize_by_vars}.  If such combinations are unique (single row), then rows are not collapsed.
##' @param denom_var A variable name (character value) from \code{data} where the variable stores the group size (the denominator in success rates).
##' @param disagg_var_col A variable name (character value) from \code{data} where the variable stores the different disaggregation scenarios.  The disaggregation variable could include such values as 'Ethnicity', 'Age Group', and 'Foster Youth', corresponding to three disaggregation scenarios.
##' @param group_var_col A variable name (character value) from \code{data} where the variable stores the group name for each group within a level of disaggregation specified in \code{disagg_var_col}.  For example, the group names could include 'Asian', 'White', 'Black', 'Latinx', 'Native American', and 'Other' for a disaggregation on ethnicity; 'Under 18', '18-21', '22-25', and '25+' for an age group disaggregation; and 'Yes' and 'No' for a foster youth status disaggregation.
##' @param disagg_var_col_2 (Optional) A variable name (character value) from \code{data} where the variable stores an optional second disaggregation variable, which allows for the intersectionality of variables listed in \code{disagg_var_col} and \code{disagg_var_col_2}.  The second disaggregation variable could describe something not in \code{disagg_var_col_2}, such as 'Gender', which would require all groups described in \code{group_var_col} to be broken out by gender.
##' @param group_var_col_2 (Optional) A variable name (character value) from \code{data} where the variable stores the group name for each group within a second level of disaggregation specified in \code{disagg_var_col_2}.  For example, the group names could include 'Male', 'Female', 'Non-binary', and 'Unknown' if 'Gender' is a value in the variable \code{disagg_var_col_2}.
##' @param cohort_var_col (Optional) A variable name (character value) from \code{data} where the variable stores the cohort label for the data described in each row.
##' @param summarize_by_vars (Optional) A character vector of variable names in \code{data} for which \code{num_var} and \code{denom_var} are used for aggregation to calculate success rates for the dispropotionate impact (DI) analysis set up by \code{disagg_var_col}, \code{group_var_col}, \code{disagg_var_col_2}, and \code{group_var_col_2}.  For example, \code{summarize_by_vars=c('Outcome')} could specify a single variable/column that describes the outcome or metric in \code{num_var}, where the outcome values might include 'Completion of Transfer-Level Math', 'Completion of Transfer-Level English','Transfer', 'Associate Degree'.
##' @param custom_reference_group_flag_var (Optional) A variable name (character value) from \code{data} where the variable flags the row or group that should be used as the reference group (\code{1} if row is a reference group, \code{0} otherwise) for comparison in the percentage point gap method and the 80\% index method.  When this argument is used, then the \code{ppg_reference_groups} and \code{di_80_index_reference_groups} arguments should not be specified.
##' @param ... (Optional) Other arguments such as \code{ppg_reference_groups}, \code{min_moe}, \code{use_prop_in_moe}, \code{prop_sub_0}, \code{prop_sub_1}, \code{di_prop_index_cutoff}, \code{di_80_index_cutoff}, \code{di_80_index_reference_groups}, and \code{check_valid_reference} from \link[DisImpact]{di_iterate}.
##' @return A summarized data set (data frame) consisting of:
##' \itemize{
##'   \item variables specified by \code{summarize_by_vars}, \code{disagg_var_col}, \code{group_var_col}, \code{disagg_var_col_2}, and \code{group_var_col_2},
##'   \item \code{di_indicator_ppg} (1 if there is disproportionate impact per the percentage point gap method, 0 otherwise),
##'   \item \code{di_indicator_prop_index} (1 if there is disproportionate impact per the proportionality index, 0 otherwise),
##'   \item \code{di_indicator_80_index} (1 if there is disproportionate impact per the 80\% index, 0 otherwise), and
##'   \item other relevant fields returned from \link[DisImpact]{di_ppg}, \link[DisImpact]{di_prop_index},  and \link[DisImpact]{di_80_index}.
##' }
##' @examples
##' library(dplyr)
##' data(ssm_cohort)
##' di_iterate_on_long(data=ssm_cohort %>% filter(missingFlag==0) # remove missing data
##'   , num_var='value', denom_var='denom'
##'   , disagg_var_col='disagg1', group_var_col='subgroup1'
##'   , cohort_var_col='academicYear', summarize_by_vars=c('categoryLabel')
##'   , ppg_reference_groups='all but current' # PPG-1
##'   , di_80_index_reference_groups='all but current')
##' @import dplyr
##' @importFrom tidyselect one_of
##' @export
di_iterate_on_long <- function(data, num_var, denom_var, disagg_var_col, group_var_col, disagg_var_col_2=NULL, group_var_col_2=NULL, cohort_var_col=NULL, summarize_by_vars=NULL, custom_reference_group_flag_var=NULL, ...) {
  
  other_args <- names(list(...))
  if (!is.null(other_args)) {
    # Check to see that success_vars, group_vars, and cohort_vars are not defined
    lu_invalid_vars <- c('success_vars', 'weight_var', 'group_vars', 'cohort_vars', 'include_non_disagg_results')
    invalid_vars <- other_args[other_args %in% lu_invalid_vars]
    if (length(invalid_vars) > 0) {
      stop(paste0('The following arguments should not be specified: ', paste0('`', invalid_vars, '`', collapse=', '), '.'))
      }

    # Check valid values
    if (any('ppg_reference_groups' == other_args)) {
      if (!all(list(...)$ppg_reference_groups %in% c('hpg', 'overall', 'all but current'))) {
        stop("The `ppg_reference_groups` argument only accepts 'hpg', 'overall', or 'all but current'.  For custom reference groups, please use the `ppg_custom_reference_group_flag_var` argument.")
      }
      if (!is.null(custom_reference_group_flag_var)) {
        stop("Only one of these arguments should be specified: `ppg_reference_groups`, `custom_reference_group_flag_var`.")
      }
    }
    if (any('di_80_index_reference_groups' == other_args)) {
      if (!all(list(...)$di_80_index_reference_groups %in% c('hpg', 'overall', 'all but current'))) {
        stop("The `di_80_index_reference_groups` argument only accepts 'hpg', 'overall', or 'all but current'.  For custom reference groups, please use the `di_80_index_custom_reference_group_flag_var` argument.")
      }
      if (!is.null(custom_reference_group_flag_var)) {
        stop("Only one of these arguments should be specified: `di_80_index_reference_groups`, `custom_reference_group_flag_var`.")
      }
    }
        
  }

  if (!is.null(disagg_var_col_2)) {
    if (is.null(group_var_col_2)) {
      stop('`group_var_col_2` needs to be specified since `disagg_var_col_2` is specified.')
    }
  }
  
  # Table of scenarios
  lu_scenarios <- data %>%
    select(one_of(summarize_by_vars, disagg_var_col, disagg_var_col_2, cohort_var_col)) %>%
    distinct %>%
    mutate(..scenario..=row_number())

  # Table of groups
  lu_groups <- data %>%
    select(one_of(summarize_by_vars, disagg_var_col, disagg_var_col_2, cohort_var_col, group_var_col, group_var_col_2, custom_reference_group_flag_var)) %>%
    distinct %>%
    mutate(..group..=row_number(), ..groupref..=..group..)

  # Custom reference
  if (!is.null(custom_reference_group_flag_var)) {
    lu_groups$..groupref..[lu_groups[[custom_reference_group_flag_var]] == 1] <- 'Custom'

    di_results <- di_iterate(
      data=data %>%
        left_join(lu_scenarios) %>%
        left_join(lu_groups) %>%
        suppressMessages # Source of many "Joining, by", so just remove them
      , success_vars=num_var
      , weight_var=denom_var
      # , group_vars=group_var_col
      # , group_vars='..group..'
      , group_vars='..groupref..'
      , cohort_vars='..scenario..'
      , include_non_disagg_results=FALSE
      , ppg_reference_groups='Custom'
      , di_80_index_reference_groups='Custom'
      , ...
    )
  } else {
    di_results <- di_iterate(
      data=data %>%
        left_join(lu_scenarios) %>%
        left_join(lu_groups) %>%
        suppressMessages # Source of many "Joining, by", so just remove them
      , success_vars=num_var
      , weight_var=denom_var
      # , group_vars=group_var_col
      # , group_vars='..group..'
      , group_vars='..groupref..'
      , cohort_vars='..scenario..'
      , include_non_disagg_results=FALSE
      , ...
    )
  }

  # For CRAN
  cohort <- group <- success_variable <- cohort_variable <- disaggregation <- ..scenario.. <- ..group.. <- ..groupref.. <- NULL
  
  d_results <- lu_scenarios %>%
    left_join(lu_groups) %>%
    left_join(di_results %>%
              rename(..scenario..=cohort) %>%
              # rename(..group..=group) %>%
              rename(..groupref..=group) %>% 
              select(-success_variable, -cohort_variable, -disaggregation)
              ) %>%
    suppressMessages %>% # Source of many "Joining, by", so just remove them
    # select(-..scenario.., -..group..)
    select(-..scenario.., -..group.., -..groupref..)
  # names(d_results)[names(d_results) == 'group'] <- group_var_col
  return(d_results)
}
