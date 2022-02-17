##' @export
##' @import dplyr
di_iterate_from_long <- function(data, num_var, denom_var, disagg_var_col, group_var_col, disagg_var_col_2=NULL, group_var_col_2=NULL, cohort_var_col=NULL, summarize_by_vars, ...) {
  # Check to see that success_vars, group_vars, and cohort_vars are not defined
  other_args <- names(list(...))
  if (!is.null(other_args)) {
    lu_invalid_vars <- c('success_vars', 'weight_var', 'group_vars', 'cohort_vars', 'include_non_disagg_results')
    invalid_vars <- other_args[other_args %in% lu_invalid_vars]
    if (length(invalid_vars) > 0) {
      stop(paste0('The following arguments should not be specified: ', paste0('`', invalid_vars, '`', collapse=', '), '.'))
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
    select(one_of(summarize_by_vars, disagg_var_col, disagg_var_col_2, cohort_var_col, group_var_col, group_var_col_2)) %>%
    distinct %>%
    mutate(..group..=row_number())
  
  di_results <- di_iterate(
    data=data %>%
      left_join(lu_scenarios) %>%
      left_join(lu_groups)
    , success_vars=num_var
    , weight_var=denom_var
    # , group_vars=group_var_col
    , group_vars='..group..'
    , cohort_vars='..scenario..'
    , include_non_disagg_results=FALSE
    , ...
  )

  d_results <- lu_scenarios %>%
    left_join(lu_groups) %>% 
    left_join(di_results %>%
              rename(..scenario..=cohort) %>%
              rename(..group..=group) %>% 
              select(-success_variable, -cohort_variable, -disaggregation)
              ) %>%
    select(-..scenario.., -..group..)
  # names(d_results)[names(d_results) == 'group'] <- group_var_col
  return(d_results)
}
