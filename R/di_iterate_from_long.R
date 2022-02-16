di_iterate_from_long <- function(data, num_var, denom_var, disagg_var_col, group_var_col, cohort_var_col=NULL, summarize_by_vars, ...) {
  # Check to see that success_vars, group_vars, and cohort_vars are not defined
  other_args <- names(list(...))
  if (!is.null(other_args)) {
    lu_invalid_vars <- c('success_vars', 'weight_var', 'group_vars', 'cohort_vars', 'include_non_disagg_results')
    invalid_vars <- other_args[other_args %in% lu_invalid_vars]
    if (length(invalid_vars) > 0) {
      stop(paste0('The following arguments should not be specified: ', paste0(invalid_vars, collapse=', '), '.'))
      }
  }
  
  # Table of scenarios
  lu_scenarios <- data %>%
    select(one_of(summarize_by_vars, disagg_var_col, cohort_var_col)) %>%
    distinct %>%
    mutate(..scenario..=row_number())

  di_results <- di_iterate(
    data=data %>% left_join(lu_scenarios)
    , success_vars=num_var
    , weight_var=denom_var
    , group_vars=group_var_col
    , cohort_vars='..scenario..'
    , include_non_disagg_results=FALSE
    , ...
  )

  d_results <- lu_scenarios %>%
    left_join(di_results %>%
              rename(..scenario..=cohort) %>%
              select(-success_variable, -cohort_variable, -disaggregation)
              ) %>%
    select(-..scenario..)
  names(d_results)[names(d_results) == 'group'] <- group_var_col
  return(d_results)
}
