##' Iteratively calculate disproportionate impact via the percentage point gap (PPG), proportionality index, and 80\% index methods for many success variables, disaggregation variables, and scenarios.
##' 
##' Iteratively calculate disproportionate impact via the percentage point gap (PPG), proportionality index, and 80\% index methods for all combinations of \code{success_vars}, \code{group_vars}, and \code{cohort_vars}, for each combination of subgroups specified by \code{scenario_repeat_by_vars}.
##' @title Iteratively calculate disproportionate impact using multiple method for many variables.
##' @param data A data frame for which to iterate DI calculations for a set of variables.
##' @param success_vars A character vector of success variable names to iterate across.
##' @param group_vars A character vector of group (disaggregation) variable names to iterate across.
##' @param cohort_vars (Optional) A character vector of the same length as \code{success_vars} to indicate the cohort variable to be used for each variable specified in \code{success_vars}.  A vector of length 1 could be specified, in which case the same cohort variable is used for each success variable.  If not specified, then a single cohort is assumed for all success variables.
##' @param scenario_repeat_by_vars (Optional) A character vector of variables to repeat DI calculations for across all combination of these variables.  For example, the following variables could be specified:
##' \itemize{
##'   \item Ed Goal: Degree/Transfer, Shot-term Career, Non-credit
##'   \item First time college student: Yes, No
##'   \item Full-time status: Yes, No
##' }
##' Each combination of these variables (eg, full time, first time college students with an ed goal of degree/transfer as one combination) would constitute an iteration / sample for which to calculate disproportionate impact for outcomes listed in \code{success_vars} and for the disaggregation variables listed in \code{group_vars}. The overall rate of success for full time, first time college students with an ed goal of degree/transfer would just include these students and not others.  Each variable specified is also collapsed to an '- All' group so that the combinations also reflect all students of a particular category.  The total number of combinations for the previous example would be (+1 representing the all category): (3 + 1) x (2 + 1) x (2 + 1) = 36.
##' @param exclude_scenario_df (Optional) A data frame with variables that match \code{scenario_repeat_by_vars} for specifying the combinations to exclude from DI calculations.  Following the example specified above, one could choose to exclude part-time non-credit students from consideration.
##' @param weight_var (Optional) A character variable specifying the weight variable if the input data set is summarized (ie,  the the success variables specified in \code{success_vars} contain count of successes).  Weight here corresponds to the denominator when calculating the success rate.  Defaults to \code{NULL} for an input data set where each row describes each individual.
##' @param include_non_disagg_results A logical variable specifying whether or not the non-disaggregated results should be returned; defaults to \code{TRUE}.  When \code{TRUE}, a new variable \code{`- None`} is added to the data set with a single data value \code{'- All'}, and this variable is added \code{group_vars} as a disaggregation/group variable.  The user would want these results returned to review non-disaggregated results.
##' @param ppg_reference_groups Either \code{'overall'}, \code{'hpg'}, \code{'all but current'}, or a character vector of the same length as \code{group_vars} that indicates the reference group value for each group variable in \code{group_vars} when determining disproportionate impact using the percentage point gap method.
##' @param min_moe The minimum margin of error to be used in the PPG calculation, passed to \link[DisImpact]{di_ppg}.
##' @param use_prop_in_moe Whether the estimated proportions should be used in the margin of error calculation by the PPG, passed to \link[DisImpact]{di_ppg}.
##' @param prop_sub_0 passed to \link[DisImpact]{di_ppg}; defaults to 0.50.
##' @param prop_sub_1 passed to \link[DisImpact]{di_ppg}; defaults to 0.50.
##' @param di_prop_index_cutoff Threshold used for determining disproportionate impact using the proportionality index; passed to \link[DisImpact]{di_prop_index}; defaults to 0.80.
##' @param di_80_index_cutoff Threshold used for determining disproportionate impact using the 80\% index; passed to \link[DisImpact]{di_80_index}; defaults to 0.80.
##' @param di_80_index_reference_groups A character vector of the same length as \code{group_vars} that indicates the reference group value for each group variable in \code{group_vars} when determining disproportionate impact using the 80\% index; defaults to \code{NA} (highest performing group as reference).
##' @param check_valid_reference Check whether \code{ppg_reference_groups} and \code{di_80_index_reference_groups} contain valid values; defaults to \code{TRUE}.
##' @return A summarized data set (data frame) consisting of:
##' \itemize{
##'   \item \code{success_variable} (elements of \code{success_vars}),
##'   \item \code{disaggregation} (elements of \code{group_vars}),
##'   \item \code{cohort} (values corresponding to the variables specified in \code{cohort_vars},
##'   \item \code{di_indicator_ppg} (1 if there is disproportionate impact per the percentage point gap method, 0 otherwise),
##'   \item \code{di_indicator_prop_index} (1 if there is disproportionate impact per the proportionality index, 0 otherwise),
##'   \item \code{di_indicator_80_index} (1 if there is disproportionate impact per the 80\% index, 0 otherwise), and
##'   \item other relevant fields returned from \link[DisImpact]{di_ppg}, \link[DisImpact]{di_prop_index},  and \link[DisImpact]{di_80_index}.
##' }
##' @examples
##' library(dplyr)
##' data(student_equity)
##' # Multiple group variables
##' di_iterate(data=student_equity, success_vars=c('Transfer')
##'   , group_vars=c('Ethnicity', 'Gender'), cohort_vars=c('Cohort')
##'   , ppg_reference_groups='overall')
##' @import dplyr
##' @importFrom tidyselect everything one_of
##' @importFrom purrr pmap
##' @importFrom tidyr unnest
##' @export
di_iterate <- function(data, success_vars, group_vars, cohort_vars=NULL, scenario_repeat_by_vars=NULL, exclude_scenario_df=NULL, weight_var=NULL, include_non_disagg_results=TRUE, ppg_reference_groups='overall', min_moe=0.03, use_prop_in_moe=FALSE, prop_sub_0=0.5, prop_sub_1=0.5, di_prop_index_cutoff=0.80, di_80_index_cutoff=0.80, di_80_index_reference_groups=NA, check_valid_reference=TRUE) {
  stopifnot(length(group_vars) == length(ppg_reference_groups) | length(ppg_reference_groups) == 1)
  stopifnot(length(group_vars) == length(di_80_index_reference_groups) | is.na(di_80_index_reference_groups))

  # Check valid group
  if (check_valid_reference) {
    for (i in 1:length(ppg_reference_groups)) {
      if (!(ppg_reference_groups[i] %in% c(as.character(formals(di_ppg)$reference)[-1], unique(data[[group_vars[i]]])))) {
        stop(paste0("'", ppg_reference_groups[i], "'", " is not valid for the argument `ppg_reference_groups` as it does not exist in the group variable `", group_vars[i], "`."))
      }
    }
    for (i in 1:length(di_80_index_reference_groups)) {
      if (!(di_80_index_reference_groups[i] %in% c(unique(data[[group_vars[i]]]))) & !is.na(di_80_index_reference_groups[i])) {
        stop(paste0("'", di_80_index_reference_groups[i], "'", " is not valid for the argument `di_80_index_reference_groups` as it does not exist in the group variable `", group_vars[i], "`."))
      }
    }
  }
  
  # Add a variable for non-disaggregated results
  if (include_non_disagg_results) {
    data$`- None` <- '- All'
    group_vars <- c(group_vars, '- None')
    if (length(ppg_reference_groups) > 1) {
      ppg_reference_groups <- c(ppg_reference_groups, 'overall')
    } else if (length(ppg_reference_groups) == 1 & !(ppg_reference_groups %in% c('overall', 'hpg', 'all but current'))) {
      ppg_reference_groups <- c(ppg_reference_groups, 'overall')
    } # else leave as is (overall, hpg, all but current to be used)
    if (length(di_80_index_reference_groups) > 1) {
      di_80_index_reference_groups <- c(di_80_index_reference_groups, NA) # Adding last NA for non-disaggregated results
    } else if (length(di_80_index_reference_groups) == 1 & !is.na(di_80_index_reference_groups)) {
      di_80_index_reference_groups <- c(di_80_index_reference_groups, NA)
    } # else leave as is (overall, hpg, all but current to be used)
  }
  
  if (length(unique(sapply(data[, group_vars], class))) > 1) {
    stop("All variables specified in `group_vars` should be of the same class.  Suggestion: set them all as character data using `as.character`.")
  }
  
  if (!is.null(scenario_repeat_by_vars)) {
    if (length(unique(sapply(data[, scenario_repeat_by_vars], class))) > 1) {
      stop("All variables specified in `scenario_repeat_by_vars` should be of the same class.  Suggestion: set them all as character data.")
    } 
  }

  if (is.null(cohort_vars)) {
    cohort_vars <- '_cohort_'
    data[[cohort_vars]] <- '- All'
  }
  if (length(cohort_vars) != 1 & length(cohort_vars) != length(success_vars)) {
    stop('`cohort_vars` must be of length 1 or the same length as `success_vars` (each success variable corresponds to a cohort variable).')
  }
  lu_success_cohort <- data.frame(success_var=success_vars, cohort_var=cohort_vars, stringsAsFactors=FALSE)
  
  if (is.null(weight_var)) {
    weight_var <- '- Weight'
    ## data[[weight_var]] <- 1

    # Create summarized data set for faster computations down the line
    data <- data %>%
      mutate_at(vars(one_of(success_vars)), .funs=list('NA_FLAG'= ~ is.na(.))) %>% # sum up successes
      group_by_at(vars(one_of(group_vars, cohort_vars, scenario_repeat_by_vars, if (length(success_vars)==1) {'NA_FLAG'} else {paste0(success_vars, '_NA_FLAG')}))) %>% # Break out by missingness in the success variables in order to sum separately for valid weights
      mutate(`- Weight`=1) %>%
      summarize_at(vars(success_vars, '- Weight'), .funs=sum) %>%  # sum of success variables and cases (weight)
      ungroup
  } else {
    if (any(is.na(data[[weight_var]]))) {
      stop(paste0("The specified column corresponding to weight_var='", weight_var, "' contain NA values."))
    }
    if (any(data[[weight_var]] <= 0)) {
      stop(paste0("The specified column corresponding to weight_var='", weight_var, "' contain non-positive values."))
    }
  }
 
  # CRAN: no visible binding for global variable
  success_var <- group_var <- cohort_var <- ppg_reference_group <- NULL

  # Set up different repeat-by data sets by determining row indices
  if (!is.null(scenario_repeat_by_vars)) {
    # All combinations, including '- All'
    dRepeatScenarios0 <- data %>%
      select(one_of(scenario_repeat_by_vars)) %>%
      lapply(function(x) c(unique(x), '- All')) %>%
      expand.grid(stringsAsFactors=FALSE)

    # Exclude
    if (!is.null(exclude_scenario_df)) {
      exclude__ <- NULL # CRAN: no visible binding for global variable
      dRepeatScenarios0 <- dRepeatScenarios0 %>%
        left_join(exclude_scenario_df %>% mutate(exclude__=1)) %>%
        filter(!is.na(exclude__)) %>%
        select(one_of(scenario_repeat_by_vars))
    }
    
    # For each combination, determine row indices; take only combination with actual observations

    # CRAN: no visible binding for global variable
    row_index <- want_indices <- n_rows <- NULL
    dRepeatScenarios <- lapply(1:nrow(dRepeatScenarios0)
                             , FUN=function(i) {
                               # CRAN: no visible binding for global variable
                               row_index <- want_indices <- n_rows <- NULL
                               
                               vars_specific <- colnames(dRepeatScenarios0)[!(dRepeatScenarios0[i, ] %in% '- All')]
                               vars_all <- colnames(dRepeatScenarios0)[dRepeatScenarios0[i, ] %in% '- All']
                               
                               if (length(vars_specific) != 0) {
                                 # dRepeatScenarios0[i, ] %>% # this gives an error when there is a single variable in scenario_repeat_by_vars
                                 d_interm <- dRepeatScenarios0 %>%
                                   slice(i) %>% 
                                   select(one_of(vars_specific)) %>%
                                   left_join(data %>% mutate(row_index=row_number())) %>%

                                   filter(!is.na(row_index)) %>% # No match
                                   group_by_at(vars(one_of(vars_specific))) %>%
                                   summarize(want_indices=list(row_index), n_rows=n()) %>%
                                   ungroup # %>%
                                   # mutate_at(.vars=vars(one_of(vars_all)), .funs=function(x) '- All') ## do this below

                                 d_interm[, vars_all] <- '- All' # No impact if vars_all is empty
                                 d_interm
                               } else { # all variables are '- All'
                                 d_interm <- data %>%
                                   mutate(row_index=row_number()) %>%
                                   summarize(want_indices=list(row_index), n_rows=n()) %>%
                                   ungroup # %>%
                                   # mutate_at(.vars=vars(one_of(vars_all)), .funs=function(x) '- All')
                                 d_interm[, vars_all] <- '- All' # No impact if vars_all is empty
                                 d_interm
                               }
                             }
                             ) %>%
      bind_rows %>%
      filter(n_rows > 0) %>%
      select(-n_rows)
  }
  
  ## if (!is.null(scenario_repeat_by_vars)) {
  ##   # Combination of subsets
  ##   dRepeatData1 <- data %>%
  ##     # expand(nesting(one_of(scenario_repeat_by_vars)))
  ##     select(one_of(scenario_repeat_by_vars)) %>%
  ##     distinct %>%
  ##     left_join(data %>% mutate(row_index=row_number())) %>%
  ##     group_by_at(vars(one_of(scenario_repeat_by_vars))) %>%
  ##     summarize(want_indices=list(row_index)) %>%
  ##     ungroup
    
  ##   # Combination of subsets: the 'All' group for each variable
  ##   if (length(scenario_repeat_by_vars) > 1) {
  ##     dRepeatData2 <- lapply(seq_along(scenario_repeat_by_vars)
  ##                          , FUN=function(i) {
  ##                            cur_var <- scenario_repeat_by_vars[i]
  ##                            dRepeatData <- data %>%
  ##                              # expand(nesting(one_of(scenario_repeat_by_vars)))
  ##                              select(one_of(scenario_repeat_by_vars[-i])) %>%
  ##                              distinct %>%
  ##                              left_join(data %>% mutate(row_index=row_number())) %>%
  ##                              group_by_at(vars(one_of(scenario_repeat_by_vars[-i]))) %>%
  ##                              summarize(want_indices=list(row_index)) %>%
  ##                              ungroup %>%
  ##                              mutate(!!cur_var := "- All")
  ##                          }
  ##                          ) %>%
  ##       bind_rows
  ##   } else { # length(scenario_repeat_by_vars) == 1
  ##     dRepeatData2 <- data %>%
  ##       mutate(row_index=row_number()) %>% 
  ##       mutate(!!scenario_repeat_by_vars := "- All") %>%
  ##       group_by_at(vars(one_of(scenario_repeat_by_vars))) %>%
  ##       summarize(want_indices=list(row_index)) %>%
  ##       ungroup
  ##   }
  ##   # Combine
  ##   dRepeatData <- bind_rows(dRepeatData1, dRepeatData2)
  ## } else {

  ## }
  
  # Set up scenarios
  dRef <- data.frame(group_var=group_vars, ppg_reference_group=ppg_reference_groups, di_80_index_reference_group=di_80_index_reference_groups, stringsAsFactors=FALSE)

  ppg_check_valid_reference <- di_80_check_valid_reference <- di_80_index_reference_group <- NULL # CRAN: no visible binding for global variable
  dScenarios <- expand.grid(success_var=success_vars, group_var=group_vars, min_moe=min_moe, use_prop_in_moe=use_prop_in_moe, prop_sub_0=prop_sub_0, prop_sub_1=prop_sub_1, ppg_check_valid_reference=FALSE, di_prop_index_cutoff=di_prop_index_cutoff, di_80_index_cutoff=di_80_index_cutoff, di_80_check_valid_reference=FALSE, stringsAsFactors=FALSE) %>%
    left_join(lu_success_cohort, by=c('success_var')) %>% 
    left_join(dRef, by=c('group_var')) %>%
    select(success_var, group_var, cohort_var, ppg_reference_group, min_moe, use_prop_in_moe, prop_sub_0, prop_sub_1, ppg_check_valid_reference, di_prop_index_cutoff, di_80_index_cutoff, di_80_index_reference_group, di_80_check_valid_reference)

  # Function to iterate for each scenario
  iterate <- function(success_var, group_var, cohort_var, ppg_reference_group, min_moe, use_prop_in_moe, prop_sub_0, prop_sub_1, ppg_check_valid_reference, di_prop_index_cutoff, di_80_index_cutoff, di_80_index_reference_group, di_80_check_valid_reference, subset_idx) {
    
    data <- data[subset_idx, ]

    # Remove rows with missing value for success_var
    data <- data[!is.na(data[[success_var]]), ]

    # If after removing rows with missing data and we end up with 0 cases, return nothing
    if (nrow(data)==0) {
      return(NULL)
    }
    
    ## if (!(ppg_reference_group %in% c('overall', 'hpg', 'all but current'))) {
    ##   # reference_val <- sapply(sort(unique(data[[cohort_var]]), na.last=TRUE), function(cohort) mean(data[[success_var]][data[[group_var]] %in% ppg_reference_group & data[[cohort_var]] %in% cohort])) # one for each non-NA cohort
    ##   reference_val <- sapply(sort(unique(data[[cohort_var]]), na.last=TRUE), function(cohort) sum(data[[success_var]][data[[group_var]] %in% ppg_reference_group & data[[cohort_var]] %in% cohort]) / sum(data[[weight_var]][data[[group_var]] %in% ppg_reference_group & data[[cohort_var]] %in% cohort]))
    ## } else {
    ##   reference_val <- ppg_reference_group # overall or hpg or all but current
    ## }
    reference_val <- ppg_reference_group # Can specify actual group value since di_ppg has been updated
    
    # CRAN: no visible binding for global variable
    success_variable <- disaggregation <- cohort_variable <- reference_group <- reference <- di_indicator <- cohort <- group <- success <- NULL

    di_ppg(success=data[[success_var]], group=data[[group_var]], cohort=data[[cohort_var]], weight=data[[weight_var]], reference=reference_val, min_moe=min_moe, use_prop_in_moe=use_prop_in_moe, prop_sub_0=prop_sub_0, prop_sub_1=prop_sub_1, check_valid_reference=ppg_check_valid_reference) %>%
      rename(ppg_reference_group=reference_group, ppg_reference=reference, di_indicator_ppg=di_indicator) %>%
      left_join(
        di_prop_index(success=data[[success_var]], group=data[[group_var]], cohort=data[[cohort_var]], weight=data[[weight_var]], di_prop_index_cutoff=di_prop_index_cutoff) %>%
        select(cohort, group, n, success, di_prop_index, di_indicator) %>% 
        rename(di_indicator_prop_index=di_indicator)
      , by=c('cohort', 'group', 'n', 'success')
      ) %>%
      left_join(
        di_80_index(success=data[[success_var]], group=data[[group_var]], cohort=data[[cohort_var]], weight=data[[weight_var]], di_80_index_cutoff=di_80_index_cutoff, reference_group=di_80_index_reference_group, check_valid_reference=di_80_check_valid_reference) %>%
        select(cohort, group, n, success, reference_group, di_80_index, di_indicator) %>% 
        rename(di_indicator_80_index=di_indicator, di_80_index_reference_group=reference_group)
      , by=c('cohort', 'group', 'n', 'success')        
      ) %>% 
      mutate(
        success_variable=success_var
      , disaggregation=group_var
      , cohort_variable=cohort_var
      # , ppg_reference_group=ppg_reference_group
             ) %>%
      select(success_variable, cohort_variable, cohort, disaggregation, everything())
  }

  # Iterate for all scenarios
  if (is.null(scenario_repeat_by_vars)) {
    subset_idx <- 1:nrow(data)
    pmap(dScenarios %>% mutate(subset_idx=list(subset_idx)), iterate) %>%
      bind_rows
  } else {
    dRepeatScenarios$df_results <- lapply(1:nrow(dRepeatScenarios)
                                     , FUN=function(i) {
                                       # data <- data %>% slice(dRepeatScenarios[i, ] %>% select(want_indices) %>% unlist)
                                       # subset_idx <- dRepeatScenarios[i, ] %>% select(want_indices) %>% unlist
                                       subset_idx <- dRepeatScenarios %>% slice(i) %>% select(want_indices) %>% unlist
                                       pmap(dScenarios %>% mutate(subset_idx=list(subset_idx)), iterate) %>%
                                         bind_rows
                                     }
                                     )
    # CRAN: no visible binding for global variable
    df_results <- NULL
    
    dRepeatScenarios %>%
      select(-want_indices) %>%
      unnest(df_results)
  }
}
