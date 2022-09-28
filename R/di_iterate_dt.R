##' Calculate disproportionate impact via the percentage point gap (PPG), proportionality index, and 80\% index methods for data stored in a \link[data.table]{data.table} object.  This is the workhorse function leveraged by the \link[DisImpact]{di_iterate_dt} function.
##' 
##' @title Calculates disproportionate impact using multiple methods for data stored in a \link[data.table]{data.table} object.
##' @param dt A data frame of class \link[data.table]{data.table}.  If the object is not a data table, one could surround the object with \link[data.table]{as.data.table}.
##' @param success_var A character value specifying the success variable name. 
##' @param group_var A character value specifying the group (disaggregation) variable name.
##' @param cohort_var (Optional) A character value specifying the cohort variable.  If not specified, then a single cohort is assumed (defaults to an empty string, \code{''}).
##' @param weight_var (Optional) A character variable specifying the weight variable if the input data set is summarized (i.e.,  the the success variables specified in \code{success_vars} contain count of successes).  Weight here corresponds to the denominator when calculating the success rate.  Defaults to \code{NULL} for an input data set where each row describes an individual.
##' @param ppg_reference_group Either \code{'overall'}, \code{'hpg'}, \code{'all but current'}, or a character value specifying a group from \code{group_var} to be used as the reference group for comparison using percentage point gap method.
##' @param min_moe The minimum margin of error to be used in the PPG calculation; see \link[DisImpact]{di_ppg}.
##' @param use_prop_in_moe (\code{TRUE} or \code{FALSE}) Whether the estimated proportions should be used in the margin of error calculation by the PPG; see \link[DisImpact]{di_ppg}.
##' @param prop_sub_0 Default is 0.50; see \link[DisImpact]{di_ppg}.
##' @param prop_sub_1 Default is 0.50; see \link[DisImpact]{di_ppg}.
##' @param di_prop_index_cutoff Threshold used for determining disproportionate impact using the proportionality index; see \link[DisImpact]{di_prop_index}; defaults to 0.80.
##' @param di_80_index_cutoff Threshold used for determining disproportionate impact using the 80\% index; see \link[DisImpact]{di_80_index}; defaults to 0.80.
##' @param di_80_index_reference_group Either \code{'overall'}, \code{'hpg'}, \code{'all but current'}, or a character value specifying a group from \code{group_var} to be used as the reference group for comparison using 80\% index.
##' @param filter_subset A character value such as \code{"Ethnicity == 'White' & Gender == 'M'"} used in the \code{i} argument (filtering rows via \code{dt[i, j, by]}) to filter data in \code{dt}.  The character value is parsed using \code{eval(parse(text=filter_subset))}.  Defaults to \code{''} for no filtering.
##' @return A \link[data.table]{data.table} object with summarized results.
##' @importFrom data.table is.data.table as.data.table setnames setkeyv .N `:=` fcase
##' @importFrom collapse fgroup_by get_vars fsum qDT collapv
##' @export
di_calc_dt <- function(dt, success_var, group_var, cohort_var='', weight_var=NULL, ppg_reference_group='overall', min_moe=0.03, use_prop_in_moe=FALSE, prop_sub_0=0.5, prop_sub_1=0.5, di_prop_index_cutoff=0.8, di_80_index_cutoff=0.8, di_80_index_reference_group='hpg', filter_subset='') {

  # Need data.table >= 1.14.3
  if(requireNamespace("data.table", quietly=TRUE) &&
    utils::packageVersion("data.table") < "1.14.3") {
    stop("`di_calc_dt` requires data.table version 1.14.3 or higher.  Please execute the following to update data.table to the current development version:\n\n    detach('package:DisImpact', unload=TRUE) # unload package\n\n    data.table::update.dev.pkg()\n\nIf this doesn't work, try:\n\n    install.packages('data.table', repo = 'https://Rdatatable.gitlab.io/data.table') # Select 'Yes' if asked to install from source.\n\nIf the previous doesn't work, you can manually download the package (.tar.gz, .zip, or .tgz) at https://rdatatable.gitlab.io/data.table/web/packages/data.table/index.html and install it yourself.")
  }
  
  # Following for CRAN: no visible binding for global variable
  weight__ <- success <- weight <- pct <- pct_ppg <- cohort <- moe <- ppg_reference <- hpg_subgroup <- overall_rate <- hpg_rate <- all_but_current_rate <- di_80_index_specific_group_rate <- di_80_index_reference <- pct_hi <- pct_lo <- di_indicator_prop_index <- cohort_success_total <- pct_group <- pct_success <- success_var_value <- cohort_var_value <- group_var_value <- group <- ppg_reference_group_name <- di_indicator_ppg <- success_needed_not_di_ppg <- success_needed_full_parity_ppg <- success_needed_not_di_prop_index <- success_needed_full_parity_prop_index <- di_80_index_reference_group_name <- di_indicator_80_index <- success_needed_not_di_80_index <- success_needed_full_parity_80_index <- . <- NULL
  
  if (is.null(weight_var)) {
    weight_var <- '.N' # for data.table
    weight_var <- 'weight__' # for collapse
  }

  if (filter_subset == '') {
    filter_subset <- 'TRUE'
  }
  
  if (cohort_var=='') {
    group_by_list <- c(group_var)
    substitute_list <- list(success_var=success_var
                          , success_var_value=I(success_var)
                          , cohort_var=I(cohort_var) # different
                          , cohort_var_value=I(cohort_var)
                          , group_var=group_var
                          , group_var_value=I(group_var)
                          , weight_var=weight_var
                          , group_by_list=as.list(group_by_list)
                            )
  } else {
    group_by_list <- c(cohort_var, group_var)
    substitute_list <- list(success_var=success_var
                          , success_var_value=I(success_var)
                          , cohort_var=cohort_var # different
                          , cohort_var_value=I(cohort_var)
                          , group_var=group_var
                          , group_var_value=I(group_var)
                          , weight_var=weight_var
                          , group_by_list=as.list(group_by_list)
                            )
  }
  
  dt <- dt[
    eval(parse(text=filter_subset))
  , env=substitute_list
  ][
    !is.na(success_var)
  , env=substitute_list
  ]
  
  if (nrow(dt) == 0) {
    return(NULL)
  }
  
  dt <- dt[ # Following is using collapse package for aggregation
    , weight__:=1
  ] %>% 
    ## fgroup_by(group_by_list) %>% # bug in collapse 1.8.8 https://github.com/SebKrantz/collapse/issues/320#issuecomment-1259065656
    ## get_vars(c(success_var, weight_var)) %>% 
    ## fsum %>%
    collapv(by=c(group_by_list), custom=list(fsum=c(success_var, weight_var))) %>% 
    qDT # to not have data.table warning without having to use suppressWarnings https://github.com/SebKrantz/collapse/issues/319#issuecomment-1249527808
  setnames(x=dt, old=c(success_var, weight_var), new=c('success', 'weight'))
  dt[
    , `:=`(
        cohort=cohort_var
      , group=group_var
      , pct=success / weight
      , pct_ppg=fcase(
          !rep(use_prop_in_moe, .N), 0.5
        , (success / weight) == 0, prop_sub_0
        , (success / weight) == 1, prop_sub_1
        , rep(TRUE, .N), success / weight
        )
      )
  , env=substitute_list
  ][
  ## ## Following is using data.table for aggregation
  ## [# Summarize by group
  ##   # !is.na(success_var) & eval(parse(text=filter_subset))
  ## , .(
  ##     cohort=cohort_var
  ##   , group=group_var
  ##   , pct=sum(success_var)/sum(weight_var)
  ##   , success=sum(success_var)
  ##   , weight=sum(weight_var)
  ##   , pct_ppg=fcase(
  ##       !use_prop_in_moe, 0.5
  ##       , sum(success_var)/sum(weight_var) == 0, prop_sub_0
  ##       , sum(success_var)/sum(weight_var) == 1, prop_sub_1
  ##       , default=sum(success_var)/sum(weight_var)
  ##     )
  ##   )
  ## # , by=.(cohort_var, group_var)
  ## , by=group_by_list
  ## , env=substitute_list
  ## ][
  ## # Calculate needed statistics
  , `:=`(
      # PPG
      overall_rate = sum(success)/sum(weight)
    , hpg_rate = max(pct)
    , hpg_subgroup = group_var[pct == max(pct)][1]
    , all_but_current_rate = (sum(success) - success) / (sum(weight) - weight)
    , ppg_specific_group_rate = c(pct[group_var == ppg_reference_group], NA)[1]
    , moe = fcase(
        (1.96 * sqrt(pct_ppg*(1-pct_ppg)/weight)) < min_moe, min_moe
        , rep(TRUE, .N), (1.96 * sqrt(pct_ppg*(1-pct_ppg)/weight))
        , default=NA
      )
    , ppg_reference_group_name = fcase(
        rep(ppg_reference_group, .N) == 'hpg', group_var[pct == max(pct)][1] ## hpg_subgroup
        # rep(ppg_reference_group, .N) == 'hpg', ppg_reference_group ## like di_iterate ver. 0.0.19
      , rep(ppg_reference_group, .N) %in% c('overall', 'all but current'), ppg_reference_group
      , default=ppg_reference_group # specific group
      )
    , ppg_reference=fcase(
        rep(ppg_reference_group, .N)=='overall', sum(success) / sum(weight)
      , rep(ppg_reference_group, .N)=='hpg', max(pct)
      , rep(ppg_reference_group, .N)=='all but current', (sum(success) - success) / (sum(weight) - weight)
      , rep(TRUE, .N), c(pct[group_var == ppg_reference_group], NA)[1]
      , default=NA
      )
    # PI
    , cohort_size = sum(weight)
    , cohort_success_total = sum(success)
    , pct_group = weight/sum(weight)
    , pct_success = success/sum(success)
    , di_prop_index = (success/sum(success)) / (weight/sum(weight))
    , di_indicator_prop_index = fcase(
        (success/sum(success)) / (weight/sum(weight)) < di_prop_index_cutoff, 1
        , default=0
      )
    # 80% index
    , di_80_index_specific_group_rate = c(pct[group_var == di_80_index_reference_group], NA)[1]
    )
  # , by=.(cohort_var)
  , by=.(cohort)
  # , env=list(cohort_var=cohort_var, group_var=group_var)
  , env=substitute_list
  ][
  , `:=`(
      # PPG
      pct_lo = pct - moe
    , pct_hi = pct + moe
    , di_indicator_ppg = fcase(
        (pct + moe) <= ppg_reference, 1
        , default=0
      )
      # 80%
    , di_80_index_reference_group_name = fcase(
        rep(di_80_index_reference_group, .N) == 'hpg', hpg_subgroup
        , rep(di_80_index_reference_group, .N) %in% c('overall', 'all but current'), di_80_index_reference_group
        , default=di_80_index_reference_group # specific group
      )
    , di_80_index_reference = fcase(
        rep(di_80_index_reference_group, .N) == 'overall', overall_rate
        , rep(di_80_index_reference_group, .N) == 'hpg', hpg_rate
        , rep(di_80_index_reference_group, .N) == 'all but current', all_but_current_rate
        , rep(TRUE, .N), di_80_index_specific_group_rate
      )
    )
  # , by=.(cohort_var)
  , by=.(cohort)
  # , env=list(cohort_var=cohort_var, group_var=group_var)
  ][ # Success needed
  , `:=`(
      di_80_index = pct / di_80_index_reference
    , di_indicator_80_index = fcase(
        pct / di_80_index_reference < di_80_index_cutoff, 1
      , default=0
      )
    , success_needed_not_di_ppg = fcase(
        pct_hi < ppg_reference, ceiling((ppg_reference - pct_hi) * weight)
        , default=0
      )
    , success_needed_full_parity_ppg = fcase(
        pct < ppg_reference, ceiling((ppg_reference - pct) * weight)
        , default=0
      )
    , success_needed_not_di_prop_index = fcase(
        di_indicator_prop_index==1, ceiling((cohort_success_total * pct_group * di_prop_index_cutoff - success) / (1 - pct_group * di_prop_index_cutoff))
        , default=0
      )
    , success_needed_full_parity_prop_index = fcase(
        pct_success < pct_group, ceiling((cohort_success_total * pct_group * 1 - success) / (1 - pct_group * 1))
        , default=0
      )
    , success_needed_not_di_80_index = fcase(
        pct / di_80_index_reference < di_80_index_cutoff, ceiling((di_80_index_cutoff * di_80_index_reference - pct) * weight)
        , default=0
      )
    , success_needed_full_parity_80_index = fcase(
        pct < di_80_index_reference, ceiling((di_80_index_reference - pct) * weight)
        , default=0
      )
    )
  # , by=.(cohort_var)
  , by=.(cohort)
  # , env=list(cohort_var=cohort_var, group_var=group_var)
  ][
  , .(
      # success_variable=success_var
      success_variable=success_var_value
      # , cohort_variable=cohort_var
      , cohort_variable=cohort_var_value
      , cohort
      # , disaggregation=group_var
      , disaggregation=group_var_value
      , group
      , n=weight
      , success
      , pct
      , ppg_reference
      , ppg_reference_group=ppg_reference_group_name
      , moe
      , pct_lo
      , pct_hi
      , di_indicator_ppg
      , success_needed_not_di_ppg
      , success_needed_full_parity_ppg
      , di_prop_index
      , di_indicator_prop_index
      , success_needed_not_di_prop_index
      , success_needed_full_parity_prop_index
      , di_80_index_reference_group=di_80_index_reference_group_name
      , di_80_index
      , di_indicator_80_index
      , success_needed_not_di_80_index
      , success_needed_full_parity_80_index
      , filter_subset=filter_subset
    )
  # , env=list(success_var=I(success_var), cohort_var=I(cohort_var), group_var=I(group_var))
  , env=substitute_list
  ][
  , filter_subset:=ifelse(filter_subset=='TRUE', '', filter_subset)
  ][]
}

##' Iteratively calculate disproportionate impact via the percentage point gap (PPG), proportionality index, and 80\% index methods for many success variables, disaggregation variables, and scenarios, using \link[data.table]{data.table} and \link[collapse]{collapse}.
##'
##' Iteratively calculate disproportionate impact via the percentage point gap (PPG), proportionality index, and 80\% index methods for all combinations of \code{success_vars}, \code{group_vars}, and \code{cohort_vars}, for each combination of subgroups specified by \code{scenario_repeat_by_vars}, using \link[data.table]{data.table} and \link[collapse]{collapse}.
##' @title Iteratively calculate disproportionate impact using multiple method for many variables, using \link[data.table]{data.table} and \link[collapse]{collapse}.
##' @param dt A data frame of class \link[data.table]{data.table}.  If the object is not a data table, one could surround the object with \link[data.table]{as.data.table}.
##' @param success_vars A character vector of success variable names to iterate across.
##' @param group_vars A character vector of group (disaggregation) variable names to iterate across.
##' @param cohort_vars (Optional) A character vector of the same length as \code{success_vars} to indicate the cohort variable to be used for each variable specified in \code{success_vars}.  A vector of length 1 could be specified, in which case the same cohort variable is used for each success variable.  If not specified, then a single cohort is assumed for all success variables (defaults to \code{NULL}).
##' @param scenario_repeat_by_vars (Optional) A character vector of variables to repeat DI calculations for across all combination of these variables.  For example, the following variables could be specified:
##' \itemize{
##'   \item Ed Goal: Degree/Transfer, Shot-term Career, Non-credit
##'   \item First time college student: Yes, No
##'   \item Full-time status: Yes, No
##' }
##' Each combination of these variables (eg, full time, first time college students with an ed goal of degree/transfer as one combination) would constitute an iteration / sample for which to calculate disproportionate impact for outcomes listed in \code{success_vars} and for the disaggregation variables listed in \code{group_vars}. The overall rate of success for full time, first time college students with an ed goal of degree/transfer would just include these students and not others.  Each variable specified is also collapsed to an '- All' group so that the combinations also reflect all students of a particular category.  The total number of combinations for the previous example would be (+1 representing the all category): (3 + 1) x (2 + 1) x (2 + 1) = 36.
##' @param exclude_scenario_df (Optional) A data frame with variables that match \code{scenario_repeat_by_vars} for specifying the combinations to exclude from DI calculations.  Following the example specified above, one could choose to exclude part-time non-credit students from consideration.
##' @param weight_var (Optional) A character variable specifying the weight variable if the input data set is summarized (i.e., the the success variables specified in \code{success_vars} contain count of successes).  Weight here corresponds to the denominator when calculating the success rate.  Defaults to \code{NULL} for an input data set where each row describes an individual.
##' @param include_non_disagg_results A logical variable specifying whether or not the non-disaggregated results should be returned; defaults to \code{TRUE}.  When \code{TRUE}, a new variable \code{`- None`} is added to the data set with a single data value \code{'- All'}, and this variable is added to \code{group_vars} as a disaggregation/group variable.  The user would want these results returned to review non-disaggregated results.
##' @param ppg_reference_groups Either \code{'overall'}, \code{'hpg'}, \code{'all but current'}, or a character vector of the same length as \code{group_vars} that indicates the reference group value for each group variable in \code{group_vars} when determining disproportionate impact using the percentage point gap method.
##' @param min_moe The minimum margin of error to be used in the PPG calculation; see \link[DisImpact]{di_ppg}.
##' @param use_prop_in_moe (\code{TRUE} or \code{FALSE}) Whether the estimated proportions should be used in the margin of error calculation by the PPG; see \link[DisImpact]{di_ppg}. 
##' @param prop_sub_0 Default is 0.50; see \link[DisImpact]{di_ppg}.
##' @param prop_sub_1 Default is 0.50; see \link[DisImpact]{di_ppg}.
##' @param di_prop_index_cutoff Threshold used for determining disproportionate impact using the proportionality index; see \link[DisImpact]{di_prop_index}; defaults to 0.80.
##' @param di_80_index_cutoff Threshold used for determining disproportionate impact using the 80\% index; see \link[DisImpact]{di_80_index}; defaults to 0.80.
##' @param di_80_index_reference_groups Either \code{'overall'}, \code{'hpg'}, \code{'all but current'}, or a character vector of the same length as \code{group_vars} that indicates the reference group value for each group variable in \code{group_vars} when determining disproportionate impact using the 80\% index.
##' @param check_valid_reference (\code{TRUE} or \code{FALSE}) Check whether \code{ppg_reference_groups} and \code{di_80_index_reference_groups} contain valid values; defaults to \code{TRUE}.
##' @param parallel If \code{TRUE}, then perform calculations in parallel.  Defaults to \code{FALSE}.  Parallel execution is based on the \code{parallel} package included in base R, using \link[parallel]{parLapply} on Windows and \link[parallel]{mclapply} on POSIX-based systems (Linux/Mac).
##' @param parallel_n_cores The number of CPU cores to use if \code{parallel=TRUE}.  Defaults to half of the maximum number of CPU cores on the system.
##' @return A summarized data set of class \link[data.table]{data.table}, with variables as described in \link[DisImpact]{di_iterate}.
##' @importFrom data.table is.data.table as.data.table setnames setkeyv .N `:=` fcase .SD rbindlist setorderv
##' @importFrom collapse fgroup_by get_vars fsum qDT fnobs
##' @importFrom sets set_power
##' @importFrom stringr str_detect str_replace_all
##' @import dplyr
##' @import parallel
##' @export
di_iterate_dt <- function(dt, success_vars, group_vars, cohort_vars=NULL, scenario_repeat_by_vars=NULL, exclude_scenario_df=NULL, weight_var=NULL, include_non_disagg_results=TRUE, ppg_reference_groups='overall', min_moe=0.03, use_prop_in_moe=FALSE, prop_sub_0=0.5, prop_sub_1=0.5, di_prop_index_cutoff=0.8, di_80_index_cutoff=0.8, di_80_index_reference_groups='hpg', check_valid_reference=TRUE, parallel=FALSE, parallel_n_cores=parallel::detectCores() / 2) {

  # Need data.table >= 1.14.3
  if(requireNamespace("data.table", quietly=TRUE) &&
    utils::packageVersion("data.table") < "1.14.3") {
    stop("`di_iterate_dt` requires data.table version 1.14.3 or higher.  Please execute the following to update data.table to the current development version:\n\n    detach('package:DisImpact', unload=TRUE) # unload package\n\n    data.table::update.dev.pkg()\n\nIf this doesn't work, try:\n\n    install.packages('data.table', repo = 'https://Rdatatable.gitlab.io/data.table') # Select 'Yes' if asked to install from source.\n\nIf the previous doesn't work, you can manually download the package (.tar.gz, .zip, or .tgz) at https://rdatatable.gitlab.io/data.table/web/packages/data.table/index.html and install it yourself.")
  }

  # Following for CRAN: no visible binding for global variable
  weight <- success_var <- cohort_var <- filter_subset <- NULL
  
  if (!is.data.table(dt)) {
    stop("`dt` should be an object of class data.table.  Consider wrapping the object around `as.data.table` after loading the `data.table` package.")
  }
  
  stopifnot(length(group_vars) == length(ppg_reference_groups) | length(ppg_reference_groups) == 1)
  stopifnot(length(group_vars) == length(di_80_index_reference_groups) | length(di_80_index_reference_groups) == 1)

  ## # Check for valid variable names for custom query construction
  ## if(any(str_detect(c(scenario_repeat_by_vars, group_vars, cohort_vars, success_vars), '[^a-zA-Z0-9_]'))) {
  ##   x <- c(scenario_repeat_by_vars, group_vars, cohort_vars, success_vars)
  ##   stop(paste0("Variable names should only contain alphanumeric characters and underscores: ", paste0(x[str_detect(x, '[^a-zA-Z0-9_]')], collapse='; ')))
  ## }

  # Check if variables are in table
  vars_to_check <- c(success_vars, group_vars, cohort_vars, scenario_repeat_by_vars, weight_var)
  if (!(all(vars_to_check %in% names(dt)))) {
    vars_not_found <- paste0(vars_to_check[!(vars_to_check %in% names(dt))], collapse=', ')
    stop(paste0('Following variables are not found in `dt`: ', vars_not_found))
  }

  # Check valid reference groups
  if (check_valid_reference) {
    for (i in 1:length(group_vars)) {
      group_var <- group_vars[i]
      unique_groups <- dt[, unique(group_var), env=list(group_var=group_var)]
      if (!(ppg_reference_groups[pmin(i, length(ppg_reference_groups))] %in% c('overall', 'hpg', 'all but current', unique_groups))) {
        stop(paste0("'", ppg_reference_groups[pmin(i, length(ppg_reference_groups))], "'", " is not valid for the argument `ppg_reference_groups` as it is not one of c('overall', 'hpg', 'all but current'), or it does not exist in the group variable `", group_vars[i], "`."))
      }
      if (!(di_80_index_reference_groups[pmin(i, length(di_80_index_reference_groups))] %in% c('overall', 'hpg', 'all but current', unique_groups))) {
        stop(paste0("'", di_80_index_reference_groups[pmin(i, length(di_80_index_reference_groups))], "'", " is not valid for the argument `di_80_index_reference_groups` as it is not one of c('overall', 'hpg', 'all but current'), or it does not exist in the group variable `", group_vars[i], "`."))
      }
    }
  }
  
  # Cohort
  if (is.null(cohort_vars)) {
    cohort_vars <- ''
  }
  if (length(cohort_vars) != 1 & length(cohort_vars) != length(success_vars)) {
    stop('`cohort_vars` must be of length 1 or the same length as `success_vars` (each success variable corresponds to a cohort variable).')
  }
  
  # Create summary table first
  if (length(cohort_vars)==1 && cohort_vars=='') {
    dt_summ <- dt[
    , c(success_vars, group_vars, scenario_repeat_by_vars, weight_var)
    , with=FALSE
    ]
  } else {
    dt_summ <- dt[
    , c(success_vars, group_vars, cohort_vars, scenario_repeat_by_vars, weight_var)
    , with=FALSE
    ]
  }

  ## ## Check for weight variable
  ## if (is.null(weight_var)) {
  ##   weight_var <- '.N'
  ## }

  ## dt_summ <- dt_summ[
  ## , paste0(success_vars, '_NA_FLAG') := lapply(.SD, function(x) is.na(x) * 1)
  ## , .SDcols=success_vars
  ## ][
  ## , .(weight = sum(weight_var))
  ## , by=group_by_list
  ## , env=list(
  ##     weight_var=weight_var
  ##   , group_by_list=as.list(c(success_vars, group_vars, cohort_vars, scenario_repeat_by_vars, paste0(success_vars, '_NA_FLAG')))
  ##   )
  ## ]

  # Use collapse package to speed up aggregations
  if (is.null(weight_var)) {
    dt_summ <- dt_summ[, weight:=1][
    , paste0(success_vars, '_NA_FLAG') := lapply(.SD, function(x) is.na(x) * 1)
    , .SDcols=success_vars
    ] %>%
      fgroup_by(c(success_vars, group_vars, if (length(cohort_vars)==1 && cohort_vars=='') NULL else cohort_vars, scenario_repeat_by_vars, paste0(success_vars, '_NA_FLAG'))) %>%
      get_vars('weight') %>% 
      fnobs %>%
      qDT # to not have data.table warning without having to use suppressWarnings https://github.com/SebKrantz/collapse/issues/319#issuecomment-1249527808

    dt_summ[, c(success_vars) := lapply(.SD, function(x) x * weight), .SDcols=success_vars] # weigh numerator
    weight_var <- 'weight'
  } ##  else {
  ##   dt_summ <- dt_summ[
  ##   , paste0(success_vars, '_NA_FLAG') := lapply(.SD, function(x) is.na(x) * 1)
  ##   , .SDcols=success_vars
  ##   ] %>%
  ##     fgroup_by(c(success_vars, group_vars, if (length(cohort_vars)==1 && cohort_vars=='') NULL else cohort_vars, scenario_repeat_by_vars, paste0(success_vars, '_NA_FLAG'))) %>%
  ##     get_vars(weight_var) %>%
  ##     fsum %>%
  ##     qDT # to not have data.table warning without having to use suppressWarnings https://github.com/SebKrantz/collapse/issues/319#issuecomment-1249527808
  ## }
  
  if (include_non_disagg_results) {
    dt_summ[, '- None' := '- All']
    group_vars <- c(group_vars, '- None')
    
    if (length(ppg_reference_groups) > 1) {
      ppg_reference_groups <- c(ppg_reference_groups, 'overall')
    } else if (length(ppg_reference_groups) == 1 & !(ppg_reference_groups %in% c('overall', 'hpg', 'all but current'))) {
      ppg_reference_groups <- c(ppg_reference_groups, 'overall')
    } # else leave as is (overall, hpg, all but current to be used)
    if (length(di_80_index_reference_groups) > 1) {
      di_80_index_reference_groups <- c(di_80_index_reference_groups, 'overall')
    } else if (length(di_80_index_reference_groups) == 1 & !(di_80_index_reference_groups %in% c('hpg', 'overall', 'all but current'))) {
      di_80_index_reference_groups <- c(di_80_index_reference_groups, 'overall')
    } # else leave as is (overall, hpg, all but current to be used)
  }
  
  lu_success_cohort <- data.frame(success_var=success_vars, cohort_var=cohort_vars, stringsAsFactors=FALSE)
  lu_group_reference <- data.frame(group_var=group_vars, ppg_reference_group=ppg_reference_groups, di_80_index_reference_group=di_80_index_reference_groups, stringsAsFactors=FALSE)
  dScenarios <- expand.grid(success_var=success_vars
                            , group_var=group_vars
                            , weight_var=weight_var
                            # , ppg_reference_group=ppg_reference_groups
                            , min_moe=min_moe
                            , use_prop_in_moe=use_prop_in_moe
                            , prop_sub_0=prop_sub_0
                            , prop_sub_1=prop_sub_1
                            , di_prop_index_cutoff=di_prop_index_cutoff
                            , di_80_index_cutoff=di_80_index_cutoff
                            # , di_80_index_reference_group=di_80_index_reference_groups
                            , stringsAsFactors=FALSE
                            ) %>%
    left_join(lu_success_cohort, by='success_var') %>%
    left_join(lu_group_reference, by='group_var') %>% 
    select(success_var, group_var, cohort_var, everything())

  if (!is.null(scenario_repeat_by_vars)) {
    dRepeatScenarios0 <- lapply(dt_summ[, scenario_repeat_by_vars, with=FALSE]
                              , function(x) c(unique(x), '- All')
                                ) %>%
      expand.grid(stringsAsFactors=FALSE)
    # names(dRepeatScenarios0) <- scenario_repeat_by_vars
    dRepeatScenarios0$filter_subset <- do.call("paste"
                                               , c(lapply(1:ncol(dRepeatScenarios0), function(i) paste0(paste0('`', names(dRepeatScenarios0)[i], '`'), " == ", "'", dRepeatScenarios0[[i]], "'")), sep=" & ")
                                                 ) %>%
      str_replace_all(" &(?:(?!&).)*'- All'", '') %>% # every "& to '- All'" that is not first
      str_replace_all("^.*'- All'", '') %>% # first "& to '- All'"
      str_replace_all("^ & ", '') # beginning with " & "
    
    if (!is.null(exclude_scenario_df)) {
      if (!all(names(exclude_scenario_df) %in% scenario_repeat_by_vars)) {
        stop("`exclude_scenario_df` contain variables that are not specified in `scenario_repeat_by_vars`.")
      }
      exclude__ <- NULL
      dRepeatScenarios0 <- dRepeatScenarios0 %>%
        left_join(exclude_scenario_df %>% mutate(exclude__ = 1)) %>%
        filter(is.na(exclude__)) %>% 
        select(one_of(names(dRepeatScenarios0)))
    }
    
    # Cartesian join of scenarios
    dScenarios <- dScenarios %>%
      full_join(dRepeatScenarios0 %>% select(filter_subset), by=character())

    # Speed up data.table with keys
    keyvars_list <- lapply(as.list(set_power(c(scenario_repeat_by_vars))), unlist)
    keyvars_list <- keyvars_list[2:length(keyvars_list)] # drop first NULL
    for (i in 1:length(keyvars_list)) {
      setkeyv(dt_summ, keyvars_list[[i]])
    }

  }

  # Repeated calculations
  if (isFALSE(parallel)) {
    d_results <- pmap(dScenarios, di_calc_dt, dt=dt_summ) %>%
      rbindlist
  } else if (isTRUE(parallel) & Sys.info()[["sysname"]] == "Windows"){
    cl <- makeCluster(parallel_n_cores)
    on.exit(stopCluster(cl), add=TRUE)   
    clusterEvalQ(cl, library(data.table))
    clusterEvalQ(cl, library(purrr))
    clusterEvalQ(cl, library(collapse))
    clusterExport(cl, varlist=c('di_calc_dt', 'dt_summ', 'dScenarios'), envir=environment())
    d_results <- parLapply(cl, 1:nrow(dScenarios), fun=function(i) pmap(dScenarios[i, ], di_calc_dt, dt=dt_summ)) %>%
      # rbindlist
      bind_rows
  } else { # parallel on POSIX-based system
    d_results <- mclapply(1:nrow(dScenarios), FUN=function(i) pmap(dScenarios[i, ], di_calc_dt, dt=dt_summ), mc.cores=parallel_n_cores) %>%
      bind_rows
  }

  # Results
  if (!is.null(scenario_repeat_by_vars)) {
    # Merge scenario variables
    d_results <- as.data.table(dRepeatScenarios0)[
      d_results
    , on='filter_subset'
    ]
  }
  d_results[
    , filter_subset:=NULL
    ][]
  setorderv(d_results, c(scenario_repeat_by_vars, 'success_variable', 'cohort_variable', 'cohort', 'disaggregation', 'group'), na.last=TRUE) # reorder
  return(d_results)  
}
