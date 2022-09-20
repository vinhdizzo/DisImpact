##' Generate SQL code that calculates disproportionate impact via the percentage point gap (PPG), proportionality index, and 80\% index methods for a specified table name, success variable, group variable, and cohort variable.  This is the workhorse function leveraged by the \link[DisImpact]{di_iterate_sql} function.
##'
##' @title Generate SQL code that calculates disproportionate impact using multiple methods for a specified table.
##' @param db_table_name A character value specifying a database table name.
##' @param success_var A character value specifying the success variable name.
##' @param group_var A character value specifying the group (disaggregation) variable name.
##' @param cohort_var (Optional) A character value specifying the cohort variable.  If not specified, then a single cohort is assumed (defaults to an empty string, \code{''}).
##' @param weight_var (Optional) A character variable specifying the weight variable if the input data set is summarized (i.e.,  the the success variables specified in \code{success_vars} contain count of successes).  Weight here corresponds to the denominator when calculating the success rate.  Defaults to a numeric \code{1} which treats each row as an individual.
##' @param ppg_reference_group Either \code{'overall'}, \code{'hpg'}, \code{'all but current'}, or a character value specifying a group from \code{group_var} to be used as the reference group for comparison using the percentage point gap method.
##' @param min_moe The minimum margin of error to be used in the PPG calculation; see \link[DisImpact]{di_ppg}.
##' @param use_prop_in_moe (\code{TRUE} or \code{FALSE}) Whether the estimated proportions should be used in the margin of error calculation by the PPG; see \link[DisImpact]{di_ppg}.
##' @param prop_sub_0 Default is 0.50; see \link[DisImpact]{di_ppg}.
##' @param prop_sub_1 Default is 0.50; see \link[DisImpact]{di_ppg}.
##' @param di_prop_index_cutoff Threshold used for determining disproportionate impact using the proportionality index; see \link[DisImpact]{di_prop_index}; defaults to 0.80.
##' @param di_80_index_cutoff Threshold used for determining disproportionate impact using the 80\% index; see \link[DisImpact]{di_80_index}; defaults to 0.80.
##' @param di_80_index_reference_group Either \code{'overall'}, \code{'hpg'}, \code{'all but current'}, or a character value specifying a group from \code{group_var} to be used as the reference group for comparison using 80\% index.
##' @param before_with_statement Character value to be added to the SQL query to allow for modification.  Defaults to \code{''} (empty string).
##' @param after_with_statement Character value to be added to the SQL query to allow for modification.  Defaults to \code{''} (empty string).
##' @param end_of_select_statement Character value to be added to the SQL query to allow for modification.  Defaults to \code{''} (empty string).
##' @param where_statement Character value to be added to the SQL query to allow for modification.  Defaults to \code{''} (empty string).
##' @param select_statement_add Character value to be added to the SQL query to allow for modification.  Defaults to \code{''} (empty string).
##' @return A character value (SQL query) that could be executed on a database.
##' @importFrom stringr str_replace_all fixed
##' @importFrom glue glue
##' @export
di_calc_sql <- function(db_table_name, success_var, group_var, cohort_var='', weight_var=1, ppg_reference_group='overall', min_moe=0.03, use_prop_in_moe=FALSE, prop_sub_0=0.5, prop_sub_1=0.5, di_prop_index_cutoff=0.8, di_80_index_cutoff=0.8, di_80_index_reference_group='hpg', before_with_statement='', after_with_statement='', end_of_select_statement='', where_statement='', select_statement_add='') {

  ## # Following removed to change cohort_var default to '' (blank); used to be cohort_var="'- All'"
  cohort_var_no_quote <- str_replace_all(cohort_var, fixed("'"), "") # '- All' when no cohort specified
  
  query <- "
  -- create table foo as -- create table for sqlite, duckdb (parquet), postgres, MySQL; not SQL Server
  {before_with_statement}
  with
  tb_summ as
  (
    select
    {cohort_var} as cohort
    , {group_var} as subgroup
    , 1.0 * sum({success_var}) / sum({weight_var}) as pct
    , 1.0 * sum({success_var}) as success
    , 1.0 * sum({weight_var}) as weight
    , case 
      when '{use_prop_in_moe}' != 'TRUE' then 0.5
      when 1.0 * sum({success_var}) / sum({weight_var}) = 0 then {prop_sub_0}
      when 1.0 * sum({success_var}) / sum({weight_var}) = 1 then {prop_sub_1}
      else 1.0 * sum({success_var}) / sum({weight_var})
      end as pct_ppg
    from
    {db_table_name}
    {where_statement}
    group by
    {cohort_var}
    , {group_var}
  )
  ,
  tb_ppg_specific_group as
  (
    select
    cohort
    , subgroup as ppg_reference_group
    , pct as ppg_specific_group_rate
    from
    tb_summ
    where
    subgroup = '{ppg_reference_group}'
  )
  ,
  tb_di_80_index_specific_group as
  (
    select
    cohort
    , subgroup as di_80_index_reference_group
    , pct as di_80_index_specific_group_rate
    from
    tb_summ
    where
    subgroup = '{di_80_index_reference_group}'
  )
  ,
  tb_overall as
  (
    select
    cohort
    , 1.0 * sum(success) / sum(weight) as overall_rate
    from
    tb_summ
    group by
    cohort
  )
  ,
  tb_hpg as
  (
    select
    cohort
    , subgroup as hpg_subgroup
    , pct as hpg_rate
    from
    (
      select
      *
      , row_number() over (partition by cohort order by pct desc, subgroup) as rn
      from
      tb_summ
    ) as a
    where
    a.rn=1
  )
  ,
  tb_all_but_current as
  (
    select
    c.cohort
    , c.subgroup
    , 1.0 * sum(success) / sum(weight) as all_but_current_rate
    from
    (
      select
      a.cohort
      , a.subgroup
      , b.success
      , b.weight
      from
      tb_summ as a
      left join
      tb_summ as b
      on
      b.cohort = a.cohort
      and
      b.subgroup != a.subgroup
    ) as c
    group by
    c.cohort
    , c.subgroup
  )
  ,
  tb_group_prop as
  (
    select
    a.cohort
    , a.subgroup
    , b.cohort_size
    , b.cohort_success_total
    , 1.0 * a.weight / b.cohort_size as pct_group
    , 1.0 * a.success / b.cohort_success_total as pct_success
    from
    tb_summ as a
    left join
    (
      select
      cohort
      , sum(weight) as cohort_size
      , sum(success) as cohort_success_total
      from
      tb_summ
      group by
      cohort
    ) as b
    on
    b.cohort = a.cohort
  )
  ,
  tb_calc as
  (
    select
    a.*
    -- ppg
    , case
      when a.pct_hi <= a.ppg_reference then 1
      else 0
      end as di_indicator_ppg
    , case
      when a.pct_hi <= a.ppg_reference then ceil((a.ppg_reference - (a.pct + a.moe)) * a.n)
      else 0
      end as success_needed_not_di_ppg
    , case when a.pct < a.ppg_reference then ceil((a.ppg_reference - a.pct) * a.n)
      else 0
      end as success_needed_full_parity_ppg
    , a.pct / a.di_80_index_reference as di_80_index
    , case when a.pct / a.di_80_index_reference < {di_80_index_cutoff} then 1 else 0 end as di_indicator_80_index
    , case
      when a.pct / a.di_80_index_reference < {di_80_index_cutoff} then ceil(({di_80_index_cutoff} * a.di_80_index_reference - a.pct) * a.n)
      else 0
      end as success_needed_not_di_80_index
    , case
      when a.pct < a.di_80_index_reference then ceil((a.di_80_index_reference - a.pct) * a.n)
      else 0
      end as success_needed_full_parity_80_index
    from
    (
      select
      a.cohort
      , a.subgroup
      , a.pct
      , a.n
      , a.moe
      , a.pct - a.moe as pct_lo
      , a.pct + a.moe as pct_hi
      -- , '{ppg_reference_group}' as ppg_reference_group
      , case
        -- when '{ppg_reference_group}' = 'hpg' then c.hpg_subgroup
        when '{ppg_reference_group}' = 'hpg' then '{ppg_reference_group}' -- like di_iterate
        when '{ppg_reference_group}' in ('overall', 'all but current') then '{ppg_reference_group}'
        else e.ppg_reference_group
        end as ppg_reference_group
      , case
        when '{ppg_reference_group}' = 'overall' then b.overall_rate
        when '{ppg_reference_group}' = 'hpg' then c.hpg_rate
        when '{ppg_reference_group}' = 'all but current' then d.all_but_current_rate
        else e.ppg_specific_group_rate
        end as ppg_reference
      -- , '{di_80_index_reference_group}' as di_80_index_reference_group
      , case
        when '{di_80_index_reference_group}' = 'hpg' then c.hpg_subgroup -- like di_iterate ver. 0.0.19
        when '{di_80_index_reference_group}' in ('overall', 'all but current') then '{ppg_reference_group}'
        else f.di_80_index_reference_group
        end as di_80_index_reference_group
      , case
        when '{di_80_index_reference_group}' = 'overall' then b.overall_rate
        when '{di_80_index_reference_group}' = 'hpg' then c.hpg_rate
        when '{di_80_index_reference_group}' = 'all but current' then d.all_but_current_rate
        else f.di_80_index_specific_group_rate
        end as di_80_index_reference
      from
      (
        select
        cohort
        , subgroup
        , pct
        , weight as n
        , case
          when (1.96 * sqrt(pct_ppg*(1-pct_ppg)/weight)) < {min_moe} then {min_moe}
          else (1.96 * sqrt(pct_ppg*(1-pct_ppg)/weight))
          end as moe
        from
        tb_summ
      ) as a
      left join
      tb_overall as b
      on
      b.cohort = a.cohort
      left join
      tb_hpg as c
      on
      c.cohort = a.cohort
      left join
      tb_all_but_current as d
      on
      d.cohort = a.cohort
      and
      d.subgroup = a.subgroup
      left join
      tb_ppg_specific_group as e
      on
      e.cohort = a.cohort
      left join
      tb_di_80_index_specific_group as f
      on
      f.cohort = a.cohort
    ) as a
    
  )
  -- insert into foo -- for append
  {after_with_statement}
  select
  {select_statement_add}
  cast('{success_var}' as varchar(255)) as success_variable
  , cast('{cohort_var}' as varchar(255)) as cohort_variable
  -- , cast('{cohort_var_no_quote}' as varchar(255)) as cohort_variable
  , cast(a.cohort as varchar(255)) as cohort
  , cast('{group_var}' as varchar(255)) as disaggregation
  , cast(a.subgroup as varchar(255)) as \"group\"
  , a.weight as n
  , a.success
  , a.pct
  -- ppg
  , f.ppg_reference
  , f.ppg_reference_group
  , f.moe
  , f.pct_lo
  , f.pct_hi
  , f.di_indicator_ppg
  , f.success_needed_not_di_ppg
  , f.success_needed_full_parity_ppg
  -- proportionality index
  , e.pct_success / e.pct_group as di_prop_index
  , case when e.pct_success / e.pct_group < {di_prop_index_cutoff} then 1 else 0 end as di_indicator_prop_index
  , case
    when e.pct_success / e.pct_group < {di_prop_index_cutoff} then ceil((e.cohort_success_total * e.pct_group * {di_prop_index_cutoff} - a.success) / (1 - e.pct_group * {di_prop_index_cutoff}))
    else 0
    end as success_needed_not_di_prop_index
  , case
    when e.pct_success / e.pct_group < 1 then ceil((e.cohort_success_total * e.pct_group * 1 - a.success) / (1 - e.pct_group * 1))
    else 0
    end as success_needed_full_parity_prop_index
  -- 80% index
  , f.di_80_index_reference_group
  , f.di_80_index
  , f.di_indicator_80_index
  , f.success_needed_not_di_80_index
  , f.success_needed_full_parity_80_index
  -- into foo -- create table for MS SQL Server
  {end_of_select_statement}
  from
  tb_summ as a
  left join
  tb_group_prop as e
  on
  e.cohort = a.cohort
  and
  e.subgroup = a.subgroup
  left join
  tb_calc as f
  on
  f.cohort = a.cohort
  and
  f.subgroup = a.subgroup
  where
  a.success is not null
  ;
  "
  
  # dbGetQuery(conn=conn, statement=glue(query))
  return(glue(query))
}

##' Iteratively calculate disproportionate impact via the percentage point gap (PPG), proportionality index, and 80\% index methods for many success variables, disaggregation variables, and scenarios, using SQL (for data stored in a database or in a parquet data file).
##'
##' Iteratively calculate disproportionate impact via the percentage point gap (PPG), proportionality index, and 80\% index methods for all combinations of \code{success_vars}, \code{group_vars}, and \code{cohort_vars}, for each combination of subgroups specified by \code{scenario_repeat_by_vars}, using SQL (calculations done on the database engine or duckdb for parquet files).
##' @title Iteratively calculate disproportionate impact using multiple methods for many variables, using SQL.
##' @param db_conn A database connection object, returned by \link[DBI]{dbConnect}.
##' @param db_table_name A character value specifying a database table name.
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
##' @param parallel If \code{TRUE}, then perform calculations in parallel.  The parallel feature is only supported when \code{db_table_name} is a path to a parquet file (\code{'/path/to/data.parquet'}) and that \code{db_conn} is a connection to a \link[duckdb]{duckdb} database (e.g.,  \code{dbConnect(duckdb(), dbdir=':memory:')}).  Defaults to \code{FALSE}.
##' @param parallel_n_cores The number of CPU cores to use if \code{parallel=TRUE}.  Defaults to half of the maximum number of CPU cores on the system.
##' @param mssql_flag User-specified logical flag (\code{TRUE} or \code{FALSE}) that indicates if the MS SQL Server variant of the SQL language should be used.
##' @param return_what A character value specifying the return value for the function call.  For \code{'data'}, the function will return a long data frame with the disproportionate calculations and relevant statistics, after the calculations are performed on the SQL database engine.  For \code{'SQL'}, a list object of individual queries will be returned for the user to execute elsewhere.  Defaults to \code{'data'}.
##' @param staging_table A character value indicating the name of the staging or results table in the database for storing the disproportionate impact calculations.
##' @param drop_staging_table \code{TRUE}/\code{FALSE} A logical flag indicating whether or not the staging table specified in \code{staging_table} should be dropped in the database after the results are returned to R; defaults to \code{TRUE}.
##' @return When \code{return_what='data'} (default), a long data frame is returned (see the return value for \link[DisImpact]{di_iterate}).  When \code{return_what='SQL'} (default), a list object where each element is a query (character value) is returned.
##' @importFrom glue glue
##' @importFrom stringr str_replace_all str_replace str_detect str_extract fixed
##' @import dplyr
##' @import parallel
##' @import duckdb
##' @import DBI
##' @export
di_iterate_sql <- function(db_conn, db_table_name, success_vars, group_vars, cohort_vars=NULL, scenario_repeat_by_vars=NULL, exclude_scenario_df=NULL, weight_var=NULL, include_non_disagg_results=TRUE, ppg_reference_groups='overall', min_moe=0.03, use_prop_in_moe=FALSE, prop_sub_0=0.5, prop_sub_1=0.5, di_prop_index_cutoff=0.8, di_80_index_cutoff=0.8, di_80_index_reference_groups='hpg', check_valid_reference=TRUE, parallel=FALSE, parallel_n_cores=parallel::detectCores() / 2, mssql_flag=FALSE, return_what='data', staging_table=paste0('DisImpact_Staging_', paste0(sample(1:9, size=5, replace=TRUE), collapse='')), drop_staging_table=TRUE) {

  # Following for CRAN: no visible binding for global variable
  success_var <- cohort_var <- where_statement <- select_statement_add <- 
  
  stopifnot(length(group_vars) == length(ppg_reference_groups) | length(ppg_reference_groups) == 1)
  stopifnot(length(group_vars) == length(di_80_index_reference_groups) | length(di_80_index_reference_groups) == 1)

  if (isTRUE(parallel)) {
    if (class(db_conn) == 'duckdb_connection') {
      db_type <- 'duckdb'
      if (!(db_table_name %>% tolower %>% str_detect("\\.parquet'$"))) {
        stop("`parallel=TRUE` is only supported for DB connections (`db_conn` argument) from the duckdb package and when `db_table_name` is a path to a parquet file surrounded by single quotes ('/path/to/data.parquet').")
      }
      if (isFALSE(drop_staging_table)) {
        stop('`drop_staging_table=FALSE` only makes sense when `parallel=FALSE` in order to keep the results written to a single table in a single database.')
      }
    } else {
      stop("`parallel=TRUE` is only supported for DB connections (`db_conn` argument) from the duckdb package and when `db_table_name` is a path to a parquet file surrounded by single quotes ('/path/to/data.parquet').")
    }
  }

  if (!(return_what %in% c('data', 'SQL'))) {
    stop("`return_what` argument only accepts 'data' or 'SQL'.")
  } else if (return_what == 'SQL' & isTRUE(parallel)) {
    stop("`return_what='SQL'` does not work when `parallel=TRUE`.")
  }
  
  check_mssql <- try(db_conn@info$dbms.name == 'Microsoft SQL Server', silent=TRUE)
  if (class(check_mssql) == 'try-error') {
    check_mssql <- FALSE
  }
  if (check_mssql | mssql_flag) {
    mssql <- TRUE
  } else {
    mssql <- FALSE
  }

  # Check for valid variable names for custom query construction
  if(any(str_detect(c(scenario_repeat_by_vars, group_vars, cohort_vars, success_vars), '[^a-zA-Z0-9_]'))) {
    x <- c(scenario_repeat_by_vars, group_vars, cohort_vars, success_vars)
    stop(paste0("Variable names should only contain alphanumeric characters and underscores: ", paste0(x[str_detect(x, '[^a-zA-Z0-9_]')], collapse='; ')))
  }

  # Check if variables are in table
  vars_to_check <- c(success_vars, group_vars, cohort_vars, scenario_repeat_by_vars, weight_var)
  if (mssql) {
    query_check_var <- "
select
top 1
{var_to_check}
from
{db_table_name}
;
"
  } else {
    query_check_var <- "
select
{var_to_check}
from
{db_table_name}
limit 1
;
"
  }

  for (var_to_check in vars_to_check) {
    check_result <- try(dbGetQuery(conn=db_conn, statement=glue(query_check_var)), silent=TRUE)
    if (class(check_result) == 'try-error') {
      stop(glue("Variable not found in table {db_table_name}: {var_to_check}."))
    }
  }

  # Check valid reference groups
  if (check_valid_reference) {
    query_distinct_group <- "
select
distinct
{group_var} as subgroup
from
{db_table_name}
;
"
    for (i in 1:length(group_vars)) {
      group_var <- group_vars[i]
      unique_groups <- dbGetQuery(conn=db_conn, statement=glue(query_distinct_group))$subgroup
      if (!(ppg_reference_groups[pmin(i, length(ppg_reference_groups))] %in% c('overall', 'hpg', 'all but current', unique_groups))) {
        stop(paste0("'", ppg_reference_groups[pmin(i, length(ppg_reference_groups))], "'", " is not valid for the argument `ppg_reference_groups` as it is not one of c('overall', 'hpg', 'all but current'), or it does not exist in the group variable `", group_vars[i], "`."))
      }
      if (!(di_80_index_reference_groups[pmin(i, length(di_80_index_reference_groups))] %in% c('overall', 'hpg', 'all but current', unique_groups))) {
        stop(paste0("'", di_80_index_reference_groups[pmin(i, length(di_80_index_reference_groups))], "'", " is not valid for the argument `di_80_index_reference_groups` as it is not one of c('overall', 'hpg', 'all but current'), or it does not exist in the group variable `", group_vars[i], "`."))
      }
    }
  }
  
  # Check for weight variable
  if(is.null(weight_var)) {
    weight_var <- 1
  }

  # Cohort
  if (is.null(cohort_vars)) {
    cohort_vars <- "'- All'"
  }
  if (length(cohort_vars) != 1 & length(cohort_vars) != length(success_vars)) {
    stop('`cohort_vars` must be of length 1 or the same length as `success_vars` (each success variable corresponds to a cohort variable).')
  }
  
  # Create summary table first
  s_group_by_vars <- paste0(c(scenario_repeat_by_vars, group_vars, cohort_vars), collapse=', ')
  s_calc_missing_flags <- paste0(', case when ', success_vars, ' is null then 1 else 0 end as ', success_vars, '_NA_FLAG', collapse='\n')
  s_missing_flag_vars <- paste0(', ', success_vars, '_NA_FLAG', collapse='\n')
  s_success_vars <- paste0(', ', success_vars, collapse='\n')
  s_calc_success_vars <- paste0(', sum(', success_vars, ') ', 'as ', success_vars, collapse='\n')
  if(mssql) {
    temp_summ_tn <- glue('##', db_table_name, '_summ_')
    before_select_statement <- ''
    before_from_statement <- paste0('into ', temp_summ_tn)
  } else {
    if (str_detect(tolower(db_table_name), "[.]+parquet'$")) {
      temp_summ_tn <- glue(db_table_name %>% tolower %>% str_extract('[a-zA-Z0-9_]+\\.parquet') %>% str_replace('\\.parquet$', ''), '_summ_')
    } else {
      temp_summ_tn <- glue(db_table_name, '_summ_')
    }
    before_select_statement <- glue('create temp table ', temp_summ_tn, ' as')
    before_from_statement <- ''
  }
  if (include_non_disagg_results) {
    s_non_disagg_var <- '"- None"'
    s_non_disagg_var_list <- ', "- None"'
    s_non_disagg_var_create <- ", '- All' as \"- None\""
    group_vars <- c(group_vars, s_non_disagg_var)
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
  } else {
    s_non_disagg_var <- ''
    s_non_disagg_var_list <- ''
    s_non_disagg_var_create <- ''
  }

  query_create_summ <- glue("
{before_select_statement}
select
{s_group_by_vars}
{s_non_disagg_var_list}
{s_missing_flag_vars}
{s_calc_success_vars}
, sum(weight) as weight
{before_from_statement}
from
(
  select
  {s_group_by_vars}
  {s_non_disagg_var_create}
  {s_calc_missing_flags}
  {s_success_vars}
  , {weight_var} as weight
  from
  {db_table_name}
) as a
group by
{s_group_by_vars}
{s_non_disagg_var_list}
{s_missing_flag_vars}
;
")
  # db_table_name <- temp_summ_tn
  weight_var <- 'weight'  
  
  lu_success_cohort <- data.frame(success_var=success_vars, cohort_var=cohort_vars, stringsAsFactors=FALSE)
  lu_group_reference <- data.frame(group_var=group_vars, ppg_reference_group=ppg_reference_groups, di_80_index_reference_group=di_80_index_reference_groups, stringsAsFactors=FALSE)
  dScenarios <- expand.grid(db_table_name=temp_summ_tn # db_table_name
                            , success_var=success_vars
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
    select(db_table_name, success_var, group_var, cohort_var, everything())

  if (!is.null(scenario_repeat_by_vars)) {
    dRepeatScenarios0 <- lapply(scenario_repeat_by_vars
                              , function(cur_var) c(dbGetQuery(conn=db_conn, statement=glue('select distinct {cur_var} from {db_table_name} ; ')) %>% unlist, '- All')
                                ) %>%
      expand.grid(stringsAsFactors=FALSE)
    names(dRepeatScenarios0) <- scenario_repeat_by_vars
    dRepeatScenarios0$where_statement <- do.call("paste"
                                               , c(lapply(1:ncol(dRepeatScenarios0), function(i) paste0(names(dRepeatScenarios0)[i], " = ", "'", dRepeatScenarios0[[i]], "'")), sep=" and ")
                                                 ) %>%
      str_replace_all(" and(?:(?!and).)*'- All'", '') %>% # every "and to '- All'" that is not first
      str_replace_all("^.*'- All'", '') %>% # first "and to '- All'"
      str_replace_all("^ and ", '') # beginning with " and "
    dRepeatScenarios0$where_statement <- ifelse(dRepeatScenarios0$where_statement=='', '', paste0('where ', dRepeatScenarios0$where_statement))
    
    dRepeatScenarios0$select_statement_add <- do.call("paste"
                                                    , c(lapply(1:(ncol(dRepeatScenarios0)-1), function(i) paste0("cast('", dRepeatScenarios0[[i]], "' as varchar(255)) as ", names(dRepeatScenarios0)[i])), sep=", ")
                                                      )
    dRepeatScenarios0$select_statement_add <- paste0(dRepeatScenarios0$select_statement_add, ', ')

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
      full_join(dRepeatScenarios0 %>% select(where_statement, select_statement_add), by=character())
  }

  if (isTRUE(parallel)) {
    n_queries_with_create <- pmin(parallel_n_cores, nrow(dScenarios))
  } else {
    n_queries_with_create <- 1
  }
  if (!mssql) {
    # Not MS SQL Server
    dScenarios$before_with_statement <- ''
    dScenarios$before_with_statement[1:n_queries_with_create] <- glue('create table {staging_table} as')
    dScenarios$after_with_statement <- glue('insert into {staging_table}')
    dScenarios$after_with_statement[1:n_queries_with_create] <- ''
    dScenarios$end_of_select_statement <- ''
  } else {
    dScenarios$before_with_statement <- ''
    dScenarios$after_with_statement <- glue('insert into {staging_table}')
    dScenarios$after_with_statement[1:n_queries_with_create] <- ''
    dScenarios$end_of_select_statement <- ''
    dScenarios$end_of_select_statement[1:n_queries_with_create] <- glue('into {staging_table}')
  }
  
  list_queries <- pmap(dScenarios, di_calc_sql) %>%
    lapply(FUN=function(query) {
      if (mssql) str_replace_all(query, fixed('ceil('), 'ceiling(')
      else query
      }
           )

  if (return_what == 'SQL') {
    return(c(query_create_summ, list_queries))
  }

  # Repeated calculations
  if (length(list_queries) == 1) {
    dbExecute(conn=db_conn, statement=query_create_summ)
    on.exit(dbExecute(conn=db_conn, statement=glue('drop table {temp_summ_tn} ;')), add=TRUE)
    dbExecute(conn=db_conn, statement=list_queries[[1]])
    if (drop_staging_table) {
      on.exit(dbExecute(conn=db_conn, statement=glue('drop table {staging_table} ;')), add=TRUE)
    }
    d_results <- dbGetQuery(conn=db_conn, statement=glue('select * from {staging_table} ;'))
    
  } else if (length(list_queries) > 1 & isFALSE(parallel)) {
    
    dbExecute(conn=db_conn, statement=query_create_summ)
    on.exit(dbExecute(conn=db_conn, statement=glue('drop table {temp_summ_tn} ;')), add=TRUE)
    dbExecute(conn=db_conn, statement=list_queries[[1]])
    if (drop_staging_table) {
      on.exit(dbExecute(conn=db_conn, statement=glue('drop table {staging_table} ;')), add=TRUE)
    }
    query_results <- lapply(list_queries[2:length(list_queries)], FUN=function(query) dbExecute(conn=db_conn, statement=query))
    # query_results <- dbExecute(conn=db_conn, statement=list_queries[2:length(list_queries)] %>% unlist %>% paste0(collapse='\n')) # a single query with multiple statements (many) could actually crash
    d_results <- dbGetQuery(conn=db_conn, statement=glue('select * from {staging_table} ;'))
    
  } else if (length(list_queries) > 1 & isTRUE(parallel)) {
    cl <- makeCluster(parallel_n_cores)
    on.exit(stopCluster(cl), add=TRUE)   
    clusterEvalQ(cl, library(dplyr))
    clusterEvalQ(cl, library(DBI))
    clusterEvalQ(cl, library(odbc))
    clusterEvalQ(cl, library(RSQLite))
    clusterEvalQ(cl, library(duckdb))
    clusterEvalQ(cl, library(pool))
    clusterEvalQ(cl, library(glue))
    clusterExport(cl, varlist=c('list_queries', 'query_create_summ', 'temp_summ_tn', 'staging_table'), envir=environment())
    
    # ## MS SQL
    # clusterEvalQ(cl, conn_remote <- dbConnect(odbc(), dsn='Sandbox_IVC', uid=Sys.getenv('uid'), pwd=Sys.getenv('pwd')))

    # ## SQLite
    # dbExecute(db_conn, 'PRAGMA busy_timeout = 10000')
    # clusterExport(cl, varlist=c('lite_db_file'))
    # clusterEvalQ(cl, db_conn_remote <- dbConnect(drv=SQLite(), lite_db_file))

    # ## DuckDB
    # clusterEvalQ(cl, db_conn_remote <- dbConnect(duckdb(), dbdir='./foo.duckdb'))
    clusterEvalQ(cl, db_conn_remote <- dbConnect(duckdb(), dbdir=':memory:'))
    
    clusterEvalQ(cl, dbExecute(conn=db_conn_remote, statement=query_create_summ))
    
    # query_results <- parLapply(cl, 2:length(list_queries), fun=function(i) dbGetQuery(conn=db_conn_remote, statement=list_queries[[i]]))
    # query_results <- parLapply(cl, 2:length(list_queries), fun=function(i) dbExecute(conn=db_conn_remote, statement=list_queries[[i]]))
    parLapply(cl, 1:parallel_n_cores, fun=function(i) dbExecute(conn=db_conn_remote, statement=list_queries[[i]]))
    if (length(list_queries) > parallel_n_cores) {
      parLapply(cl, (parallel_n_cores+1):length(list_queries), fun=function(i) dbExecute(conn=db_conn_remote, statement=list_queries[[i]]))
    }
    
    d_results <- bind_rows(parLapply(cl, 1:parallel_n_cores, fun=function(i) dbGetQuery(conn=db_conn_remote, statement=glue('select * from {staging_table} ;'))))
    if (drop_staging_table) {
      clusterEvalQ(cl, dbExecute(conn=db_conn_remote, statement=glue('drop table {staging_table} ;')))
    }
    clusterEvalQ(cl, dbExecute(conn=db_conn_remote, statement=glue('drop table {temp_summ_tn} ;')))
    clusterEvalQ(cl, dbDisconnect(conn=db_conn_remote))
  }

  return(d_results)
}
