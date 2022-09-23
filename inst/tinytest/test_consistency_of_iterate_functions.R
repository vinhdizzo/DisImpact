# This script tests the consistency of the following functions:
# di_iterate, di_iterate_dt, di_iterate_sql

# Needed packages
library(DisImpact)
library(dplyr)
library(data.table)
library(duckdb)
library(glue)
library(tinytest)

# DuckDB connection (in memory)
duck_db <- dbConnect(duckdb(), dbdir=':memory:')

# Data
data(student_equity) # load

student_equity_dt <- as.data.table(student_equity) # data.table

student_equity_parquet_path <- system.file('extdata', 'student_equity.parquet', package='DisImpact', mustWork=TRUE)
student_equity_parquet <- paste0("'", student_equity_parquet_path, "'")

# Summarized Data: for testing weights
student_equity_summ <- student_equity %>%
  group_by(Math, English, Transfer, Cohort_Math, Cohort_English, Cohort, Ethnicity, Gender, Ed_Goal, College_Status) %>%
  summarize(N=n()) %>%
  ungroup %>%
  mutate(Math=Math*N, English=English*N, Transfer=Transfer*N)
dim(student_equity_summ)
tail(student_equity_summ) %>% as.data.frame

student_equity_summ_dt <- as.data.table(student_equity_summ)

dbExecute(conn=duck_db, statement=glue("
create table student_equity_summ as
select
Math * N as Math
, English * N as English
, Transfer * N as Transfer
, Cohort_Math
, Cohort_English
, Cohort
, Ethnicity
, Gender
, Ed_Goal
, College_Status
, N
from
(
  select
  Math
  , English
  , Transfer
  , Cohort_Math
  , Cohort_English
  , Cohort
  , Ethnicity
  , Gender
  , Ed_Goal
  , College_Status
  , count(1) as N
  from
  {student_equity_parquet}
  group by
  Math
  , English
  , Transfer
  , Cohort_Math
  , Cohort_English
  , Cohort
  , Ethnicity
  , Gender
  , Ed_Goal
  , College_Status
) as a
"))

# Data: 2017-Asian missing, to test reference
student_equity_no_2017_asian <- student_equity %>%
  filter(!(Cohort==2017 & Ethnicity=='Asian'))
dim(student_equity)
dim(student_equity_no_2017_asian)

student_equity_dt_no_2017_asian <- student_equity_dt[!(Cohort==2017 & Ethnicity=='Asian')]
dim(student_equity_dt_no_2017_asian)

dbExecute(conn=duck_db, statement=glue("
create table student_equity_no_2017_asian as
select
*
from
{student_equity_parquet}
where
not
(Cohort = 2017 and Ethnicity = 'Asian')
;
"))

# Data: 2017 all non-success on Transfer, for testing the behavior of proportionality index
student_equity_2017_all_non_success <- student_equity %>%
  mutate(Transfer=ifelse(!is.na(Cohort) & Cohort==2017 & !is.na(Transfer), 0, Transfer))
student_equity_2017_all_non_success %>%
  group_by(Cohort, Transfer) %>%
  tally

student_equity_dt_2017_all_non_success <- copy(student_equity_dt)
student_equity_dt_2017_all_non_success[!is.na(Cohort) & Cohort==2017 & !is.na(Transfer), Transfer := 0]
student_equity_dt_2017_all_non_success[, .N, by=.(Cohort, Transfer)]

dbExecute(conn=duck_db, statement=glue("
create table student_equity_2017_all_non_success as
select
*
from
{student_equity_parquet}
;

update
student_equity_2017_all_non_success
set
Transfer=0
where
Cohort=2017
and
Transfer is not null
;
"))

# Data: 2017 1 group for gender to test all but current
student_equity_2017_gender_1_group <- student_equity %>%
  mutate(Gender=ifelse(!is.na(Cohort) & Cohort==2017, 'Male', Gender))
student_equity_2017_gender_1_group %>%
  group_by(Cohort, Gender) %>%
  tally

student_equity_dt_2017_gender_1_group <- copy(student_equity_dt)
student_equity_dt_2017_gender_1_group[!is.na(Cohort) & Cohort==2017, Gender := 'Male']
student_equity_dt_2017_gender_1_group[, .N, by=.(Cohort, Gender)]

dbExecute(conn=duck_db, statement=glue("
create table student_equity_2017_gender_1_group as
select
*
from
{student_equity_parquet}
;

update
student_equity_2017_gender_1_group
set
Gender='Male'
where
Cohort=2017
;
"))

# Data: 2017 has missing values for Math and English outcomes
student_equity_2017_has_missing_outcomes <- student_equity %>%
  mutate(Cohort_Math=ifelse(is.na(Cohort_Math), 2017, Cohort_Math), Cohort_English=ifelse(is.na(Cohort_English), 2017, Cohort_English))
student_equity_2017_has_missing_outcomes %>%
  group_by(Cohort_Math, Math) %>%
  tally

student_equity_dt_2017_has_missing_outcomes <- copy(student_equity_dt)
student_equity_dt_2017_has_missing_outcomes[is.na(Cohort_Math), Cohort_Math := 2017]
student_equity_dt_2017_has_missing_outcomes[is.na(Cohort_English), Cohort_English := 2017]
student_equity_dt_2017_has_missing_outcomes[, .N, by=.(Cohort_Math, Math)]
student_equity_dt_2017_has_missing_outcomes[, .N, by=.(Cohort_English, English)]

dbExecute(conn=duck_db, statement=glue("
create table student_equity_2017_has_missing_outcomes as
select
*
from
{student_equity_parquet}
;

update
student_equity_2017_has_missing_outcomes
set
Cohort_Math=2017
where
Cohort_Math is null
;
update
student_equity_2017_has_missing_outcomes
set
Cohort_English=2017
where
Cohort_English is null
;
"))


# Scenario: single variables
results_tb <- di_iterate(data=student_equity
                       , success_vars=c('Math')
                       , group_vars=c('Ethnicity')
                       , cohort_vars=c('Cohort_Math')
                       # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='overall'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='hpg'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )

results_dt <- di_iterate_dt(dt=student_equity_dt
                          , success_vars=c('Math')
                          , group_vars=c('Ethnicity')
                          , cohort_vars=c('Cohort_Math')
                          # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , ppg_reference_groups='overall'
                          , min_moe=0.03
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups='hpg'
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )

results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name=student_equity_parquet
                            , success_vars=c('Math')
                            , group_vars=c('Ethnicity')
                            , cohort_vars=c('Cohort_Math')
                            # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups='overall'
                            , min_moe=0.03
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups='hpg'
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )

expect_equivalent(results_tb, results_dt, info='single variables: tb vs. dt')
expect_equivalent(results_tb, results_sql %>% mutate(cohort=as.numeric(cohort)), info='single variables: tb vs. SQL')

# Scenario: single variables, no cohort
results_tb <- di_iterate(data=student_equity
                       , success_vars=c('Math')
                       , group_vars=c('Ethnicity')
                       # , cohort_vars=c('Cohort_Math')
                       # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='overall'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='hpg'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )

results_dt <- di_iterate_dt(dt=student_equity_dt
                          , success_vars=c('Math')
                          , group_vars=c('Ethnicity')
                          # , cohort_vars=c('Cohort_Math')
                          # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , ppg_reference_groups='overall'
                          , min_moe=0.03
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups='hpg'
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )

results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name=student_equity_parquet
                            , success_vars=c('Math')
                            , group_vars=c('Ethnicity')
                            # , cohort_vars=c('Cohort_Math')
                            # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups='overall'
                            , min_moe=0.03
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups='hpg'
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )

expect_equivalent(results_tb, results_dt, info='single variables, no cohort: tb vs. dt')
expect_equivalent(results_tb, results_sql, info='single variables, no cohort: tb vs. SQL')

# Scenario: multiple variables
results_tb <- di_iterate(data=student_equity
                       , success_vars=c('Math', 'English', 'Transfer')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                       # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='overall'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='hpg'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )

results_dt <- di_iterate_dt(dt=student_equity_dt
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , ppg_reference_groups='overall'
                          , min_moe=0.03
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups='hpg'
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )

results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name=student_equity_parquet
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups='overall'
                            , min_moe=0.03
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups='hpg'
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )

expect_equivalent(results_tb, results_dt, info='multiple variables: tb vs. dt')
expect_equivalent(results_tb, results_sql %>% mutate(cohort=as.numeric(cohort)), info='multiple variables: tb vs. SQL')

# Scenario: multiple variables, no cohort
results_tb <- di_iterate(data=student_equity
                       , success_vars=c('Math', 'English', 'Transfer')
                       , group_vars=c('Ethnicity', 'Gender')
                       # , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                       # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='overall'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='hpg'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )

results_dt <- di_iterate_dt(dt=student_equity_dt
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          # , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , ppg_reference_groups='overall'
                          , min_moe=0.03
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups='hpg'
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )

results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name=student_equity_parquet
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            # , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups='overall'
                            , min_moe=0.03
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups='hpg'
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )

expect_equivalent(results_tb, results_dt, info='multiple variables, no cohort: tb vs. dt')
expect_equivalent(results_tb, results_sql, info='multiple variables, no cohort: tb vs. SQL')

# Scenario: multiple variables, scenario_repeat_by_vars
results_tb <- di_iterate(data=student_equity
                       , success_vars=c('Math', 'English', 'Transfer')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                       , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='overall'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='hpg'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )

results_dt <- di_iterate_dt(dt=student_equity_dt
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , ppg_reference_groups='overall'
                          , min_moe=0.03
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups='hpg'
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )

results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name=student_equity_parquet
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups='overall'
                            , min_moe=0.03
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups='hpg'
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )

expect_equivalent(results_tb, results_dt, info='multiple variables, scenario_repeat_by_vars: tb vs. dt')
expect_equivalent(results_tb, results_sql %>% mutate(cohort=as.numeric(cohort)), info='multiple variables, scenario_repeat_by_vars: tb vs. SQL')

# Scenario: multiple variables, scenario_repeat_by_vars, no disagg results
results_tb <- di_iterate(data=student_equity
                       , success_vars=c('Math', 'English', 'Transfer')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                       , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=FALSE
                       , ppg_reference_groups='overall'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='hpg'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )

results_dt <- di_iterate_dt(dt=student_equity_dt
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=FALSE
                          , ppg_reference_groups='overall'
                          , min_moe=0.03
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups='hpg'
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )

results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name=student_equity_parquet
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , include_non_disagg_results=FALSE
                            , ppg_reference_groups='overall'
                            , min_moe=0.03
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups='hpg'
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )

expect_equivalent(results_tb, results_dt, info='multiple variables, scenario_repeat_by_vars, no disagg results: tb vs. dt')
expect_equivalent(results_tb, results_sql %>% mutate(cohort=as.numeric(cohort)), info='multiple variables, scenario_repeat_by_vars, no disagg results: tb vs. SQL')

# Scenario: multiple variables, scenario_repeat_by_vars, ppg reference hpg
results_tb <- di_iterate(data=student_equity
                       , success_vars=c('Math', 'English', 'Transfer')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                       , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='hpg'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='hpg'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )

results_dt <- di_iterate_dt(dt=student_equity_dt
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , ppg_reference_groups='hpg'
                          , min_moe=0.03
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups='hpg'
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )

results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name=student_equity_parquet
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups='hpg'
                            , min_moe=0.03
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups='hpg'
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )

expect_equivalent(results_tb, results_dt, info='multiple variables, scenario_repeat_by_vars, ppg reference hpg: tb vs. dt')
expect_equivalent(results_tb, results_sql %>% mutate(cohort=as.numeric(cohort)), info='multiple variables, scenario_repeat_by_vars, ppg reference hpg: tb vs. SQL')

# Scenario: multiple variables, scenario_repeat_by_vars, ppg reference all but current
results_tb <- di_iterate(data=student_equity
                       , success_vars=c('Math', 'English', 'Transfer')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                       , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='all but current'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='hpg'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )

results_dt <- di_iterate_dt(dt=student_equity_dt
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , ppg_reference_groups='all but current'
                          , min_moe=0.03
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups='hpg'
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )

results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name=student_equity_parquet
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups='all but current'
                            , min_moe=0.03
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups='hpg'
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )

expect_equivalent(results_tb, results_dt, info='multiple variables, scenario_repeat_by_vars, ppg reference all but current: tb vs. dt')
expect_equivalent(results_tb, results_sql %>% mutate(cohort=as.numeric(cohort)), info='multiple variables, scenario_repeat_by_vars, ppg reference all but current: tb vs. SQL')

# Scenario: multiple variables, scenario_repeat_by_vars, ppg reference custom
results_tb <- di_iterate(data=student_equity
                       , success_vars=c('Math', 'English', 'Transfer')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                       , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups=c('Asian', 'Male')
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='hpg'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )

results_dt <- di_iterate_dt(dt=student_equity_dt
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , ppg_reference_groups=c('Asian', 'Male')
                          , min_moe=0.03
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups='hpg'
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )

results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name=student_equity_parquet
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups=c('Asian', 'Male')
                            , min_moe=0.03
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups='hpg'
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )

expect_equivalent(results_tb, results_dt, info='multiple variables, scenario_repeat_by_vars, ppg reference custom: tb vs. dt')
expect_equivalent(results_tb, results_sql %>% mutate(cohort=as.numeric(cohort)), info='multiple variables, scenario_repeat_by_vars, ppg reference custom: tb vs. SQL')

# Scenario: multiple variables, scenario_repeat_by_vars, 80% index overall
results_tb <- di_iterate(data=student_equity
                       , success_vars=c('Math', 'English', 'Transfer')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                       , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='overall'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='overall'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )

results_dt <- di_iterate_dt(dt=student_equity_dt
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , ppg_reference_groups='overall'
                          , min_moe=0.03
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups='overall'
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )

results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name=student_equity_parquet
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups='overall'
                            , min_moe=0.03
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups='overall'
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )

expect_equivalent(results_tb, results_dt, info='multiple variables, scenario_repeat_by_vars, 80% index overall: tb vs. dt')
expect_equivalent(results_tb, results_sql %>% mutate(cohort=as.numeric(cohort)), info='multiple variables, scenario_repeat_by_vars, 80% index overall: tb vs. SQL')

# Scenario: multiple variables, scenario_repeat_by_vars, 80% index all but current
results_tb <- di_iterate(data=student_equity
                       , success_vars=c('Math', 'English', 'Transfer')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                       , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='overall'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='all but current'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )

results_dt <- di_iterate_dt(dt=student_equity_dt
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , ppg_reference_groups='overall'
                          , min_moe=0.03
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups='all but current'
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )

results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name=student_equity_parquet
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups='overall'
                            , min_moe=0.03
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups='all but current'
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )

expect_equivalent(results_tb, results_dt, info='multiple variables, scenario_repeat_by_vars, 80% index all but current: tb vs. dt')
expect_equivalent(results_tb, results_sql %>% mutate(cohort=as.numeric(cohort)), info='multiple variables, scenario_repeat_by_vars, 80% index all but current: tb vs. SQL')

# Scenario: multiple variables, scenario_repeat_by_vars, 80% index custom
results_tb <- di_iterate(data=student_equity
                       , success_vars=c('Math', 'English', 'Transfer')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                       , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='overall'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups=c('Asian', 'Male')
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )

results_dt <- di_iterate_dt(dt=student_equity_dt
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , ppg_reference_groups='overall'
                          , min_moe=0.03
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups=c('Asian', 'Male')
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )

results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name=student_equity_parquet
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups='overall'
                            , min_moe=0.03
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups=c('Asian', 'Male')
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )

expect_equivalent(results_tb, results_dt, info='multiple variables, scenario_repeat_by_vars, 80% index custom: tb vs. dt')
expect_equivalent(results_tb, results_sql %>% mutate(cohort=as.numeric(cohort)), info='multiple variables, scenario_repeat_by_vars, 80% index custom: tb vs. SQL')

# Scenario: multiple variables, use_prop_in_moe TRUE
results_tb <- di_iterate(data=student_equity
                       , success_vars=c('Math', 'English', 'Transfer')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                       # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='overall'
                       , min_moe=0.03
                       , use_prop_in_moe=TRUE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='hpg'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )

results_dt <- di_iterate_dt(dt=student_equity_dt
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , ppg_reference_groups='overall'
                          , min_moe=0.03
                          , use_prop_in_moe=TRUE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups='hpg'
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )

results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name=student_equity_parquet
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups='overall'
                            , min_moe=0.03
                            , use_prop_in_moe=TRUE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups='hpg'
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )

expect_equivalent(results_tb, results_dt, info='multiple variables, use_prop_in_moe TRUE: tb vs. dt')
expect_equivalent(results_tb, results_sql %>% mutate(cohort=as.numeric(cohort)), info='multiple variables, use_prop_in_moe TRUE: tb vs. SQL')

# Scenario: multiple variables, min_moe=0.2
results_tb <- di_iterate(data=student_equity
                       , success_vars=c('Math', 'English', 'Transfer')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                       # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='overall'
                       , min_moe=0.2
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='hpg'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )

results_dt <- di_iterate_dt(dt=student_equity_dt
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , ppg_reference_groups='overall'
                          , min_moe=0.2
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups='hpg'
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )

results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name=student_equity_parquet
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups='overall'
                            , min_moe=0.2
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups='hpg'
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )

expect_equivalent(results_tb, results_dt, info='multiple variables, min_moe=0.2: tb vs. dt')
expect_equivalent(results_tb, results_sql %>% mutate(cohort=as.numeric(cohort)), info='multiple variables, min_moe=0.2: tb vs. SQL')

if (at_home()) { # don't test this on CRAN due to parallelization

# Scenario: multiple variables, parallel
results_tb <- di_iterate(data=student_equity
                       , success_vars=c('Math', 'English', 'Transfer')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                       # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='overall'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='hpg'
                       , check_valid_reference=TRUE
                       , parallel=TRUE
                       , parallel_n_cores=4
                         )

results_dt <- di_iterate_dt(dt=student_equity_dt
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , ppg_reference_groups='overall'
                          , min_moe=0.03
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups='hpg'
                          , check_valid_reference=TRUE
                          , parallel=TRUE
                          , parallel_n_cores=4
                            )

results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name=student_equity_parquet
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups='overall'
                            , min_moe=0.03
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups='hpg'
                            , check_valid_reference=TRUE
                            , parallel=TRUE
                            , parallel_n_cores=4
                              )

expect_equivalent(results_tb, results_dt, info='multiple variables, parallel: tb vs. dt')
expect_equivalent(results_tb, results_sql %>% mutate(cohort=as.numeric(cohort)), info='multiple variables, parallel: tb vs. SQL')

} # don't test this on CRAN due to parallelization


# Scenario: invalid success_vars
expect_error({
results_tb <- di_iterate(data=student_equity
                       , success_vars=c('Math1', 'English', 'Transfer')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                       # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='overall'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='hpg'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )
}, info="invalid success_vars: tb")

expect_error({
results_dt <- di_iterate_dt(dt=student_equity_dt
                          , success_vars=c('Math1', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , ppg_reference_groups='overall'
                          , min_moe=0.03
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups='hpg'
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )
}, info="invalid success_vars: dt")

expect_error({
results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name=student_equity_parquet
                            , success_vars=c('Math1', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups='overall'
                            , min_moe=0.03
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups='hpg'
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )
}, info="invalid success_vars: sql")

# Scenario: invalid group_vars
expect_error({
results_tb <- di_iterate(data=student_equity
                       , success_vars=c('Math', 'English', 'Transfer')
                       , group_vars=c('Ethnicity1', 'Gender')
                       , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                       # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='overall'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='hpg'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )
}, info="invalid group_vars: tb")

expect_error({
results_dt <- di_iterate_dt(dt=student_equity_dt
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity1', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , ppg_reference_groups='overall'
                          , min_moe=0.03
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups='hpg'
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )
}, info="invalid group_vars: dt")

expect_error({
results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name=student_equity_parquet
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity1', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups='overall'
                            , min_moe=0.03
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups='hpg'
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )
}, info="invalid group_vars: sql")

# Scenario: invalid cohort_vars
expect_error({
results_tb <- di_iterate(data=student_equity
                       , success_vars=c('Math', 'English', 'Transfer')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math1', 'Cohort_English', 'Cohort')
                       # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='overall'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='hpg'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )
}, info="invalid cohort_vars: tb")

expect_error({
results_dt <- di_iterate_dt(dt=student_equity_dt
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math1', 'Cohort_English', 'Cohort')
                          # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , ppg_reference_groups='overall'
                          , min_moe=0.03
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups='hpg'
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )
}, info="invalid cohort_vars: dt")

expect_error({
results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name=student_equity_parquet
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math1', 'Cohort_English', 'Cohort')
                            # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups='overall'
                            , min_moe=0.03
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups='hpg'
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )
}, info="invalid cohort_vars: sql")

# Scenario: invalid scenario_repeat_by_vars
expect_error({
results_tb <- di_iterate(data=student_equity
                       , success_vars=c('Math', 'English', 'Transfer')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                       , scenario_repeat_by_vars=c('Ed_Goal1', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='overall'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='hpg'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )
}, info="invalid scenario_repeat_by_vars: tb")

expect_error({
results_dt <- di_iterate_dt(dt=student_equity_dt
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          , scenario_repeat_by_vars=c('Ed_Goal1', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , ppg_reference_groups='overall'
                          , min_moe=0.03
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups='hpg'
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )
}, info="invalid scenario_repeat_by_vars: dt")

expect_error({
results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name=student_equity_parquet
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            , scenario_repeat_by_vars=c('Ed_Goal1', 'College_Status')
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups='overall'
                            , min_moe=0.03
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups='hpg'
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )
}, info="invalid scenario_repeat_by_vars: sql")

# Scenario: invalid cohort_vars length
expect_error({
results_tb <- di_iterate(data=student_equity
                       , success_vars=c('Math', 'English', 'Transfer')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math', 'Cohort_English')
                       # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='overall'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='hpg'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )
}, info="invalid cohort_vars length: tb")

expect_error({
results_dt <- di_iterate_dt(dt=student_equity_dt
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English')
                          # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , ppg_reference_groups='overall'
                          , min_moe=0.03
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups='hpg'
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )
}, info="invalid cohort_vars length: dt")

expect_error({
results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name=student_equity_parquet
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English')
                            # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups='overall'
                            , min_moe=0.03
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups='hpg'
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )
}, info="invalid cohort_vars length: sql")

# Scenario: invalid ppg_reference_groups
expect_error({
results_tb <- di_iterate(data=student_equity
                       , success_vars=c('Math', 'English', 'Transfer')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                       # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='overall__'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='hpg'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )
}, info="invalid ppg_reference_groups: tb")

expect_error({
results_dt <- di_iterate_dt(dt=student_equity_dt
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , ppg_reference_groups='overall__'
                          , min_moe=0.03
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups='hpg'
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )
}, info="invalid ppg_reference_groups: dt")

expect_error({
results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name=student_equity_parquet
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups='overall__'
                            , min_moe=0.03
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups='hpg'
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )
}, info="invalid ppg_reference_groups: sql")

# Scenario: invalid ppg_reference_groups custom group
expect_error({
results_tb <- di_iterate(data=student_equity
                       , success_vars=c('Math', 'English', 'Transfer')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                       # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups=c('Asian', 'Non-binary')
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='hpg'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )
}, info="invalid ppg_reference_groups custom group: tb")

expect_error({
results_dt <- di_iterate_dt(dt=student_equity_dt
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , ppg_reference_groups=c('Asian', 'Non-binary')
                          , min_moe=0.03
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups='hpg'
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )
}, info="invalid ppg_reference_groups custom group: dt")

expect_error({
results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name=student_equity_parquet
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups=c('Asian', 'Non-binary')
                            , min_moe=0.03
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups='hpg'
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )
}, info="invalid ppg_reference_groups custom group: sql")

# Scenario: invalid di_80_index_reference_groups
expect_error({
results_tb <- di_iterate(data=student_equity
                       , success_vars=c('Math', 'English', 'Transfer')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                       # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='overall'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='hpg__'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )
}, info="invalid di_80_index_reference_groups: tb")

expect_error({
results_dt <- di_iterate_dt(dt=student_equity_dt
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , ppg_reference_groups='overall'
                          , min_moe=0.03
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups='hpg__'
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )
}, info="invalid di_80_index_reference_groups: dt")

expect_error({
results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name=student_equity_parquet
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups='overall'
                            , min_moe=0.03
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups='hpg__'
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )
}, info="invalid di_80_index_reference_groups: sql")

# Scenario: invalid di_80_index_reference_groups custom group
expect_error({
results_tb <- di_iterate(data=student_equity
                       , success_vars=c('Math', 'English', 'Transfer')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                       # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='overall'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups=c('Asian', 'Non-binary')
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )
}, info="invalid di_80_index_reference_groups custom group: tb")

expect_error({
results_dt <- di_iterate_dt(dt=student_equity_dt
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , ppg_reference_groups='overall'
                          , min_moe=0.03
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups=c('Asian', 'Non-binary')
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )
}, info="invalid di_80_index_reference_groups custom group: dt")

expect_error({
results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name=student_equity_parquet
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups='overall'
                            , min_moe=0.03
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups=c('Asian', 'Non-binary')
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )
}, info="invalid di_80_index_reference_groups custom group: sql")

# Scenario: weight_var for summarized data
results_tb0 <- di_iterate(data=student_equity
                       , success_vars=c('Math', 'English', 'Transfer')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                       , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='overall'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='hpg'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )

results_tb <- di_iterate(data=student_equity_summ
                       , success_vars=c('Math', 'English', 'Transfer')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                       , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , weight_var='N'
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='overall'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='hpg'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )

results_dt <- di_iterate_dt(dt=student_equity_summ_dt
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , weight_var='N'
                          , ppg_reference_groups='overall'
                          , min_moe=0.03
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups='hpg'
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )

results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name='student_equity_summ'
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , weight_var='N'
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups='overall'
                            , min_moe=0.03
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups='hpg'
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )


expect_equivalent(results_tb0, results_tb, info='weight_var for summarized data: unsummarized vs. summarized')
expect_equivalent(results_tb0, results_dt, info='weight_var for summarized data: tb vs. dt')
expect_equivalent(results_tb, results_sql %>% mutate(cohort=as.numeric(cohort)), info='weight_var for summarized data: tb vs. dt')

# Scenario: multiple variables, ppg reference custom (Asian), but 2017 has no Asian students for comparison
results_tb <- di_iterate(data=student_equity_no_2017_asian
                       , success_vars=c('Math', 'English', 'Transfer')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                       # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups=c('Asian', 'Male')
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='hpg'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )

results_dt <- di_iterate_dt(dt=student_equity_dt_no_2017_asian
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , ppg_reference_groups=c('Asian', 'Male')
                          , min_moe=0.03
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups='hpg'
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )

results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name='student_equity_no_2017_asian'
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups=c('Asian', 'Male')
                            , min_moe=0.03
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups='hpg'
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )

expect_equivalent(results_tb, results_dt, info='multiple variables, ppg reference custom (Asian), but 2017 has no Asian students for comparison: tb vs. dt')
expect_equivalent(results_tb, results_sql %>% mutate(cohort=as.numeric(cohort)), info='multiple variables, ppg reference custom (Asian), but 2017 has no Asian students for comparison: tb vs. SQL')

# Scenario: multiple variables, 80 index reference custom (Asian), but 2017 has no Asian students for comparison
results_tb <- di_iterate(data=student_equity_no_2017_asian
                       , success_vars=c('Math', 'English', 'Transfer')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                       # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='overall'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups=c('Asian', 'Male')
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )

results_dt <- di_iterate_dt(dt=student_equity_dt_no_2017_asian
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , ppg_reference_groups='overall'
                          , min_moe=0.03
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups=c('Asian', 'Male')
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )

results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name='student_equity_no_2017_asian'
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups='overall'
                            , min_moe=0.03
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups=c('Asian', 'Male')
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )

expect_equivalent(results_tb, results_dt, info='multiple variables, 80 index reference custom (Asian), but 2017 has no Asian students for comparison: tb vs. dt')
expect_equivalent(results_tb, results_sql %>% mutate(cohort=as.numeric(cohort)), info='multiple variables, 80 index reference custom (Asian), but 2017 has no Asian students for comparison: tb vs. SQL')

# Scenario: multiple variables, when one cohort has 0 success (impact to PI, PPG, and 80 index)
results_tb <- di_iterate(data=student_equity_2017_all_non_success
                       , success_vars=c('Math', 'English', 'Transfer')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                       # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='overall'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='hpg'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )

results_dt <- di_iterate_dt(dt=student_equity_dt_2017_all_non_success
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , ppg_reference_groups='overall'
                          , min_moe=0.03
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups='hpg'
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )

results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name='student_equity_2017_all_non_success'
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups='overall'
                            , min_moe=0.03
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups='hpg'
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )

expect_equivalent(results_tb, results_dt, info='multiple variables, when one cohort has 0 success (impact to PI, PPG, and 80 index): tb vs. dt')
expect_equivalent(results_tb, results_sql %>% mutate(cohort=as.numeric(cohort)), info='multiple variables, when one cohort has 0 success (impact to PI, PPG, and 80 index): tb vs. SQL')

# Scenario: multiple variables, when 1 cohort has just 1 group in Gender, overall reference
results_tb <- di_iterate(data=student_equity_2017_gender_1_group
                       , success_vars=c('Math', 'English', 'Transfer')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                       # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='overall'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='overall'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )

results_dt <- di_iterate_dt(dt=student_equity_dt_2017_gender_1_group
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , ppg_reference_groups='overall'
                          , min_moe=0.03
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups='overall'
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )

results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name='student_equity_2017_gender_1_group'
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups='overall'
                            , min_moe=0.03
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups='overall'
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )

expect_equivalent(results_tb, results_dt, info='multiple variables, when 1 cohort has just 1 group in Gender, overall reference: tb vs. dt')
expect_equivalent(results_tb, results_sql %>% mutate(cohort=as.numeric(cohort)), info='multiple variables, when 1 cohort has just 1 group in Gender, overall reference: tb vs. SQL')

# Scenario: multiple variables, when 1 cohort has just 1 group in Gender, hpg reference
results_tb <- di_iterate(data=student_equity_2017_gender_1_group
                       , success_vars=c('Math', 'English', 'Transfer')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                       # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='hpg'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='hpg'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )

results_dt <- di_iterate_dt(dt=student_equity_dt_2017_gender_1_group
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , ppg_reference_groups='hpg'
                          , min_moe=0.03
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups='hpg'
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )

results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name='student_equity_2017_gender_1_group'
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups='hpg'
                            , min_moe=0.03
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups='hpg'
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )

expect_equivalent(results_tb, results_dt, info='multiple variables, when 1 cohort has just 1 group in Gender, hpg reference: tb vs. dt')
expect_equivalent(results_tb, results_sql %>% mutate(cohort=as.numeric(cohort)), info='multiple variables, when 1 cohort has just 1 group in Gender, hpg reference: tb vs. SQL')

# Scenario: multiple variables, when 1 cohort has just 1 group in Gender, all but current reference
results_tb <- di_iterate(data=student_equity_2017_gender_1_group
                       , success_vars=c('Math', 'English', 'Transfer')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                       # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='all but current'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='all but current'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )

results_dt <- di_iterate_dt(dt=student_equity_dt_2017_gender_1_group
                          , success_vars=c('Math', 'English', 'Transfer')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                          # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , ppg_reference_groups='all but current'
                          , min_moe=0.03
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups='all but current'
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )

results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name='student_equity_2017_gender_1_group'
                            , success_vars=c('Math', 'English', 'Transfer')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English', 'Cohort')
                            # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups='all but current'
                            , min_moe=0.03
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups='all but current'
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )

expect_equivalent(results_tb, results_dt, info='multiple variables, when 1 cohort has just 1 group in Gender, all but current reference: tb vs. dt')
expect_equivalent(results_tb, results_sql %>% mutate(cohort=as.numeric(cohort)), info='multiple variables, when 1 cohort has just 1 group in Gender, all but current reference: tb vs. SQL')

# Scenario: single success var, outcome has NA
results_manually_removed <- di_iterate(data=student_equity_2017_has_missing_outcomes %>% filter(!is.na(Math))
                       , success_vars=c('Math')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math')
                       # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='all but current'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='all but current'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )

results_tb <- di_iterate(data=student_equity_2017_has_missing_outcomes
                       , success_vars=c('Math')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math')
                       # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='all but current'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='all but current'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )

results_dt <- di_iterate_dt(dt=student_equity_dt_2017_has_missing_outcomes
                          , success_vars=c('Math')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math')
                          # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , ppg_reference_groups='all but current'
                          , min_moe=0.03
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups='all but current'
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )

results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name='student_equity_2017_has_missing_outcomes'
                            , success_vars=c('Math')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math')
                            # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups='all but current'
                            , min_moe=0.03
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups='all but current'
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )

expect_equivalent(results_tb, results_manually_removed, info='single success var, outcome has NA: tb vs. manually removed')
expect_equivalent(results_tb, results_dt, info='single success var, outcome has NA: tb vs. dt')
expect_equivalent(results_tb, results_sql %>% mutate(cohort=as.numeric(cohort)), info='single success var, outcome has NA: tb vs. SQL')

# Scenario: multiple success var, outcome has NA
results_manually_removed <- bind_rows(
  di_iterate(data=student_equity_2017_has_missing_outcomes %>% filter(!is.na(English))
                       , success_vars=c('English')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_English')
                       # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='all but current'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='all but current'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )
  , 
  di_iterate(data=student_equity_2017_has_missing_outcomes %>% filter(!is.na(Math))
                       , success_vars=c('Math')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math')
                       # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='all but current'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='all but current'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )
)

results_tb <- di_iterate(data=student_equity_2017_has_missing_outcomes
                       , success_vars=c('Math', 'English')
                       , group_vars=c('Ethnicity', 'Gender')
                       , cohort_vars=c('Cohort_Math', 'Cohort_English')
                       # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                       , include_non_disagg_results=TRUE
                       , ppg_reference_groups='all but current'
                       , min_moe=0.03
                       , use_prop_in_moe=FALSE
                       , prop_sub_0=0.5
                       , prop_sub_1=0.5
                       , di_prop_index_cutoff=0.8
                       , di_80_index_cutoff=0.8
                       , di_80_index_reference_groups='all but current'
                       , check_valid_reference=TRUE
                       , parallel=FALSE
                       # , parallel_n_cores=4
                         )

results_dt <- di_iterate_dt(dt=student_equity_dt_2017_has_missing_outcomes
                          , success_vars=c('Math', 'English')
                          , group_vars=c('Ethnicity', 'Gender')
                          , cohort_vars=c('Cohort_Math', 'Cohort_English')
                          # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                          , include_non_disagg_results=TRUE
                          , ppg_reference_groups='all but current'
                          , min_moe=0.03
                          , use_prop_in_moe=FALSE
                          , prop_sub_0=0.5
                          , prop_sub_1=0.5
                          , di_prop_index_cutoff=0.8
                          , di_80_index_cutoff=0.8
                          , di_80_index_reference_groups='all but current'
                          , check_valid_reference=TRUE
                          , parallel=FALSE
                          # , parallel_n_cores=4
                            )

results_sql <- di_iterate_sql(db_conn=duck_db
                            , db_table_name='student_equity_2017_has_missing_outcomes'
                            , success_vars=c('Math', 'English')
                            , group_vars=c('Ethnicity', 'Gender')
                            , cohort_vars=c('Cohort_Math', 'Cohort_English')
                            # , scenario_repeat_by_vars=c('Ed_Goal', 'College_Status')
                            , include_non_disagg_results=TRUE
                            , ppg_reference_groups='all but current'
                            , min_moe=0.03
                            , use_prop_in_moe=FALSE
                            , prop_sub_0=0.5
                            , prop_sub_1=0.5
                            , di_prop_index_cutoff=0.8
                            , di_80_index_cutoff=0.8
                            , di_80_index_reference_groups='all but current'
                            , check_valid_reference=TRUE
                            , parallel=FALSE
                            # , parallel_n_cores=4
                              )

expect_equivalent(results_tb, results_manually_removed, info='multiple success var, outcome has NA: tb vs. manually removed')
expect_equivalent(results_tb, results_dt, info='multiple success var, outcome has NA: tb vs. dt')
expect_equivalent(results_tb, results_sql %>% mutate(cohort=as.numeric(cohort)), info='multiple success var, outcome has NA: tb vs. SQL')

