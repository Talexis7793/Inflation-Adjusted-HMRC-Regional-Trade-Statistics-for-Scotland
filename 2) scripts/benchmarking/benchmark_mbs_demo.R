# Explanation ----

# Demonstration of general purpose benchmarking function using nested loops for two stratified 'byvars'.
# In this example we take weighted Monthly Business Survey data from the GDP system up to Jan 2024, by sic and employment band.
# First, we summarise the weighted turnover for each group to quarterly turnover and constrain this to annualised IDBR turnover 
# for the same groups (IDBR until 2019). We would never do this in reality, but it works as an example.
# Second, we run the benchmarking on the monthly data using the same function.
# This wrangles the data through an open source implementation (tempdisagg) of the Denton proportional first differences benchmarking method 
# (with modified starting point by Cholette) widely used in national accounts and previously encoded for SAS by Gary Campbell.
# Benchmarking can be viewed as constraining a high frequency indicator series to a low frequency reference series, or equivalently as
# interpolating and extrapolating a high frequency estimate of the reference series using movements in the indicator.
# The functions remain under development to improve their general purposeness. 
# Known issues so far include not forcing the indicator to be strictly positive, and not working with quarterly reference series 

# Load libraries and functions ----

library("tidyverse")

source("//s0177a/datashare/OCEA/SNAP/r_code/functions/run_qna_benchmark.R")

#work with a standard long thin data format which includes columns for:
# year ; quarter or month ; classification variable 1 ; classification variable 2 ; >=1 other columns (one of which will be benchmarked)  

# Load datasets from SAS and wrangle them ----


# read monthly business survey data from SAS library and reformat including sum to quarters
mbs_1 <- haven::read_sas("//s0177a/sasdata2/gdp/gdplive/grossed_turnover_empbnd.sas7bdat")

mbs_2 <- mbs_1 %>% 
  rename(year = yr) %>%
  mutate(quarter = ceiling(month/3)) %>% 
  group_by(isicgros, isicempbnd, year, quarter) %>% 
  summarise(mbs_turnover = sum(weighted_turnover)) %>%
  select(isicgros, isicempbnd, year, quarter, mbs_turnover) %>%
  filter(year <= 2023) %>%                                             #filter to end at the last full quarter
  ungroup()

# read annual IDBR turnover data from SAS library and reformat it, including scale monthlified turnover value to annual
idbr_1 <- haven::read_sas("//s0177a/sasdata2/gdp/gdplive/all_idbr_testing.sas7bdat")

idbr_2 <- idbr_1 %>%
  mutate(iturnov = iturnov*12) %>% 
  rename(year = yr) %>% 
  filter(year<=2019)                                                   #filter to end at 2019, just to simulate a lagged base year


# benchmark quarterly MBS to the annual IDBR turnover ----
mbs_quart_bmk <- run_qna_benchmark(
  isicgros,            #byvar1
  isicempbnd,          #byvar2
  mbs_2,               #indicator dataset 
  mbs_turnover,        #indicator variable
  4,                   #indicator frequency 4=quarterly 12=monthly
  idbr_2,              #reference dataset
  iturnov,             #reference variable (the benchmark levels)
  1                    #reference frequency 1=annual (only annual for now, but will be extended to quarterly and monthly)
  )

#merge back on the monthlified (annual/12) IDBR values as a reference level to examine in output
mbs_quart_bmk <- left_join(mbs_quart_bmk,idbr_2,by=c("isicgros","isicempbnd","year")) %>% 
  mutate(iturnov = iturnov/4, BI_ratio = bmk_value/mbs_turnover)  %>% 
  rename(mbs_turnover_bmk = bmk_value)

#output to csv, because why not
write.csv(mbs_quart_bmk,"mbs_idbr_bmk_quart.csv")


# benchmark monthly MBS to the annual IDBR turnover ----
mbs_3 <- mbs_1 %>% 
  rename(year = yr, mbs_turnover = weighted_turnover) %>%
  select(isicgros, isicempbnd, year, month, mbs_turnover)

# benchmark quarterly MBS to the annual IDBR turnover to 2019
mbs_mon_bmk <- run_qna_benchmark(
    isicgros,            #byvar1
    isicempbnd,          #byvar2
    mbs_3,               #indicator dataset 
    mbs_turnover,        #indicator variable
    12,                   #indicator frequency 4=quarterly 12=monthly
    idbr_2,              #reference dataset
    iturnov,             #reference variable (the benchmark levels)
    1                    #reference frequency 1=annual (only annual for now, but will be extended to quarterly and monthly)
  )

#merge on the monthlified (annual/12) IDBR values as a reference level to examine in output
mbs_mon_bmk <- left_join(mbs_mon_bmk,idbr_2,by=c("isicgros","isicempbnd","year")) %>%
  mutate(iturnov = iturnov/12, BI_ratio = bmk_value/mbs_turnover) %>% 
  rename(mbs_turnover_bmk = bmk_value)

#output to csv, because why not
write.csv(mbs_mon_bmk,"mbs_idbr_bmk_mon.csv")

  
