
#This will read in the benchmarking function needed. See the comments there for documentation (and links to theoretical reading)
#This will also source the benchmarking_denton_cholette_prop.R script. That script has the temporal disaggregation (td) function defined, the script sourced in the line below prepares the input data and applies the td function to each unique value of the by variables
source("//s0177a/datashare/OCEA/SNAP/r_code/functions/run_qna_benchmark.R")

#Load data and prepare it for input into the functions
#We want to constrain quarterly and annual time series values. The function expects the quarterly and annual values to be in different dataframes
#It also expects the data to be in long format, with 2 grouping (by) variables
#I am planning to apply the benchmarking to the cvm_series file, which is output from script '3) CVM Calculations.R'.
#There are potentially too many grouping variables in the cvm series data



