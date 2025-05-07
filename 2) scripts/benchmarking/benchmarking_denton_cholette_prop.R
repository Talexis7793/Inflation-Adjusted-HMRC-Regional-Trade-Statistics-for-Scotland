# benchmark_denton_cholette v1.0
# created by JSD on 13/03/2024

# Benchmark process for one series at a time, using basic Denton-chollette proportional differences method described
# in the IMF Quarterly National Accounts manual. This uses the td() funcion from library("tempdisagg")
# Note that this could be modified with a better forecast of the BI ratio, as described by the IMF manual,
# for the forward series. The latest edition of the IMF manual now recommends using the 
# Cholette-Dagum method for benchmarking. There does not seem to be an open source R implementation of this yet.
# However, the Chow Lin method can be viewed as a particular case of the the additive Cholette-Dagum method.
# ref: https://www.imf.org/external/pubs/ft/qna/pdf/2017/chapterv6.pdf
# NOTE - should verify that the denton-cholette method is simply the Denton method modified for the starting position, as set
# out in the IMF manual, and not actually the Cholette-Dagum regression method. There is some confusion online!

# The td() function is relatively flexible and deal with monthly, quarterly and annual data for the indicator and reference series
# if both the indicator and reference series have the same frequency, then the former is rescaled and constrained to the latter
# the default is to benchmark the SUM of a high frequency indicator to the reference level, but other options exist (see documentation)

# TO DO processing options to distinguish between quarterly and monthly indicators 

library("tempdisagg")

benchmark_denton_cholette_prop <- function(byvar1,       #first variable name used for sorting unique series, e.g. isicgros or icode
                                      byval1,       #selected unique value to be used
                                      byvar2,       #second variable name used for sorting unique subseries, e.g. isicempbnd or pcode
                                      byval2,       #selected unique value to be used
                                      startyr,      #first full year in reference dataset (cannot begin mid year)
                                      ref_var,      #variable name in reference (benchmark) dataset
                                      ref_data,     #reference (benchmark) dataset source name
                                      ref_freq,     #frequency of reference data, must be same as indicator or lower. 1=ann, 4=quart, 12=mon
                                      ind_var,      #variable name in indicator (high frequency) dataset
                                      ind_data,     #indicator dataset source name
                                      ind_freq      #frequency of indicator. 1=ann, 4=quart, 12=mon
                                      ) 
{
  
  #select reference time series from OCEA QNA long-thin dataset 
  xtemp_reference <- ref_data %>% 
    filter({{byvar1}}==byval1 & {{byvar2}}==byval2 & {{ref_var}} > 0 & year>=startyr)%>% 
    select ({{ref_var}})
  
  #convert to time series format
  xreference_ts <- ts(xtemp_reference, start = startyr, frequency = ref_freq) 
  
  #select high frequency indicator time series from OCEA QNA long-thin dataset
  xtemp_indicator <- ind_data %>% 
    filter({{byvar1}}==byval1 & {{byvar2}}==byval2 & {{ind_var}} > 0 & year>=startyr) %>% 
    select ({{ind_var}})
  
  #convert to time series format
  xindicator_ts <- ts(xtemp_indicator, start = startyr, frequency = ind_freq) 
  
  #benchmark high frequency to indicator using td() function from library(tempdisagg)
  xbmk_ts <- predict(
    td(xreference_ts ~ 0 + xindicator_ts, #For Denton method we must set a 0 intercept in the formula
       # conversion = "sum", #sum default, other options "average", "first", "last
       method = "denton-cholette",
       criterion = "proportional",
    ) 
  )
  
  #make the byvars arguments back into strings to use as varnames in the output
  sbyvar1 <- rlang::enquo(byvar1)
  sbyvar2 <- rlang::enquo(byvar2)
  
  
  # to do make this work for months as well as quarters depending on ind_freq
  #convert benchmarked data back to data frame with year, quarter, and byvars variables
#  xtemp_bmk <- data.frame(value_bmk=as.matrix(xbmk_ts), period=time(xbmk_ts)) %>% 
#    mutate(year=floor(period),subyear = ((period-year)*4)+1, !! sbyvar1 := byval1, !! sbyvar2 := byval2) %>% 
#    select (-period)
  
  if({{ind_freq}} == 4)
  {
    xtemp_bmk <- data.frame(value_bmk=as.matrix(xbmk_ts), period=time(xbmk_ts)) %>% 
    mutate(year=as.integer(floor(period)),subyear = as.integer(round(((period-year)*4)+1)), !! sbyvar1 := byval1, !! sbyvar2 := byval2) %>% 
    select (-period)
    } 
  else if ({{ind_freq}} == 12)
  {
    xtemp_bmk <- data.frame(value_bmk=as.matrix(xbmk_ts), period=time(xbmk_ts)) %>% 
    mutate(year=floor(period),subyear = as.integer(round(((period-year)*12)+1)), !! sbyvar1 := byval1, !! sbyvar2 := byval2) %>% 
    select (-period)
    }
  else
  {print("indicator frequency not quarterly or monthly")}
  
  
  
  
  
  #return benchmarked values for use in global environment
  return(xtemp_bmk)
} #end of benchmark_denton_cholette
