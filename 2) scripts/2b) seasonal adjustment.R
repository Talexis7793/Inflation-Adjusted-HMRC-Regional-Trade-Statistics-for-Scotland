library(tidyverse)
library(seasonal)
library(shiny)
library(rlang)
library(forecast) #for detecting (and maybe dealing with) outliers
#to speed up loop processing:
    library(furrr)
    library(purrr)
library(truncnorm) # for adding random noise
options(scipen=999) # turn off scientific notation
set.seed(02052024) # Set seed for reproducibility

#set start time
start_time <- Sys.time()

#Read in the seasonal adjustment loop function
source("./2) scripts/Apply seasonal adjustment function.R")

#This program is to be run inbetween steps 2) data processing and 3) CVM calculations.

#############################################
#IMPORTANT:
#note that seasonal adjustment can introduce some strange results when there is a strong irregular component in the input series, or when the data are non-stationary
#or when there is a lot of low to no-value trade in some periods with some irregular spikes. Even if these irregular spikes are small it can lead to the model massively
#over or under-compensating in some cases. In the 'seasonal adjustment QA.R' program you can see that the series which have the big differences between the original and 
#seasonally adjusted values are often series with erratic trade or with period of very low value. A consistent method to identify erratic series or ones with sufficiently 
#low trade in some periods to that they can be excluded from seasonal adjustment (and probably CVM as well) is very desirable.
#############################################

#Read in data we want to seasonally adjust
if(all_uk == FALSE) {

  if(country_breakdown == FALSE) {
  current_price_values <- read_csv(paste0("1) data/raw/RTS UK and Scotland ", data_date , api_suffix,".csv"))
  output_file <- paste0("3) outputs/CP SA - UK & Scotland ",data_date, ".csv")
  }
  
  if(country_breakdown == TRUE) {
    
    if(country_product_breakdown == TRUE) {
      if(single_country == FALSE) {
        current_price_values <- read_csv(paste0("1) data/raw/RTS UK and Scotland - dest country & product breakdown - ", data_date, api_suffix, ".csv")) %>% 
          filter(country != "UK") #Remove UK if we are looking this granular
        output_file <- paste0("3) outputs/CP SA - UK & Scotland - dest country & product breakdown - ",data_date, ".csv")
      }
      if(single_country != FALSE){
        current_price_values <- read_csv(paste0("1) data/raw/RTS ", region_choice, " - ", single_country, " product breakdown - ",data_date,".csv"))
        output_file <- paste0("3) outputs/CP SA - ", region_choice, " - ", single_country," product breakdown - ",data_date, ".csv")
      }
    }
    if(country_product_breakdown == FALSE) { 
      current_price_values <- read_csv(paste0("1) data/raw/RTS UK and Scotland - dest country breakdown - ", data_date, api_suffix, ".csv"))
      output_file <- paste0("3) outputs/CP SA - UK & Scotland - dest country breakdown - ",data_date, ".csv")
    }
    
  }
  
}

if(all_uk == TRUE) {
  current_price_values <- read_csv(paste0("1) data/raw/RTS regions and countries - ", data_date, api_suffix, ".csv"))
  output_file <- paste0("3) outputs/CP SA - all UK ",data_date, ".csv")
}

#remove the series causing a problem and just add it later - this series was causing an error with the seasonal adjustment function. The model was not invertible (a desirable mathetmatical property).
#R ensures that the model is both stationary and invertible before returning results
series_with_error <- filter(current_price_values, partner == "Morocco" & ProductCode == "73")
current_price_values <- filter(current_price_values, !(partner == "Morocco" & ProductCode == "73"))

#updates automatically:
earliest_date_in_data <- paste(year(min(current_price_values$date)),".",month(min(current_price_values$date)), sep = "") # format must be e.g. ("2017.1")

#create a list of all the UK nations we are interested in. This will be used later
  if(all_uk == TRUE) {
    reporter_list <- c("England", "UK", "Scotland", "Wales", "Northern Ireland")
  }
  
  if(all_uk == FALSE) {
    reporter_list <- c("UK", "Scotland")
    if(country_breakdown == TRUE) {
      reporter_list <- c("Scotland")
    }
  }

#Define the pandemic period as another variable to consider in constructing the seasonal adjustment model for each series:
#this works if starting in 2016, COVID effects starts in start of 2019 and ends at the end of 2021:
#also need to add code to toggle whether this is on or off (see email from ONS specialists):
list_quarters_post_covid <- unique(current_price_values$date)
list_quarters_post_covid <- list_quarters_post_covid[ format(list_quarters_post_covid, "%Y") > 2021]
number_of_quarters_post_covid <- length(list_quarters_post_covid) + 4 #adding 4 because we need another year to account for the forecast in the seasonal adjustment

covid <- c(rep(0, times = 12), rep(1, times = 12), rep(0, times = number_of_quarters_post_covid))
covid_ts <- ts(covid,
               start = c(2016,1), 
               frequency = 4)
rm(covid)

######################################################################################################################################
                  #THIS SECTION OF THE CODE IS FOR TESTING, IT IS NOT NEEDED TO COMPLETE THE CVM ANALYSIS, BUT MIGHT BE USEFUL IN FURTHER DEVELOPMENTS
                  #Testing the seasonal adjustment for just one series (international exports, totals excluding erratic for Scotland). This code can probably be removed once the loop is set up to run this for all breakdowns separately.
                  #Using value as ref_var just to test, this step filters down all the by variables so we just have one series, then selects only the variable we want to plot
                  
                  #convert to time series format
                  # cvm_time_series <- ts(cvm_time_series, start = 2014, frequency = 4)   #indicator frequency: 4=quarterly, 12=monthly
                  # #View time series object
                  # plot(cvm_time_series)
                  # 
                  # #perform seasonal adjustment
                  # adjusted_cvm_ts_1 <- seas(cvm_time_series)
                  # #adjusted_cvm_ts_2 <- seas(cvm_time_series, x11 = "")
                  # plot(adjusted_cvm_ts_1)
                  # #plot(adjusted_cvm_ts_2)
                  # 
                  # #View adjusted series:
                  # final(adjusted_cvm_ts_1)
                  # summary(adjusted_cvm_ts_1)
                  # #summary(adjusted_cvm_ts_2)
                  # 
                  # #Inspect the seasonal adjustment model, this is to help summarise all relevant options for the model - does not work for some reason
                  # inspect(adjusted_cvm_ts_1)
                  # 
                  ######################################################################################################################################
#Preprocessing:

  #If the sum of a whole series = 0 then remove the series, 
  #if either the value or the volume = 0 then I am removing the series. 
  #This may remove some cases where either the value or volume is 0 but not the other. I am not too worried about this because this really doesn't make sense anyway.
sum_series <- current_price_values %>%
  group_by(country, flowtype, partner, ProductCode) %>% 
  summarise(sum_value = sum(value),
            sum_netmass = sum(netmass)) %>% 
  ungroup() %>% 
  mutate(zero_value = ifelse(sum_value == 0, 1, 0),
         zero_netmass = ifelse(sum_netmass == 0, 1, 0)) %>% 
  select(-sum_value, - sum_netmass)

current_price_values <- current_price_values %>% 
  left_join(sum_series, by = c("country", "flowtype", "partner", "ProductCode")) %>% 
  filter(zero_value == 0 & zero_netmass == 0) %>% 
  select(-zero_value, -zero_netmass)


#Function to run the seasonal adjustment in a loop, this is done to create a separate series for each unique value of the 2 strata variables
run_sa_loop <- function(
    reporter,   #adding this, and we will have to call this function twice, once for UK, once for Scotland
    flow,  #also adding this, so again we will need to call the function for imports and exports for 2*2 = 4 total calls. I'm doing it this way because there are too many by variables in our data and I don't want to have to add extra loops
    byvar1,
    byvar2,
    ind_data,
    ind_var,
    ind_freq = 4,  #default to quarterly data
    new_var_name
)
          {
  
  
            # set product categories that you want to use to filter. This is so the SA can be done at each appropriate level of aggregation
            # all_products <- current_price_values %>% pull(ProductCode) %>% unique()
            # erratic <- c("66","68", "79", "93", "97")
            # sitc3 <- all_products[which(substr(all_products, 0,1) == "3")] # MIGHT NEED TO ADD MORE
            # all_excl_79 <- all_products[!all_products %in% c("79")]
            # all_excl_erratic <- all_products[!all_products %in% c(erratic)]
            # all_excl_erratic_sitc3 <- all_products[!all_products %in% c(erratic, sitc3, "NAvalue")]
   
            #Make a character value for the current reporter country we are looking at
            reporter_chr <- reporter_list[reporter]
            
            #Filter for flowtype and country here
            ind_data_filt <- ind_data %>% 
              filter(flowtype == flow,
                     country == reporter_chr)
            
            if({{ind_freq}} == 4){
              xind_data <- ind_data_filt %>%
              select(date, {{byvar1}}, {{byvar2}}, {{ind_var}})
              }
            
            indnames <- colnames(xind_data)
            
            colnames(xind_data) <- c("date","bylevel1","bylevel2","indicator")
            
            #find the start year and date
            start_date_ref <- xind_data %>%
              filter(date == min(date)) %>% 
              distinct(date) %>% 
              select(date)
            
            startdt <- pull(start_date_ref[1,1])
            
            start_year_ref <- xind_data %>%
             mutate(year = year(date)) %>% 
              filter (year == min(year)) %>% 
              distinct(year) %>% 
              select (year)
            
            startyr <- pull(start_year_ref[1,1])
            
            # create a loop for all distinct values of byvar1 for benchmarking
            list_byvar1 <- ind_data_filt %>%
             # filter({{byvar1}} != "NA" & {{ind_var}} != "NA" & {{ind_var}} > 0) %>% 
              filter({{byvar1}} != "NA") %>% #This was more restrictive in the code I inherited, but I would rather all (or most at least) values are captured in this list. Any problems with individual values can be investigated as they arise, otherwise some series are silently not seasonally adjusted. 
              distinct({{byvar1}})
            
            #Some series have missing rows in their time series, we need to fill these in or there will be an error. To do this create a vector of the dates in the range we are interested in
            start_date <- min(xind_data$date)
            end_date <- max(xind_data$date)
            ts_vec <- seq(from = start_date, to = end_date, by = "quarter") 
            ts_vec <- as.data.frame(ts_vec)
            ts_vec <- rename(ts_vec, date = ts_vec)
        
            
            for (j in 1:nrow(list_byvar1))
            {# create a loop for all distinct values of byvar2
              
              j_val <- pull(list_byvar1[j,1])
              
              print(j_val)
              
              list_byvar2 <- ind_data_filt %>%
               # filter({{byvar1}} == j_val & {{ind_var}} > 0) %>%
                filter({{byvar1}} == j_val) %>% # This did say ind_var had to be greater than 0, but I want to be sure all the series are captured. We can deal with problems in individual series as they arise
                distinct({{byvar2}})
              
              #Only continue if there is at least one entry in the by var 2 list. Otherwise this will cause an error. Commenting this out for now, because it was only an issue for the country breakdowns and I want to be aware of all series that are not being seasonally adjusted in this way
          #if(length(list_byvar2$ProductCode) > 0) {
          
          #apply the seasonal adjustment function to each series
          
              adjusted_cpv_df <- purrr::map_df(1:nrow(list_byvar2),
                                    \(x)  apply_seasonal_adjustment(input_data = xind_data,
                                                                    j_val_input = j_val,
                                                                    i_val_input = x,
                                                                    start_date = startdt,
                                                                    ts_vec = ts_vec,
                                                                    i_list = list_byvar2,
                                                                    ind_freq = ind_freq,
                                                                    startyr = startyr)  
                                      ) 
            
          ######Additional processing needed here to combine the different series
          #If this is the very first series then create the file that will ultimately contain all the series
          if(j == 1) {
            all_sa_data <- adjusted_cpv_df
          }
          
          #otherwise bind the output from the function onto the accumulated results dataset
          if(!(j == 1)) {
            all_sa_data <- bind_rows(all_sa_data, adjusted_cpv_df)
          }
          
              #} #end condition to check there are enough entries in list_byvar2 # commented out for now, see comment above
            } #end for byvar1 loop
          
        
            all_sa_data <- all_sa_data %>% 
              select(date, seasonaladj, partner, ProductCode, excluded_series)
            
            #Join the seasonadj column back onto the original data. This step ensures that the imputed values (for missing data) are not retained in the final output. E.g. if a series had only rows for 2016 and 2018 in the input data, it will be the same in the output. The imputed values are only there to help the seasonal adjustment calculations for values that did exist in the input.
            sa_output <- left_join(ind_data_filt, all_sa_data, by=c("date","partner","ProductCode")) %>% #note this is not joining by reporter country and flow, because the data will be filtered for each flow and reporter each time this function is called.
              rename({{new_var_name}} := seasonaladj)
            
            return(sa_output)
          
          
} # end function 

#Run this function in a loop for each reporting nation. This needs to be done separately for both flow types and for netmass and value for 2*2 = 4 total function calls run in a loop for each of the 5 nations. 4*5 = 20 total function calls counting the ones that run in the loops                  


#In order to speed up processing, this code may be useful, then we also need to change purrr::map_df to furrr::map_dfr (or something like this) - it didn't work when I tried it, but the below code does
        #Check available cores
        num_cores <- future::availableCores()
        
        # set workers to number of cores, this should parallelise the processing for a gain in speed
        future::plan(future::multisession, workers = num_cores)

#Exports / value
###############################################################################################################
   #     start_time <- Sys.time()
                #future_map_dfr will row bind the output dataframes together in the final output
  #      current_price_values_exports_output <- furrr::future_map_dfr(1:length(reporter_list),
  #                                            ~ run_sa_loop(
  #                                                reporter = .x,
  #                                                flow = "Exports",
  #                                                byvar1 = partner,
  #                                                byvar2 = ProductCode,
  #                                                ind_data = current_price_values,
  #                                                ind_var = value,
  #                                                ind_freq = 4,
  #                                                new_var_name = "value_sa"
  #                                                                    ),
  #                                            .progress = TRUE) #add a progress bar
  #      end_time <- Sys.time()
        
current_price_values_exports_output <- purrr::map_df(1:length(reporter_list),
                                                \(x) run_sa_loop(
                                                    reporter = x,
                                                    flow = "Exports",  
                                                    byvar1 = partner,
                                                    byvar2 = ProductCode,
                                                   ind_data = current_price_values,
                                                   ind_var = value,
                                                   ind_freq = 4,
                                                   new_var_name = "value_sa"
                                                   ))
###############################################################################################################


#Imports / value
  
current_price_values_imports_output <- purrr::map_df(1:length(reporter_list),
                                                \(x) run_sa_loop(
                                                  reporter = x,
                                                   flow = "Imports",
                                                   byvar1 = partner,
                                                   byvar2 = ProductCode,
                                                   ind_data = current_price_values,
                                                   ind_var = value,
                                                   ind_freq = 4,
                                                   new_var_name = "value_sa"
                                                  ))
#################################################################################################################
#Netmass / Exports
current_price_volumes_exports_output <- purrr::map_df(1:length(reporter_list),
                                               \(x) run_sa_loop(
                                                 reporter = x,   
                                                 flow = "Exports",  
                                                 byvar1 = partner,
                                                 byvar2 = ProductCode,
                                                 ind_data = current_price_values,
                                                 ind_var = netmass,
                                                 ind_freq = 4,
                                                 new_var_name = "netmass_sa"
                                                 ))

#Netmass / Imports
current_price_volumes_imports_output <- purrr::map_df(1:length(reporter_list),
                                               \(x) run_sa_loop(
                                                 reporter = x,   
                                                 flow = "Imports",  
                                                 byvar1 = partner,
                                                 byvar2 = ProductCode,
                                                 ind_data = current_price_values,
                                                 ind_var = netmass,
                                                 ind_freq = 4,
                                                 new_var_name = "netmass_sa"
                                                 ))



current_price_values_sa <- bind_rows(current_price_values_exports_output, current_price_values_imports_output) %>% 
  rename(excluded_series_value = excluded_series)

current_price_volumes_sa <- bind_rows(current_price_volumes_exports_output, current_price_volumes_imports_output) %>% 
  rename(excluded_series_volume = excluded_series)
  
#There are fewer rows in the final output because we have not included unallocated regions separately. They are aggregated in the UK total but are not separated out
current_price_sa_output <- current_price_values_sa %>% 
  left_join(current_price_volumes_sa, by=c("date","country","flowtype", "partner", "ProductCode", "Sitc2Desc", "value", "netmass"))
  
rm(current_price_values_exports_output, current_price_values_imports_output,
  current_price_volumes_exports_output, current_price_volumes_imports_output,
  current_price_values_sa, current_price_volumes_sa)

#Add back in any series causing errors unadjusted
series_with_error <- series_with_error %>% 
  mutate(excluded_series_value = 1,
         excluded_series_volume = 1,
         value_sa = value,
         netmass_sa = netmass)

#Add back in any series that were causing an error and needed to be excluded, then calculate the difference
#between each field and it's seasonally adjusted counterpart so that we can see the impact
current_price_sa_output <- bind_rows(current_price_sa_output, series_with_error) %>% 
  mutate(diff_sa_netmass = netmass - netmass_sa,
         diff_sa_value = value - value_sa)

if(write_to_csv == TRUE){write_csv(current_price_sa_output, output_file)}

#calculate run time
end_time <- Sys.time()
run_time_seas <- end_time - start_time


