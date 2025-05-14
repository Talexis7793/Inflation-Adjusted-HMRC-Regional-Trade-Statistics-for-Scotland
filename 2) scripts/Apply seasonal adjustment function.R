apply_seasonal_adjustment <- function(input_data, 
                                      j_val_input,
                                      i_val_input,
                                      #Past this point the arguments all have defaults, I need to pass arguments between the two functions I have written, that is all this accomplishes. The values passed in here will be evaluated at each iteration of the loop in the other script 
                                        start_date = startdt,
                                        ts_vec = ts_vec,
                                        i_list = list_byvar2,
                                        ind_freq = ind_freq, 
                                        startyr = startyr) {
  
  
  #get("list_byvar2", envir = run_sa_loop) #This doesn't work, I don't know how to reference the function environment for the function that calls this function. It would be useful to get it working though. 
  i_val <- pull(i_list[i_val_input,1]) #convert the position of the i value to the actual name of the category from the list
  print(i_val)
  ###################################PREPROCESSING#####################################################################################################

      current_price_values_filt <- input_data %>%
        #filter(bylevel1 == j_val_input & bylevel2 == i_val & indicator > 0 & date>=start_date) %>%
        filter(bylevel1 == j_val_input & bylevel2 == i_val & date>=start_date) %>% #Might want to remove indicator = 0, the mean will be imputed here, but we cannot run automatic outlier adjustment with the 0 values in the dataset.
        select(indicator, date)
      
        #Some products are in the product list but have no trade values for a particular partner (e.g. UK trades in something but not Scotland).
        #This will cause the code to fail, so only continue if the number of rows is not 0 (ie there is at least some trade in that product)
        #There is probably a clearer way to just stop the code and return nothing, but using an if statement for now
        if(nrow(current_price_values_filt != 0)) {
  
      #process series with missing rows:
      if(nrow(current_price_values_filt) != nrow(ts_vec)) {
        #If the number of rows in the filtered dataframe is not equal to the expected length of the time series we are looking at, we need to impute some rows for this particular series
        #Add Index rows for joining
        
        current_price_values_filt <- current_price_values_filt #%>% 
          #mutate(row_id = seq.int(nrow(current_price_values_filt)))
        
        mean_of_short_series <- current_price_values_filt %>% 
          summarise(series_mean = mean(indicator)) %>% 
          pull(series_mean)
        
        ts_vec <- ts_vec 
        #%>% 
          #mutate(row_id = seq.int(nrow(ts_vec)))
        
        #Create a dataframe that has the correct number of rows for the full time series, so that we can impute missing vals
        current_price_values_filt <- ts_vec %>% 
          left_join(current_price_values_filt, by = join_by(date == date)) %>% 
          #add random noise to mean imputations
          mutate(random_multiplier = rtruncnorm(nrow(ts_vec), 
                                                a = 0.6, 
                                                b = 1.4, 
                                                mean = 1, 
                                                sd = 0.1))
        
        #Any rows that are NA, impute the mean (ie. assume the mean value/volume was traded in that flow for that quarter). 0 does not work here; the seasonal adjustment function returns an error, so we also need to do the same for periods with 0 traded (see below code)
        current_price_values_filt$indicator[is.na(current_price_values_filt$indicator)] <- mean_of_short_series
        
        
        #Finally, if we have imputed the mean, add some random noise to it by multiplying it by a random number drawn from the normal distribution (parameters found in the rtruncnorm function arguments)
        current_price_values_filt <- current_price_values_filt %>% 
          mutate(indicator_plus_noise = ifelse(indicator == mean_of_short_series,
                                               indicator*random_multiplier,
                                               indicator)) %>% 
          select(indicator_plus_noise) %>% 
          rename(indicator = indicator_plus_noise) |> 
          mutate(indicator = as.integer(indicator)) #change from double to integer
          
        
        ts_vec <- ts_vec #%>% 
          #select(-row_id)
      }
          
    #now treat series that have any 0 values (impute some noise to show very low trade). This is because the automatic outlier detection in the seas function cannot handle 0 values
      if(any(current_price_values_filt$indicator == 0)) {
      
        #add random noise
        zero_series_noise <- current_price_values_filt |>
          filter(indicator == 0) |> 
          mutate(noise = rtruncnorm(#nrow(current_price_values_filt[current_price_values_filt$indicator == 0,]), 
                                    sum(current_price_values_filt$indicator == 0),
                                    a = 0.6, 
                                    b = 1.4, 
                                    mean = 1, 
                                    sd = 0.1))
        
        current_price_values_filt$indicator[(current_price_values_filt$indicator) == 0] <- zero_series_noise$noise  
         
      }
          
  #remove the date field if it is still there
  current_price_values_filt <- current_price_values_filt %>% 
    select(indicator)
          
  #############################APPLY SEASONAL ADJUSTMENT################################################################################
  
  #If the sd of the indicator is too low the seasonal adjustment will fail, we cannot do it for these series so just take a note of it
  if(sd(current_price_values_filt$indicator) > 1) {
    
    #convert to time series format
    current_price_values_ts <- ts(current_price_values_filt, 
                                  start = c(startyr,1), 
                                  frequency = ind_freq)   #indicator frequency: 4=quarterly, 12=monthly
  
    #perform seasonal adjustment

    adjust_seasonal <- function(ts_data, 
                                time_span) {
      tryCatch({
        # Apply the seasonal adjustment
        #note in the seas function an empty string "" means the spec is enabled but has no arguments
        adjusted_cpv_ts <- seas(ts_data,
                                pickmdl = "", #use the pickmdl algorithm to select the most appropriate model. This is one of 3 options: arima, pickmdl or automdl (default)
                                x11="", #use the alternate x11 procedure (as advised by colleagues in national accounts) as opposed to the default SEATS
                                #estimate.maxiter=10000, #This might be needed if the model picking algorithm does not converge
                                series.span = time_span,
                                outlier = NULL, #the default of the outlier argument is with automatic outlier detection enabled, set to NULL to turn off
                                #The outlier special is used to perform automatic detection of additive (point) outliers, temporary change outliers, level shifts,
                                #or any combination of the three using the specified model.
                                #IMPORTANT: this feature was causing some very strange results (ironically introducing results that were huge outliers in the seasonally adjusted results where there
                                #had been none before). This could be due to innappropriate rectification of outliers due to the specified model not being appropriate. 
                                #There are so many series to adjust that it is not practical to manually assess each one for suitability of the ARIMA model applied (this could be done using ACF and PACF plots
                                #to assess whether the number of parameters (p,d,q) in the ARIMA model is correct). So I will check for outliers and differences between seasonally adjusted values and the 
                                #input values in the output. 
                                
                                #Note remainder(adjusted_cpv_ts) should retrieve the irregular (remainder) compenent. seasadj(adjusted_cpv_ts) gets the seasonally adjusted time series (this is just another way to 
                                #access the model rather than manually accessing it by e.g. turning it into a DF which I do later here)
                                
                                #seed=TRUE)
                                
                                #Might want to consider the xreg argument to seas(). Using a helper function genhol() and passing in holiday names, you are able to seasonally adjust for specific holidays, e.g:
                                xreg = covid_ts #This marks the pandemic as an exogenous variable to consider, adding another component to the model to decompose. It is separate from the usual seasonal component.
                                      #> seas(iip,
                                      # + x11 = "",
                                      # + xreg = genhol(diwali, start = 0, end = 0, center = "calendar"),
                                      # + regression.usertype = "holiday"
                                      # + 
                                #forces yearly total of sa to match those of the orginal series 
                                #using the dention method. A regression benchmarking can also be used
                                  #force.type = "denton"
        )
        print(adjusted_cpv_ts)
        #get the Bayesian Information Criterion to judge the accuracy of the model for the data
        print(paste("The BIC is", BIC(adjusted_cpv_ts)))
        #out(adjusted_cpv_ts) - could give full output of x-13 ARIMA SEATS
        # Return the final adjusted series
        return(final(adjusted_cpv_ts))
      }, error = function(e) {
        # Print an error message and return original series
        cat("Error in processing time series: ", e$message, "\n")
        
        df_data <- as.data.frame(ts_data)
        #If there is an error, just take the original value as the output and mark the series as excluded
        names(df_data)[1] <- "seasonaladj" 
        
        #df_data should have the same number of rows as ts_vec at this stage so bind_cols should be OK
        df_data <- bind_cols(ts_vec, df_data) |> 
          mutate(partner = j_val_input,
                 ProductCode = i_val,
                 excluded_series = 1)
        
        return(df_data)
      })
    }# end adjust_seasonal function

#Define the latest period to pass to the arguments in the seasonal adjustment function 
latest_year <- max(year(ts_vec$date)) 
latest_month <- month(tail(ts_vec$date,1))
latest_quarter <- case_when(latest_month == 1 ~ 1,
                            latest_month == 4 ~ 2,
                            latest_month == 7 ~ 3,
                            latest_month == 10 ~ 4)

#use the seasonal adjustment function on the time series object, returns a time series object
adjusted_cpv_df <- adjust_seasonal(current_price_values_ts, 
                                   time_span = paste0(earliest_date_in_data ,", ", latest_year, ".", latest_quarter)
                  )

if(!(is.data.frame(adjusted_cpv_df))) { #If this is already a dataframe, then it is an excluded series, otherwise it will be a time series at the moment
    #convert the seas object back into a dataframe, so that the different series can be bound back together for input into the CVM code.
    adjusted_cpv_df <- as.data.frame(adjusted_cpv_df)  
      
    names(adjusted_cpv_df)[1] <- "seasonaladj" #When the ts object is turned back into a df, the value column just gets called 'x'.

#Store the values of partner and product code for this particular iteration
adjusted_cpv_df <- bind_cols(ts_vec, adjusted_cpv_df) %>%  #This step gets the date variable back.
  mutate(partner = j_val_input,
         ProductCode = i_val,
         excluded_series = 0)
  }#end if product of seas is DF


}#end if SD > 1
  
  #If the sd of the series is lt or = 1 then the seasonal adjustment function won't work, so just create an analogous dataset without seasonal adjustment for further processing
  if(sd(current_price_values_filt$indicator) <= 1) {
    
          adjusted_cpv_df <- current_price_values_filt %>% 
            rename(seasonaladj = indicator) #just put the original series as the seasonally adjusted value so it can be retained. This series will be marked as excluded from seasonal adjustment so we will know.
          
          #Just do this to get the date variable back, it is used for joining the series together later in the final output:  
          adjusted_cpv_df <- bind_cols(ts_vec, adjusted_cpv_df) %>%
          mutate(partner = j_val_input,
                 ProductCode = i_val,
                 excluded_series = 1) 
   }#end if SD < 1
 
  
  return(adjusted_cpv_df)
  } # End if statement to check for 0 value series
} #end for function
