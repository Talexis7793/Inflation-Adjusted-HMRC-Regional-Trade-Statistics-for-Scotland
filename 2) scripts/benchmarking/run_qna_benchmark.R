source("//s0177a/datashare/OCEA/SNAP/r_code/functions/benchmarking_denton_cholette_prop.R")

run_qna_benchmark <- function(
    byvar1,
    byvar2,
    ind_data,
    ind_var,
    ind_freq,
    ref_data,
    ref_var,
    ref_freq
)
{
  
  #initialise all_bmk_data to accumulate results from benchmarking function
  all_bmk_data <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("value_bmk", "year", "subyear", "bylevel1", "bylevel2"))
  
  #make a copy of the input indicator data using generalised 5 column names. Must be in shape year, quarter/month, byvar1, byvar2, indicator                           
  #add if_else depending on ind_freq to work with either annual quarterly or monthly
   
    if({{ind_freq}} == 4)
      {xind_data <- ind_data %>%
        select(year, quarter, {{byvar1}}, {{byvar2}}, {{ind_var}})} 
    else if ({{ind_freq}} == 12)
      {xind_data <- ind_data %>%
        select(year, month, {{byvar1}}, {{byvar2}}, {{ind_var}})}
    else
    {print("indicator frequency not quarterly or monthly")}
    
    #select(year, quarter, {{byvar1}}, {{byvar2}}, {{ind_var}})
  
  indnames <- colnames(xind_data)
  
  colnames(xind_data) <- c("year","subyear","bylevel1","bylevel2","indicator")
  
  #make a copy of the input reference data using generalised 5 column names. Must be in shape year, quarter/month, byvar1, byvar2, indicator                           
  #add if_else depending on ind_freq to work with either annual quarterly or monthly
  xref_data <- ref_data %>% 
    select(year, {{byvar1}}, {{byvar2}}, {{ref_var}})
  
  refnames <- colnames(xref_data)
  
  colnames(xref_data) <- c("year","bylevel1","bylevel2","reference")
  
  
  start_year_ref <- ref_data %>% 
    filter (year == min(year)) %>% 
    distinct(year) %>% 
    select (year)
  
  startyr <- pull(start_year_ref[1,1])
  
  # create a loop for all distinct values of byvar1 for benchmarking
  
  list_byvar1 <- ind_data %>%
    filter({{byvar1}} != "NA" & {{ind_var}} != "NA" & {{ind_var}} > 0) %>% 
    distinct({{byvar1}})
  
  for (j in 1:nrow(list_byvar1))
  {
    
    # create a loop for all distinct values of byvar2 for benchmarking
    
    j_val <- pull(list_byvar1[j,1])
    
    print(j_val)
    
    list_byvar2 <- ind_data %>%
      filter({{byvar1}} == j_val & year == startyr & {{ind_var}} != "NA" & {{ind_var}} > 0) %>% 
      distinct({{byvar2}})
    
    for (i in 1:nrow(list_byvar2))
    {
      
      i_val <- pull(list_byvar2[i,1])
      
      print(i_val)
      
      
      temp_bmk_data <- 
        benchmark_denton_cholette_prop (bylevel1,
                                        j_val,         #byval1
                                        bylevel2,
                                        i_val,         #byval2   
                                        startyr,
                                        reference,
                                        xref_data,
                                        {{ref_freq}},
                                        indicator,
                                        xind_data,
                                        {{ind_freq}}
        ) 

      #bind the output from the function onto the accumulated results dataset
      all_bmk_data <- bind_rows(all_bmk_data,temp_bmk_data)

      
    } #end for byvar2 loop
} #end for byvar1 loop
  
  #merge benchmarked data back into OCEA long thin dataframe
  bmk_merge <- left_join(xind_data,all_bmk_data,by=c("bylevel1","bylevel2","year","subyear"))  

  #prep_output <- bmk_merge %>% 
  #  arrange(bmk_merge, bylevel1, bylevel2, year, subyear)
  
  bmk_output <- bmk_merge
  
  colnames(bmk_output) <- c(indnames,"bmk_value")
  
  return(bmk_output)
}