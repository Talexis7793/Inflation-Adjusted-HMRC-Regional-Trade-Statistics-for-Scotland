#Run this program after running 2b) seasonal adjustment

#set start time
start_time <- Sys.time()

#This reads in the product of the seasonal adjustment program - make sure you are running this program at the right time.
current_price_sa_output <- read_csv(output_file)

#Create a grouping variable
current_price_sa_output <- current_price_sa_output %>% 
  mutate(group = paste(country, flowtype, partner, gsub(pattern = "/", replacement = "", Sitc2Desc), sep = " - "),
         threshold_value = NA,
         threshold_netmass = NA)
  
#########################################################
#First find the threshold for identifying potentially problematic series (in addition to the absolute_threshold). 
  #this will be 1.5*IQR of that series before seasonal adjustment. If 1.5*IQR is smaller than the difference between the original value and the seasonally adjusted value
  #in any time point of the series then this series may be problematic. 
########################################################
for (each_group in unique(current_price_sa_output$group)) {
  
  Q1_value <- quantile(current_price_sa_output$value[current_price_sa_output$group == each_group], 0.25)
  Q3_value <- quantile(current_price_sa_output$value[current_price_sa_output$group == each_group], 0.75)
  IQR_value <- Q3_value - Q1_value
  
  Q1_netmass <- quantile(current_price_sa_output$netmass[current_price_sa_output$group == each_group], 0.25)
  Q3_netmass <- quantile(current_price_sa_output$netmass[current_price_sa_output$group == each_group], 0.75)
  IQR_netmass <- Q3_netmass - Q1_netmass
  
  current_price_sa_output <- current_price_sa_output %>% 
    mutate(threshold_value = case_when(group == each_group ~ IQR_value*1.5, #For the current group, fill in the threshold with the IQR*1.5
                                 .default = threshold_value)) %>%  #Otherwise leave it, this iteration is not this group's turn
    mutate(threshold_netmass = case_when(group == each_group ~ IQR_netmass*1.5, #For the current group, fill in the threshold with the IQR*1.5
                                       .default = threshold_netmass)) #Otherwise leave it, this iteration is not this group's turn
  }

#Filter the output so we are only checking results where the seasonal adjustment has made a big difference 
  #First get a list of the series we need to check using the group variable
results_to_check <- current_price_sa_output %>% 
  filter((abs(diff_sa_netmass) > threshold_netmass | abs(diff_sa_value) > threshold_value) & 
        (abs(diff_sa_netmass) > absolute_threshold | abs(diff_sa_value) > absolute_threshold)) %>% 
  distinct(group)

  #Then pull out these whole series from the whole dataset
results_to_check <- current_price_sa_output %>% 
  inner_join(results_to_check, by = join_by(group)) %>% 
  select(-diff_sa_netmass, -diff_sa_value, -excluded_series_value, -excluded_series_volume, -country, -flowtype, -partner, -ProductCode, -Sitc2Desc)

#pivot longer to make line charts appropriate
results_to_check <- results_to_check %>% 
  pivot_longer(cols = c(value, value_sa, netmass, netmass_sa), names_to = "value_type", values_to = "trade_value")

######################################################################################
all_results <- current_price_sa_output %>% 
  distinct(group)

#Then pull out these whole series from the whole dataset
all_results <- current_price_sa_output %>% 
  inner_join(all_results, by = join_by(group)) %>% 
  select(-diff_sa_netmass, -diff_sa_value, -excluded_series_value, -excluded_series_volume, -country, -flowtype, -partner, -ProductCode, -Sitc2Desc)

#pivot longer to make line charts appropriate
all_results <- all_results %>% 
  pivot_longer(cols = c(value, value_sa, netmass, netmass_sa), names_to = "value_type", values_to = "trade_value") 

#Initialise some empty lists that will be filled with values by the loop   
plot_list <- list()
problem_value_series_list <- list()
problem_netmass_series_list <- list()

#create line charts for each flow
create_line_charts_for_qa <- function(input_data, save_location) {
          
        for (each_group in unique(input_data$group)) {
          
          group_data <- subset(input_data, 
                               group == each_group) 
          
          #create a plot for each group
          # plot_list[[each_group]] <- ggplot(group_data, aes(x = date, y = diff_sa_value)) +
          #   geom_line() +
          #   labs(title = "differences between original and S.A. value") +
          #   theme_minimal()
          # 
          # ggsave(filename = paste("./QA/", each_group, "seasonal adjustment QA.png"), plot = last_plot(), width = 7, height = 5, dpi = 300)
        
          #Calculate some stats on the series
          cv_value <- sd(group_data$trade_value[group_data$value_type == "value"]) / mean(group_data$trade_value[group_data$value_type == "value"])
          cv_netmass <- sd(group_data$trade_value[group_data$value_type == "netmass"]) / mean(group_data$trade_value[group_data$value_type == "netmass"])
        
          
          print(paste(#each_group, 
                      #": Sum value - ", sum(group_data$trade_value[group_data$value_type == "value"]), " Sum netmass - ",
                    #sum(group_data$trade_value[group_data$value_type == "netmass"]), ". SD value - ", sd(group_data$trade_value[group_data$value_type == "value"]), 
                        #"SD netmass - ", sd(group_data$trade_value[group_data$value_type == "netmass"]),
                    "CV value:", cv_value,
                    "CV netmass:", cv_netmass
                    ))
          
          
          #Create plots of original by SA value over time
          plot_list[[each_group]] <- ggplot(group_data, aes(x = date, y = trade_value, group = value_type, colour = value_type)) +
            geom_line() +
            labs(title = "original and S.A. trade in netmass and value") +
            theme_minimal()
          
          save_location_2 <- save_location
          
          #If both series are blank move to next iteration
          if((is.na(cv_value)) & (is.na(cv_netmass))) {
            next
          }
          
          if(cv_value > 1 | cv_netmass > 1){
            #If there is a largish CV, save the graph in the appropriate folder and add the series name to a list of problematic series, so we can use the unadjusted series in these cases.
            save_location_2 <- paste0(save_location, "/large cv")
            
              if(cv_value > 1){
              problem_value_series_list <- append(problem_value_series_list, each_group)
              
              #Use the unadjusted value series if the value series is the one with the problem
              current_price_sa_output <- current_price_sa_output %>% 
                mutate(value_sa = case_when(group == each_group ~ value, #for this group (the one with a difference exceeding threshold, where the original series also has a high CV) just use the original series as the SA series
                                            .default = value_sa), # leave all other rows for value_sa as they are
                       excluded_series_value = case_when(group == each_group ~ 1, #mark this series as excluded from SA
                                                         .default = excluded_series_value))
              }
            
              if(cv_netmass > 1){
                problem_netmass_series_list <- append(problem_netmass_series_list, each_group)
                
                #Use the unadjusted netmass series if the netmass series is the one with the problem
                current_price_sa_output <- current_price_sa_output %>% 
                  mutate(netmass_sa = case_when(group == each_group ~ netmass, #
                                              .default = netmass_sa),
                         excluded_series_volume = case_when(group == each_group ~ 1, #
                                                           .default = excluded_series_volume))
              }
            
            }
  
  #If the difference between SA and unadjusted series is large but the CV of the series is smaller than 1, there generally does not seem to be too much of a problem. 
  #Output the graphs to the appropriate folder for visual inspection though. These graphs will likely look quite erratic, but the seasonal adjustment will probably have 
  #done a reasonable job despite this. This is probably because there is enough trade overall for the model to function reasonably despite volatility (hence the CV is smaller
  #as the larger size of the mean brings it down). Whether these series are OK though will still be a judgement call. 
  
                if(cv_value <= 1 & cv_netmass <= 1){
                  save_location_2 <- paste0(save_location, "/smaller cv")
                }
                        
                ggsave(filename = paste(save_location_2, "/", each_group, "seasonal adjustment QA.png"), plot = last_plot(), width = 7, height = 5, dpi = 300)

  
    } #end for loop
  #Assign output to global env
  current_price_sa_output <<- current_price_sa_output
  problem_netmass_series_list <<- problem_netmass_series_list
  problem_value_series_list <<- problem_value_series_list
  
}#end function  

create_line_charts_for_qa(input_data = results_to_check, save_location = "./QA/series with large difference")

#finally if the series is a known erratic, just use the original series:
current_price_sa_output <- current_price_sa_output %>% 
  mutate(value_sa = case_when(ProductCode %in% c(erratic, sitc3) ~ value, #if the product is erratic, make the SA series the same as the original 
                              .default = value_sa), #otherwise leave it alone
         netmass_sa = case_when(ProductCode %in% c(erratic, sitc3) ~ netmass, 
                              .default = netmass_sa))

#using regex just to remove some characters from the string so we can alter the file name appropriately
if(write_to_csv == TRUE){write_csv(current_price_sa_output, paste0("./QA/", sub("^.*?/", "", sub("\\..*", "", output_file)), " post QA of SA.csv"))}

print("The following value series could be problematic: ") 
problem_value_series_list
print("The following netmass series could be problematic: ") 
problem_netmass_series_list

#calculate run time
end_time <- Sys.time()
run_time_qa <- end_time - start_time
