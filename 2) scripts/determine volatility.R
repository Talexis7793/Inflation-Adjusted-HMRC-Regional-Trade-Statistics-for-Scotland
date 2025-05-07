library(rugarch)
options(scipen=999)

#Note this script currently only works on the UK & Scotland breakdowns (not for all UK). Will just need to change input files to get it to work for all UK

#Define input file:
if(collapse_to_eu == TRUE) { 
  input_file <- paste0("1) data/raw/RTS UK and Scotland ",data_date,".csv")
}

if(collapse_to_eu == FALSE) {
  
  if(country_product_breakdown == FALSE) {
    input_file <- paste0("1) data/raw/RTS UK and Scotland - dest country breakdown - ",data_date,".csv")
  }
  
  if(country_product_breakdown == TRUE) {
    input_file <- paste0("1) data/raw/RTS UK and Scotland - dest country & product breakdown - ",data_date,".csv")
  }
}

test_volatility <- read_csv(input_file) %>% 
  mutate(group = paste(country, flowtype, partner, Sitc2Desc, sep = " - ")) %>% 
  filter(country == "Scotland")

#sanitise group names
  test_volatility <- test_volatility %>%
    mutate(group = sub("/", "", test_volatility$group))

#sd - but how large a variation to look for?

#differences - if large could be volatile - could use the diff_sa_netmass and diff_sa_value fields to check this, but
#how large a difference to look for? Maybe try some other methods first
#also use ACF and PACF charts?

list_groups_value <- test_volatility %>%
  select(group) %>% 
  distinct() %>% 
  mutate(test_var = "value")

list_groups_netmass <- test_volatility %>%
  select(group) %>% 
  distinct() %>% 
  mutate(test_var = "netmass")

list_groups <- bind_rows(list_groups_value, list_groups_netmass)

#pivot the data longer
test_volatility <- test_volatility %>% 
  pivot_longer(cols = c(value, netmass), names_to = "value_type", values_to = "trade_value")

##############
#Define function
#Could just check overall country level
check_volatility <- function(group_name,
                             test_var,
                             test_type 
) {
  
  group_name <- group_name %>% pull(group)
  test_var <- test_var %>% pull(test_var)
  
  #filter to this specific partner, and exclude known erratic products
  test_data <- test_volatility %>% 
    filter(group == group_name &
             value_type == test_var &
             ProductCode %in% all_excl_erratic_sitc3) %>% 
    mutate(month = month(date),
           year = year(date),
           quarter = case_when(month == 1 ~ 1,
                               month == 4 ~ 2,
                               month == 7 ~ 3,
                               month == 10 ~ 4))
  
  if(nrow(test_data) == 0) {
    #Skip this iteration if no trade
    return(NULL)
  }
  
  #min_year <- filter(test_data, year == min(test_data$year))
  
  if(test_type == "acf") {
  
      final_test <- test_data %>%
        select(trade_value) %>% 
        mutate(test_var_sq = trade_value^2) %>% 
        select(test_var_sq)
      #%>% 
        # ts(start = c(min(year(test_data$date)), min(min_year$quarter)), 
        #    frequency = 4)
      
      #define confidence interval
      ci <- 1/sqrt(nrow(final_test))
      
      #plot ACF
      acf_ob <- acf(final_test)
      
      #from the ACF plot object, get the ACF results and check if any lags exceed the CI
      sig_lags <- data.frame(acf_ob$acf) %>% 
        mutate(sig = case_when(abs(acf_ob.acf) > ci ~ 1, 
                               .default = 0))
      
      #if any lags are significant, it may be an indicator that variance is not homoskedastistic, so mark the whole series as potentially volatile
      #note the ACF at lag 0 will always be significant because every data point is perfectly correlated with itself, so remove the first row
      sig_lags <- sig_lags[-1, ] 
      
      results <- sum(sig_lags$sig)
  
  } #end ACF
  
  if(test_type == "difference") {
    
      final_test <- test_data %>%
        select(trade_value) 
      
      #calculate first differences
      diff <- final_test %>% 
        mutate(lag = lag(trade_value), 
               diff = abs(trade_value - lag))
      
      #define threshold
      threshold <- 100000
      
      #from the ACF plot object, get the ACF results and check if any lags exceed the CI
      diff <- diff %>% 
        mutate(sig = case_when(diff > threshold ~ 1, 
                               .default = 0))
      
      results <- sum(diff$sig)
  }#end difference
  
  if(test_type == "cv") {
    
    #calculate cv
    test_data <- test_data %>% 
      mutate(cv = sd(trade_value) / mean(trade_value))
    
    #define threshold for CV
    threshold <- 1
    #define threshold for SD (we only really care about marking series as volatile if the swings are large enough
    #to cause an issue)
    threshold_sd <- 10000
    
    if(unique(test_data$cv) > threshold & sd(test_data$trade_value) > threshold_sd) {
      
      results <- 1
      
      #plot the series
      plot <- ggplot(test_data, aes(x = date, y = trade_value, group = value_type, colour = value_type)) +
        geom_line() +
        labs(title = "potentially volatile series") +
        theme_minimal()
      
      #save the plot
      save_location = "./QA/check volatility/large cv"
      ggsave(filename = paste(save_location, "/", unique(test_data$group), "volatility QA.png"), plot = last_plot(), width = 7, height = 5, dpi = 300)
      
    } else {
      results <- -1
      
      #plot the series
      plot <- ggplot(test_data, aes(x = date, y = trade_value, group = value_type, colour = value_type)) +
        geom_line() +
        labs(title = "likely non-volatile series") +
        theme_minimal()
      
      #save the plot
      save_location = "./QA/check volatility/smaller cv"
      ggsave(filename = paste(save_location, "/", unique(test_data$group), "volatility QA.png"), plot = last_plot(), width = 7, height = 5, dpi = 300)
      
    }
    
    
  }#end cv
  
  #Might need to figure out what to return here - prob need a DF for each group
  if(results > 0) {
    
        test_data <- test_data %>% 
          mutate(volatile = "potentially volatile") %>% 
          select(-month, -year, -quarter)

  }
  
  if(results <= 0) {
    
    test_data <- test_data %>% 
      mutate(volatile = "not likely to be volatile") %>% 
      select(-month, -year, -quarter)
    
  }
  
  return(test_data)
}
######################################################

#first remove all graphs in the directories from the previous run:
# Define the directory path
dir_path <- "./QA/check volatility/smaller cv"  
# List all files in the directory
files <- list.files(path = dir_path, full.names = TRUE)
# Remove all files
unlink(files)
# Define the directory path
dir_path <- "./QA/check volatility/large cv"  
# List all files in the directory
files <- list.files(path = dir_path, full.names = TRUE)
# Remove all files
unlink(files)


start_time <- Sys.time()

check_volat <- purrr::map_df(1:nrow(list_groups),
                                    \(x) check_volatility(group_name = list_groups[x, 1],
                                                          test_var = list_groups[x, 2],
                                                          test_type = "cv") # specify "difference" or "acf" or "cv"
                                     )

end_time <- Sys.time()

run_time_volat <- end_time - start_time

#produce a summary table to show volatile series
check_volat_summary <- check_volat %>% 
  group_by(group, value_type, cv) %>% 
  distinct(volatile) %>% 
  ungroup()

sum_volat <- nrow(filter(check_volat_summary, volatile == "potentially volatile"))
sum_non_volat <- nrow(filter(check_volat_summary, volatile == "not likely to be volatile"))
perc_vol = (sum_volat / nrow(check_volat_summary))*100


###################################################################################################################################
###################################################################################################################################

# #Initialise some empty lists that will be filled with values by the loop   
# plot_list <- list()
# problem_value_series_list <- list()
# problem_netmass_series_list <- list()
# 
# #Prepare data for visualisation
#   data_for_vis <- test_volatility %>% 
#     select(-diff_sa_netmass, -diff_sa_value, -threshold_value, -threshold_netmass, excluded_series_value, -excluded_series_volume) #excluded series vars are for the seasonal adjustment
# 

#   
# ####################################################################################################################################################
#   #create line charts for each flow
#   create_line_charts_to_check_volatility <- function(input_data, save_location) {
#     
#     for (each_group in unique(input_data$group)) {
#       
#       group_data <- subset(input_data, 
#                            group == each_group) 
#       
#       #Calculate some stats on the series
#       cv_value <- sd(group_data$trade_value[group_data$value_type == "value"]) / mean(group_data$trade_value[group_data$value_type == "value"])
#       cv_netmass <- sd(group_data$trade_value[group_data$value_type == "netmass"]) / mean(group_data$trade_value[group_data$value_type == "netmass"])
#       
#       
#       print(paste(
#         "CV value:", cv_value,
#         "CV netmass:", cv_netmass
#       ))
#       
#       
#       #Create plots of original by SA value over time
#       plot_list[[each_group]] <- ggplot(group_data, aes(x = date, y = trade_value, group = value_type, colour = value_type)) +
#         geom_line() +
#         labs(title = "original and S.A. trade in netmass and value") +
#         theme_minimal()
#       
#       save_location_2 <- save_location
#       
#       #If both series are blank move to next iteration
#       if((is.na(cv_value)) & (is.na(cv_netmass))) {
#         next
#       }
#       
#       if(cv_value > 1 | cv_netmass > 1){
#         #If there is a largish CV, save the graph in the appropriate folder and add the series name to a list of problematic series, so we can use the unadjusted series in these cases.
#         save_location_2 <- paste0(save_location, "/large cv")
#         
#         if(cv_value > 1){
#           problem_value_series_list <- append(problem_value_series_list, each_group)
#       
#         }
#         
#         if(cv_netmass > 1){
#           problem_netmass_series_list <- append(problem_netmass_series_list, each_group)
#          
#         }
#         
#       }
#       
#       if(cv_value <= 1 & cv_netmass <= 1){
#         save_location_2 <- paste0(save_location, "/smaller cv")
#       }
#       
#       ggsave(filename = paste(save_location_2, "/", each_group, "seasonal adjustment QA.png"), plot = last_plot(), width = 7, height = 5, dpi = 300)
#       
#       
#     } #end for loop
#     
#   }#end function 
#   
# 
#   
# #check plots of series - need to get this working in this context, just haven't prepped input data right
# create_line_charts_to_check_volatility(input_data = data_for_vis, save_location = "./QA/check volatility")
# 



#Create a residual plot for the difference between SA model and original value? (may have already done this, and it may be overcomplicating the volatility check)


#}


#ARCH models - test for changing variance over time - could not get packages for this.
#library(aTSA)
#arch_test <- arch.test(data, lag = 4)  # You can adjust the lag based on your data

#for now I am looking for a quick and dirty method to find volatile series, but there may be more sophisticated methods, for this:
#Learn about GARCH models here: https://campus.datacamp.com/courses/garch-models-in-r/the-standard-garch-model-as-the-workhorse-model?ex=9
    # Specify a GARCH(1,1) model
    # garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    #                          mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
    #                          distribution.model = "std")
    # 
    # # Fit the model to the returns data
    # garch_fit <- ugarchfit(spec = garch_spec, data = stock_returns)
    # 
    # # Show the summary of the fitted model
    # summary(garch_fit)
    # 
    # # Plot the conditional volatility
    # plot(garch_fit, which = 4)
    
    # Look at the volatility coefficients (α and β) and their p-values. If α (the ARCH term) is significant,
    # this suggests that volatility clustering is present in your data.

#Z-Score: A value greater than 2 or less than -2 often indicates an outlier.

#Augmented Dickey-Fuller (ADF) test to check if the series has a unit root (indicating non-stationarity).
#Non-stationary data means the statistical properties of the data change over time.
library(tseries)
adf.test(data)
