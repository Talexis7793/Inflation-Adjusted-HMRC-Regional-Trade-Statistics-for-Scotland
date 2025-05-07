
####### PRODUCE TABLES ######
library(tidyverse)
library(readxl)
library(openxlsx) # for writing to excel
#### Year up to Quarter x comparisons ####



#Enter the name of the series you want to appear in the key stats tables. Usually this will be "Total excl. sitc3 & erratic", but might also be "Total excl. sitc3, erratic & drink" if we want to know how trade is performing without e.g. drink
total_interested_in <- "Total excl. sitc3 & erratic"

#User input needed:
use_sa <- TRUE
all_uk <- FALSE
file_date <- "2024-12-10"  #Enter the date the file was generated
data_date = "2024Q3"
quarter_want = "Q3"
comparison_year <- 2018 #year to compare the current year to (in addition to the previous one)
current_year <- 2024 #e.g. if the year we are interested in is the year ending Q1 2024 then put 2024 here.
previous_year <- current_year - 1

#These two are automatically updated:
diff_prev_year <- paste0("Change", current_year, "vs", previous_year)
diff_comp_year <- paste0("Change", current_year, "vs", comparison_year)

#Reindex to year - this will make all CVM values relative to this base year. Set to FALSE if you do not want to reindex
reindex_year <- 2018
cvm_index_to <- "value" #Is the CVM field (in the input data for this program) indexed to the current price value or 100? Type either "value" or "100".

#Options for the SITC 2 breakdown table: 
world_area <- "World" # Enter "World", "EU" or "Non EU"
#Note for further development - show both the below options in the same output file:
#Note the 'value' option below will produce a table showing the changes in CVM values between years in sbsolute terms. This is
#a useful rough indicator for detecting the most impactful changes (i.e. a 3% change in a category that brings in a very large value in
#current prices might be more important than a 12% change in something with a lower value in current prices). However the CVM is 
#still an index based on the current price value and should be used with caution. If calculating a % of total change accounted for by each
#product for example, these should be used as rough guides for what are the most important product categories only.  

# Name output files (automatically updated)
output_file_changes = paste0("3) outputs/table_", world_area, "_cvm_changes_", data_date)
output_file_key_stats = paste0("3) outputs/table_key_stats_", data_date)

################################################################################################################################################

  #### Rounding function to make R do it properly ####
  round2 = function(x, digits) {
    posneg = sign(x)
    z = abs(x)*10^digits
    z = z + 0.5 + sqrt(.Machine$double.eps)
    z = trunc(z)
    z = z/10^digits
    z*posneg
  }
            
#Function to call on awkwardly named variables, defining this here to make the code more readable

call_year_variable <- function(variable_name) {
            return_me <- eval(parse(text = paste0("`", variable_name, "`")))
return(return_me)
            }

#automatically updated:
  quarters_to_advance <- case_when(
    quarter_want == "Q1" ~ list(2, 3, 4),
    
    quarter_want == "Q2" ~ list(3, 4, NA),
    
    quarter_want == "Q3" ~ list(4, NA, NA),
    
    quarter_want == "Q4" ~ list(NA, NA, NA)
  )

  
            #Read in data           
            if(use_sa == FALSE) {
              
              if(all_uk == FALSE){
                cvm_series <- read_csv(paste0("3) outputs/CVM - UK & Scotland ",data_date, " (Generated ", file_date,").csv"))
                cvm_series_sitc2 <- read_csv(paste0("3) outputs/CVM SITC2 - UK & Scotland ",data_date, " (Generated ", file_date,").csv"))
              }
              
              if(all_uk == TRUE) {
                cvm_series <- read_csv(paste0("3) outputs/CVM - all UK ",data_date, " (Generated ", file_date,").csv"))
                cvm_series_sitc2 <- read_csv(paste0("3) outputs/CVM SITC2 - all UK ",data_date, " (Generated ", file_date,").csv"))
              }
            }
            
            #Here we use the seasonally adjusted values, and simply rename them to be compatible with the rest of the code 
            if(use_sa == TRUE) {
              
              if(all_uk == FALSE){
                cvm_series <- read_csv(paste0("3) outputs/CVM - SA - UK & Scotland ",data_date, " (Generated ", file_date,").csv"))
                cvm_series_sitc2 <- read_csv(paste0("3) outputs/CVM SITC2 - SA - UK & Scotland ",data_date, " (Generated ", file_date,").csv"))
              }
              
              if(all_uk == TRUE){
                cvm_series <- read_csv(paste0("3) outputs/CVM - SA - all UK ",data_date, " (Generated ", file_date,").csv"))
                cvm_series_sitc2 <- read_csv(paste0("3) outputs/CVM SITC2 - SA - all UK ",data_date, " (Generated ", file_date,").csv"))
              }
            }
            

                
                options(scipen=999)
#Read in user friendly names to make tables interpretable
                sitc_names <- read_excel("./1) data/HMRC - Look up table for user-friendly SITC names.xlsx",
                                         sheet = "SITC 2 LUT",
                                         col_names = c("SITC_Division", "sitc_simple", "SITC_Division2")) %>% 
                  select(SITC_Division, sitc_simple) %>% 
                  mutate(SITC_short_code = str_sub(SITC_Division, 1, 2))

################################################################################################################
                #Reindex
if(reindex_year != FALSE) {                
    cvm_series_reindexed <- cvm_series  %>%
                  select(-value_base) %>% 
      left_join(cvm_series %>%
                  filter(ye_currq==reindex_year) %>% #1.8.24 indexing to ye_currq to make consistent with tables (the CVM value in the base year should match the CP value in that period)
                  select(-date, -PYP, -CYPlink, -lagyear, -CYPlink.lag, -netmass, -value_base, -value_index, -implied_deflator) %>%
                  group_by(country, agg, flowtype, partner) %>%
                    summarise(value_base = sum(value), 
                                cvm_base = mean(CVM)) %>% #value_base is the mean value in the base year  
                  select(country, agg, flowtype, partner, cvm_base, value_base) %>%
                  unique(),
                by = c("agg" = "agg", "country" = "country", "flowtype" = "flowtype", "partner" = "partner")) %>%
      mutate(CVM =  value_base*(CVM/cvm_base)) %>% #Overwrite CVM variable with the reindexed version. CVM/cvm_base gives the index number, then multiplying by the average current price value in the reindex year makes it relative to the current price value of each flow
      ungroup() %>%
      select(date, year, country, flowtype, agg, partner, CVM, implied_deflator, value)

    cvm_series <- cvm_series_reindexed
    
    #Sitc2
    cvm_series_sitc2_reindexed <- cvm_series_sitc2  %>%
      select(-value_base) %>% 
      left_join(cvm_series_sitc2 %>%
                  filter(ye_currq==reindex_year) %>%
                  select(-date, -PYP, -CYPlink, -lagyear, -CYPlink.lag, -netmass, -value_base, -value_index, -implied_deflator) %>%
                  group_by(country, agg, flowtype, partner) %>%
                  summarise(value_base = sum(value),
                            cvm_base = mean(CVM)) %>% #value_base is the mean value in the base year
                  select(country, agg, flowtype, partner, cvm_base, value_base) %>%
                  unique(),
                by = c("agg" = "agg", "country" = "country", "flowtype" = "flowtype", "partner" = "partner")) %>%
      mutate(CVM =  value_base*(CVM/cvm_base)) %>% #Overwrite CVM variable with the reindexed version. CVM/cvm_base gives the index number, then multiplying by the average current price value in the reindex year makes it relative to the current price value of each flow
      ungroup() %>%
      select(date, year, country, flowtype, agg, partner, CVM, implied_deflator, value)
    
    cvm_series_sitc2 <- cvm_series_sitc2_reindexed
}
#########################################################################################################
###Key stats tables               
 # Create tables
              table_raw_df <- cvm_series %>% 
                rename("IDEF" = "implied_deflator") %>%
                mutate(date = as.Date(date,"%m/%d/%Y"),
                       month = as.numeric(str_sub(date, 6,7)),
                       quarter = ceiling(month/3))
              
              #Add year ending in current quarter variable
              table_raw_df$YE_currq <- ifelse(table_raw_df$quarter %in% quarters_to_advance, table_raw_df$year + 1, table_raw_df$year)
              
              table_years_df <- table_raw_df %>%
                filter(YE_currq >= comparison_year) %>%
                select(date, country, YE_currq, partner, flowtype, agg, CVM, value, IDEF)

 # Table with annual changes for CVM, Value and IDEF terms
table_changes <- table_years_df %>% 
  ungroup() %>%
  reshape2::melt(id.vars = c("date", "YE_currq", "country", "partner", "flowtype", "agg")) %>% # puts CVM, Value (£) and IDEF into separate rows, melt is a bit like pivot_longer, in the resulting dataframe there will be rows for each unique combination of id.vars, all other cols will be pivoted to long format
  group_by(YE_currq, country, partner, flowtype, agg, variable) %>%
  summarise(value = mean(value)) %>% #value here is each of CVM, Values and IDEF
  reshape2::dcast(flowtype + agg + partner + country + variable ~ YE_currq, value.var="value") %>% #put the molten dataframe back into original (wide) format.
  mutate("{diff_comp_year}" := signif((eval(parse(text = paste0("`", current_year, "`"))) / eval(parse(text = paste0("`", comparison_year, "`")))  -1), 4), 
         "{diff_prev_year}" := signif((eval(parse(text = paste0("`", current_year, "`"))) / eval(parse(text = paste0("`", previous_year, "`")))  -1), 4)) %>%
  select(-(starts_with("2"))) #remove the year variables starting with 2, will stop working in the year 3000

# Key Stats Table
table_key_stats_comparison_year <- table_changes %>% 
  filter(agg == total_interested_in) %>%
  mutate(country_var = paste(variable, country, sep = " ")) %>%
  select(-c(agg, all_of(diff_prev_year), variable, country)) %>%
  spread(c(country_var), diff_comp_year) 

table_key_stats_previous_year <- table_changes %>% 
  filter(agg == total_interested_in) %>%
  mutate(country_var = paste(variable, country, sep = " ")) %>%
  select(-c(agg, all_of(diff_comp_year), variable, country)) %>%
  spread(c(country_var), diff_prev_year) 

#Write to clipboard, run one at a time, pasting the table into excel in between
write.xlsx(table_key_stats_comparison_year, paste0(output_file_key_stats, "- ", current_year, "-", comparison_year, ".xlsx"))
write.xlsx(table_key_stats_previous_year, paste0(output_file_key_stats, "- ", current_year, "-", previous_year, ".xlsx"))
                      
                      # SITC 1 stats table - not really used
                      
                      # table_sitc1_2023_v_2018 <- table_changes %>%
                      #   filter(variable == "CVM",
                      #          partner == "World",
                      #          flowtype != "Trade") %>%
                      #   mutate(country_flow = paste(flowtype, country, sep = " ")) %>%
                      #   select(-c( Change2023vs2022, country, flowtype)) %>%
                      #   spread(c(country_flow), Change2023vs2018)
                      # 
                      # table_sitc1_2023_v_2022 <- table_changes %>%
                      #   filter(variable == "CVM",
                      #          partner == "World",
                      #          flowtype != "Trade") %>%
                      #   mutate(country_flow = paste(flowtype, country, sep = " ")) %>%
                      #   select(-c( Change2023vs2018, country, flowtype)) %>%
                      #   spread(c(country_flow), Change2023vs2022)
                      # 
                      # clipr::write_clip(table_sitc1_2023_v_2018)
                      # clipr::write_clip(table_sitc1_2023_v_2022)

##############Now create a table by SITC2:
#Filter the data for the specific series wanted

  cvm_series_sitc2_filt <- cvm_series_sitc2 %>% 
  filter(partner == world_area) %>% 
  ungroup() |> 
  select(-partner)

table_raw_df_sitc2 <- cvm_series_sitc2_filt %>% 
  rename("IDEF" = "implied_deflator") %>%
  mutate(date = as.Date(date,"%m/%d/%Y"),
         month = as.numeric(str_sub(date, 6,7)),
         quarter = ceiling(month/3))

#Define the year ending in the current quarter variable
table_raw_df_sitc2$YE_currq <- ifelse(table_raw_df_sitc2$quarter %in% quarters_to_advance, table_raw_df_sitc2$year + 1, table_raw_df_sitc2$year)

table_years_df_sitc2 <- table_raw_df_sitc2 %>%
  filter(YE_currq >= comparison_year) %>%
  mutate(ye_year = case_when(YE_currq %in% current_year ~ "Current",
                             YE_currq %in% comparison_year ~ "Comparison",
                             YE_currq %in% previous_year ~ "Previous",
                             TRUE ~ "Exclude")) %>%
  filter(ye_year != "Exclude") %>%
  select(date, country, ye_year, flowtype, agg, CVM, value, IDEF)

# Table with annual changes for CVM, Value and IDEF terms
table_changes_sitc2 <- table_years_df_sitc2 %>% 
  ungroup() %>%
  reshape2::melt(id.vars = c("date", "ye_year", "country", "flowtype", "agg")) %>% # puts CVM Values and IDEF into separate rows
  group_by(ye_year, country, flowtype, agg, variable) %>%
  summarise(value = mean(value)) %>% #value here is a variable containing CVM, Values (£) and IDEF
  reshape2::dcast(flowtype + agg + country + variable ~ ye_year, value.var="value") %>%
  mutate(ChangevsComparisonYear = signif((`Current` / `Comparison`  -1), 4),
         ChangevsPreviousYear = signif((`Current` / `Previous`  -1), 4),
         diff_v_comp_year = `Current`-`Comparison`,
         diff_v_prev_year = `Current`-`Previous`) %>% 
  select(-c(`Comparison`:`Previous`)) %>%
  left_join(sitc_names, by = join_by(agg == SITC_short_code)) %>%
  mutate(agg = case_when(!is.na(sitc_simple) ~ sitc_simple,
                         TRUE ~ agg)) %>%
  rename(sitc_division = agg) %>%
  select(-sitc_simple, -SITC_Division) |> 
  filter(variable == "CVM") # remove value and IDEF for now, can maybe look at using this another time

#Filter out just the Scotland results, this makes the files easier to work with in excel
table_changes_sitc2_scotland_exp <- table_changes_sitc2 |> 
  filter(flowtype == "Exports",
         country == "Scotland")

table_changes_sitc2_scotland_imp <- table_changes_sitc2 |> 
  filter(flowtype == "Imports",
         country == "Scotland")

#write.xlsx(table_changes_sitc2, paste0(output_file_changes, " SITC 2- changes between years.xlsx"))
write.xlsx(table_changes_sitc2_scotland_exp, paste0(output_file_changes, " SITC 2- changes between years - Scotland exports only.xlsx"))
write.xlsx(table_changes_sitc2_scotland_imp, paste0(output_file_changes, " SITC 2- changes between years - Scotland imports only.xlsx"))

#if(percentage_or_value == "value") {
      
#       table_sitc2_current_v_comp_val <- table_changes_sitc2 %>%
#         filter(variable == "CVM",
#                flowtype != "Trade") %>%
#         mutate(country_flow = paste(flowtype, country, sep = " ")) %>%
#         select(country_flow, agg, diff_v_comp_year) %>%
#         spread(c(country_flow), diff_v_comp_year) %>% 
#         left_join(sitc_names, by = join_by(agg == SITC_short_code))
#       
#       table_sitc2_current_v_prev_val <- table_changes_sitc2 %>%
#         filter(variable == "CVM",
#                flowtype != "Trade") %>%
#         mutate(country_flow = paste(flowtype, country, sep = " ")) %>%
#         select(country_flow, agg, diff_v_prev_year) %>%
#         spread(c(country_flow), diff_v_prev_year) %>% 
#         left_join(sitc_names, by = join_by(agg == SITC_short_code)) 
#       
# #}
# 
# #if(percentage_or_value == "percentage") {
#   
#       table_sitc2_current_v_comp_perc <- table_changes_sitc2 %>%
#         filter(variable == "CVM",
#                flowtype != "Trade") %>% # there shouldn't be a 'trade' option to be honest, but leaving this in for safety.
#         mutate(country_flow = paste(flowtype, country, sep = " ")) %>%
#         select(country_flow, agg, ChangevsComparisonYear) %>% #This is the difference to the value option above, one shows % change, the other shows the absolute change in the CVM (one year minus another)
#         spread(c(country_flow), ChangevsComparisonYear) %>% 
#         left_join(sitc_names, by = join_by(agg == SITC_short_code))
#       
#       table_sitc2_current_v_prev_perc <- table_changes_sitc2 %>%
#         filter(variable == "CVM",
#                flowtype != "Trade") %>%
#         mutate(country_flow = paste(flowtype, country, sep = " ")) %>%
#         select(country_flow, agg, ChangevsPreviousYear) %>%
#         spread(c(country_flow), ChangevsPreviousYear) %>% 
#         left_join(sitc_names, by = join_by(agg == SITC_short_code)) 
#       
# #}
# 
# #define numeric columns - this should be fine for both tables as they have similar structure
# numeric_cols_val <- sapply(table_sitc2_current_v_comp_val, class)=="numeric"
# numeric_cols_perc <- sapply(table_sitc2_current_v_comp_perc, class)=="numeric"
# #apply the round function to numeric cols
# table_sitc2_current_v_comp_val[,numeric_cols_val] <- lapply(table_sitc2_current_v_comp[,numeric_cols], round2, digits = 2)
# table_sitc2_current_v_prev_val[,numeric_cols_val] <- lapply(table_sitc2_current_v_prev[,numeric_cols], round2, digits = 2)
# 
# table_sitc2_current_v_comp_perc[,numeric_cols_perc] <- lapply(table_sitc2_current_v_comp[,numeric_cols], round2, digits = 2)
# table_sitc2_current_v_prev_perc[,numeric_cols_perc] <- lapply(table_sitc2_current_v_prev[,numeric_cols], round2, digits = 2)
# 
# table_sitc2_current_v_comp_final <- table_sitc2_current_v_comp_val %>%
#   left_join(table_sitc2_current_v_comp_perc)
# 
# write_csv(table_sitc2_current_v_prev, paste0(output_file_changes, " SITC 2- current v previous year.csv"))
# write_csv(table_sitc2_current_v_comp, paste0(output_file_changes, " SITC 2- current v comparison year.csv"))
