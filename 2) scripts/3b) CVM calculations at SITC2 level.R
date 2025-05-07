#This script will create CVM and IDEF series at SITC2 level of aggregation. The most granular data produced by 3)CVM calculations script is at SITC1 level but we want to be able to examine the CVM series at SITC2 level

if(use_sa == FALSE) {
        
        if(all_uk == FALSE){
                current_price_values <- read_csv(paste0("1) data/raw/RTS UK and Scotland ", data_date ," - API.csv"))
                output_file <- paste0("3) outputs/CVM SITC2 - UK & Scotland ",data_date, " (Generated ", as.character(Sys.Date()),").csv")        
        }
        
        if(all_uk == TRUE) {
                current_price_values <- read_csv(paste0("1) data/raw/RTS regions and countries - ",data_date," - API.csv"))
                output_file <- paste0("3) outputs/CVM SITC2 - all UK ",data_date, " (Generated ", as.character(Sys.Date()),").csv")
        }
}

#Here we use the seasonally adjusted values, and rename them to be compatible with the rest of the code 
if(use_sa == TRUE) {
        
        if(all_uk == FALSE){
          if(country_breakdown == FALSE){
                current_price_values <- read_csv(paste0("QA/CP SA - UK & Scotland ",data_date," post QA of SA.csv")) %>%
                        select(-value, -netmass) %>%
                        rename(value = value_sa,
                               netmass = netmass_sa)
                output_file <- paste0("3) outputs/CVM SITC2 - SA - UK & Scotland ",data_date, " (Generated ", as.character(Sys.Date()),").csv")
          }
          
          if(country_breakdown == TRUE){
            
                if(country_product_breakdown == TRUE){
                    current_price_values <- read_csv(paste0("QA/CP SA - UK & Scotland - dest country & product breakdown - ",data_date," post QA of SA.csv")) %>%
                      select(-value, -netmass) %>%
                      rename(value = value_sa,
                             netmass = netmass_sa)
                    output_file <- paste0("3) outputs/CVM SITC2 - SA - UK & Scotland - dest country & product breakdown - ",data_date, " (Generated ", as.character(Sys.Date()),").csv")
                }
                
                if(country_product_breakdown == FALSE){
                  current_price_values <- read_csv(paste0("QA/CP SA - UK & Scotland - dest country breakdown - ",data_date," post QA of SA.csv")) %>%
                    select(-value, -netmass) %>%
                    rename(value = value_sa,
                           netmass = netmass_sa)
                  output_file <- paste0("3) outputs/CVM SITC2 - SA - UK & Scotland - dest country breakdown - ",data_date, " (Generated ", as.character(Sys.Date()),").csv")
                }
          }
        }
        
        if(all_uk == TRUE){
                current_price_values <- read_csv(paste0("QA/CP SA - all UK ",data_date," post QA of SA.csv")) %>% 
                        select(-value, -netmass) %>% 
                        rename(value = value_sa,
                               netmass = netmass_sa)
                output_file <- paste0("3) outputs/CVM SITC2 - SA - all UK ",data_date, " (Generated ", as.character(Sys.Date()),").csv")
        }
}

# Generate PYP series
pyp_series_disagg <- generate_pyp(current_price_values,
                                  date = "date", 
                                  grouping_vars = c("country", "flowtype", "partner", "ProductCode"), 
                                  value = "value", 
                                  netmass = "netmass") 

# Filter NAs 
pyp_series_disagg <- pyp_series_disagg %>% filter(ProductCode != "NAvalue")

####################################################################################################################################
#pyp_series_disagg comes from script 3), use this to calculate the pyp series, this time at sitc2 level (this is what makes this script different from script 3)
        pyp_series_sitc2 <- pyp_series_disagg %>% ungroup() %>%
          mutate(sitc2 = substr(ProductCode,0,2)) %>%
          aggregate_pyp(value = "value",
                        filter_on = "ProductCode", 
                        filter_to = all_excl_erratic_sitc3, #I did have this set to all_excl_erratic, but actually I don't think there is a reason to exclude them; if the extreme variation in some products doesn't contribute to a total then it should be fine to include them.  
                        group_vars = c("date", "country", "flowtype", "partner", "sitc2")) %>% 
          rename("agg" = "sitc2") 

#Add rows for the aggregated totals
        # excl erratic & sitc3 flows by EU & Non EU, country (UK, Scotland) and flow type
        pyp_series_part_flow <- pyp_series_disagg %>% ungroup() %>%
          aggregate_pyp(value = "value",
                        filter_on = "ProductCode",
                        filter_to = all_excl_erratic_sitc3,
                        group_vars = c("date", "country", "flowtype", "partner")) %>%
          mutate(agg = "Total excl. sitc3 & erratic") 

#Add rows for world totals (remove partner as grouping var)
        pyp_series_sitc2_world <- pyp_series_disagg %>% ungroup() %>%
                mutate(sitc2 = substr(ProductCode,0,2)) %>%
                aggregate_pyp(value = "value",
                              filter_on = "ProductCode", 
                              filter_to = all_excl_erratic_sitc3, #I did have this set to all_excl_erratic, but actually I don't think there is a reason to exclude them; if the extreme variation in some products doesn't contribute to a total then it should be fine to include them.  
                              group_vars = c("date", "country", "flowtype", "sitc2")) %>% 
                rename("agg" = "sitc2") |> 
                mutate(partner = "World") 
        
        #total excl erratic & sitc3 flows by country (UK, Scotland) and flow type
        pyp_series_world <- pyp_series_disagg %>% ungroup() %>%
          aggregate_pyp(value = "value",
                        filter_on = "ProductCode",
                        filter_to = all_excl_erratic_sitc3,
                        group_vars = c("date", "country", "flowtype")) %>%
          mutate(agg = "Total excl. sitc3 & erratic",
                 partner = "World")
#################################################################################################################################################

if(exclude_drink == TRUE){        
        #Add rows for the aggregated totals
        # excl erratic & sitc3 & drink flows by EU & Non EU, country (UK, Scotland) and flow type
        pyp_series_part_flow_excl_drink <- pyp_series_disagg %>% ungroup() %>%
                aggregate_pyp(value = "value",
                              filter_on = "ProductCode",
                              filter_to = all_excl_erratic_sitc3_drink,
                              group_vars = c("date", "country", "flowtype", "partner")) %>%
                mutate(agg = "Total excl. sitc3, erratic & drink") 
        
        
        #total excl erratic & sitc3 & drink flows by country (UK, Scotland) and flow type
        pyp_series_world_excl_drink <- pyp_series_disagg %>% ungroup() %>%
                aggregate_pyp(value = "value",
                              filter_on = "ProductCode",
                              filter_to = all_excl_erratic_sitc3_drink,
                              group_vars = c("date", "country", "flowtype")) %>%
                mutate(agg = "Total excl. sitc3, erratic & drink",
                       partner = "World")        
}
################################################################################################################################################

# combine dataframes
pyp_series_all_sitc2 <- rbind(pyp_series_sitc2,
                              pyp_series_part_flow,
                              pyp_series_sitc2_world,
                              pyp_series_world)
        
        if(exclude_drink == TRUE){ 
         pyp_series_all_sitc2 <- rbind(pyp_series_all_sitc2,
                                       pyp_series_part_flow_excl_drink,
                                       pyp_series_world_excl_drink)       
        }


# remove dataframes to clean
rm(pyp_series_sitc2,
   pyp_series_part_flow,
   pyp_series_sitc2_world,
   pyp_series_world
)

if(exclude_drink == TRUE){ 
        rm(
      pyp_series_part_flow_excl_drink,
      pyp_series_world_excl_drink)       
}

#Now generate the CVM on the pyp series data at sitc2 level
cvm_series_sitc2 <- generate_cvm(pyp_series_all_sitc2, 
                           group_vars = c("country", "flowtype", "partner", "agg"), #Had to remove partner from this as well 
                           value = "value", 
                           deflators = TRUE,
                           CVM_index_to = value)

#add year ending current Q
quarter_want <- substr(data_date, 5,6)

quarters_to_advance <- case_when(
        quarter_want == "Q1" ~ list("Q2", "Q3", "Q4"),
        
        quarter_want == "Q2" ~ list("Q3", "Q4", ""),
        
        quarter_want == "Q3" ~ list("Q4", "", ""),
        
        quarter_want == "Q4" ~ list("", "", "")
)

cvm_series_sitc2 <- cvm_series_sitc2 %>%
        mutate(month = month(date),
               quarter = case_when(month == 1 ~ "Q1",
                                   month == 4 ~ "Q2",
                                   month == 7 ~ "Q3",
                                   month == 10 ~ "Q4"),
               year = as.numeric(year))

cvm_series_sitc2$ye_currq <- ifelse(cvm_series_sitc2$quarter %in% quarters_to_advance, cvm_series_sitc2$year + 1, cvm_series_sitc2$year)

#export
if(write_to_csv == TRUE){write_csv(cvm_series_sitc2, output_file)}

if(copy_to_clipboard == TRUE){clipr::write_clip(cvm_series_sitc2)}
