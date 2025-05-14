
#####  B) Load in packages and data #####

source("2) scripts/1) Packages.R")

#If you have not used the API to create data for the most recent quarter, go back to the API scipts and do that now, otherwise read in the csv files created by those scripts using the lines below
#source("2) scripts/2) API Data - processing.R") #THIS COULD TAKE A LONG TIME TO RUN
#Alternative to sourcing script 2), load in the csv file which is the final output from that script (if it has been run already recently of course)
        if(use_sa == FALSE) {
          
          if(all_uk == FALSE){
            current_price_values <- read_csv(paste0("1) data/raw/RTS UK and Scotland ", data_date ," - API.csv"))
            output_file <- paste0("3) outputs/CVM - UK & Scotland ",data_date, " (Generated ", as.character(Sys.Date()),").csv")
          }
          
          if(all_uk == TRUE) {
            current_price_values <- read_csv(paste0("1) data/raw/RTS regions and countries - ",data_date," - API.csv"))
            output_file <- paste0("3) outputs/CVM - all UK ",data_date, " (Generated ", as.character(Sys.Date()),").csv")
          }
        }

#Here we use the seasonally adjusted values, and simply rename them to be compatible with the rest of the code 
if(use_sa == TRUE) {
  
  if(all_uk == FALSE){
    
    if(country_breakdown == FALSE){
    current_price_values <- read_csv(paste0("QA/CP SA - UK & Scotland ",data_date," post QA of SA.csv")) %>%
      select(-value, -netmass) %>%
      rename(value = value_sa,
             netmass = netmass_sa)
    output_file <- paste0("3) outputs/CVM - SA - UK & Scotland ",data_date, " (Generated ", as.character(Sys.Date()),").csv")
    }
    
    if(country_breakdown == TRUE){
      
            if(country_product_breakdown == FALSE){
                current_price_values <- read_csv(paste0("QA/CP SA - UK & Scotland - dest country breakdown - ",data_date," post QA of SA.csv")) %>%
                  select(-value, -netmass) %>%
                  rename(value = value_sa,
                         netmass = netmass_sa) %>% 
                  #filter out trade for countries that have been found to be erratic
                  mutate(flow_partner = paste(partner, flowtype)) %>% 
                  filter(!(flow_partner %in% partner_erratics)) %>% 
                  select(-flow_partner)
                
                output_file <- paste0("3) outputs/CVM - SA - UK & Scotland - dest country breakdown - ",data_date, " (Generated ", as.character(Sys.Date()),").csv")
            }
            
            if(country_product_breakdown == TRUE){
              
              current_price_values <- read_csv(paste0("QA/CP SA - UK & Scotland - dest country & product breakdown - ",data_date," post QA of SA.csv")) %>%
                select(-value, -netmass) %>%
                rename(value = value_sa,
                       netmass = netmass_sa) %>% 
              #filter out trade for countries that have been found to be erratic
                mutate(flow_partner = paste(partner, flowtype)) %>% 
                filter(!(flow_partner %in% partner_erratics)) %>% 
                select(-flow_partner)
              
              output_file <- paste0("3) outputs/CVM - SA - UK & Scotland - dest country & product breakdown - ",data_date, " (Generated ", as.character(Sys.Date()),").csv")
            }
    }
  
  }
                      if(all_uk == TRUE){
                      current_price_values <- read_csv(paste0("QA/CP SA - all UK ",data_date," post QA of SA.csv")) %>% 
                        select(-value, -netmass) %>% 
                        rename(value = value_sa,
                               netmass = netmass_sa)
                      output_file <- paste0("3) outputs/CVM - SA - all UK ",data_date, " (Generated ", as.character(Sys.Date()),").csv")
                      }
  }

#-------------------------------------------------------------------------------#

###### C) Generate Previous Year Prices series (PYPs) for each product. #####

# Data should be in a dataframe with the columns including:
#  - date, value, mass, grouping variables (eg. country, products, flow direction or anything else desired, in a c("a","b", "c", ...) format.
#  - you can specify the names of those columns when you call function, otherwise it will default to country and product
#  - currently only works with quarterly data. 


# Generate PYP series
pyp_series_disagg <- generate_pyp(current_price_values,
                                  date = "date", 
                                  grouping_vars = c("country", "flowtype", "partner", "ProductCode"), 
                                  value = "value", 
                                  netmass = "netmass") 

# Filter NAs 
pyp_series_disagg <- pyp_series_disagg %>% filter(ProductCode != "NAvalue")

# Basic QA check
product_date_counts <- pyp_series_disagg  %>%
  ungroup() %>%
  group_by(date, partner, country, flowtype) %>%
  summarise(n=n()) %>% ungroup() %>%
  select(n) %>% unique()

if(length(product_date_counts) > 1){ stop("Could be issue with data")} 
  
#-------------------------------------------------------------------------------#

##### C) Aggregate how you like #####

# - key options: aggregate to total imports/exports, and total trade. Aggregate to SITC1 level.
# - (not 100% convinced it works perfectly in all scenarios, but here it seems to work)


######################################################################################################
 # excl erratic & sitc3 flows by EU & Non EU, country (UK, Scotland) and flow type
pyp_series_part_flow <- pyp_series_disagg %>% ungroup() %>%
  aggregate_pyp(value = "value",
                 filter_on = "ProductCode",
                 filter_to = all_excl_erratic_sitc3,
                 group_vars = c("date", "country", "flowtype", "partner")) %>%
  mutate(agg = "Total excl. sitc3 & erratic") 

 # total excl erratic & sitc3 flows by country (UK, Scotland) and flow type
pyp_series <- pyp_series_disagg %>% ungroup() %>%
  aggregate_pyp(value = "value",
                 filter_on = "ProductCode",
                 filter_to = all_excl_erratic_sitc3,
                 group_vars = c("date", "country", "flowtype")) %>%
  mutate(agg = "Total excl. sitc3 & erratic",
         partner = "World") 

 # EU and Non EU trade for Scotland and UK
pyp_series_trade <- pyp_series_disagg %>% ungroup() %>%
  aggregate_pyp(value = "value",
                 filter_on = "ProductCode",
                 filter_to = all_excl_erratic_sitc3,
                 group_vars = c("date", "country", "partner")) %>%
  mutate(agg = "Total excl. sitc3 & erratic",
         flowtype = "Trade") 

 # total trade for Scotland and UK
pyp_series_t_trade <- pyp_series_disagg %>% ungroup() %>%
  aggregate_pyp(value = "value",
                 filter_on = "ProductCode",
                 filter_to = all_excl_erratic_sitc3,
                 group_vars = c("date", "country")) %>%
  mutate(agg = "Total excl. sitc3 & erratic",
         flowtype = "Trade",
         partner = "World") 

####################################################################################################
#24/9/24 there is interest in how trade is doing when we exclude drink, I'm adding this code in case we ever want to exlude additional product(s) in future  
if(exclude_drink == TRUE){
    # excl erratic & sitc3 & drink flows by EU & Non EU, country (UK, Scotland) and flow type
    pyp_series_part_flow_excl_drink <- pyp_series_disagg %>% ungroup() %>%
      aggregate_pyp(value = "value",
                    filter_on = "ProductCode",
                    filter_to = all_excl_erratic_sitc3_drink,
                    group_vars = c("date", "country", "flowtype", "partner")) %>%
      mutate(agg = "Total excl. sitc3, erratic & drink") 
    
    # total excl erratic & sitc3 flows by country (UK, Scotland) and flow type
    pyp_series_excl_drink <- pyp_series_disagg %>% ungroup() %>%
      aggregate_pyp(value = "value",
                    filter_on = "ProductCode",
                    filter_to = all_excl_erratic_sitc3_drink,
                    group_vars = c("date", "country", "flowtype")) %>%
      mutate(agg = "Total excl. sitc3, erratic & drink",
             partner = "World") 
    
    # EU and Non EU trade for Scotland and UK, total trade
    pyp_series_trade_excl_drink <- pyp_series_disagg %>% ungroup() %>%
      aggregate_pyp(value = "value",
                    filter_on = "ProductCode",
                    filter_to = all_excl_erratic_sitc3_drink,
                    group_vars = c("date", "country", "partner")) %>%
      mutate(agg = "Total excl. sitc3, erratic & drink",
             flowtype = "Trade") 
    
    # total trade for Scotland and UK
    pyp_series_t_trade_excl_drink <- pyp_series_disagg %>% ungroup() %>%
      aggregate_pyp(value = "value",
                    filter_on = "ProductCode",
                    filter_to = all_excl_erratic_sitc3_drink,
                    group_vars = c("date", "country")) %>%
      mutate(agg = "Total excl. sitc3, erratic & drink",
             flowtype = "Trade",
             partner = "World") 
}

#################################################################################
#SITC1 level breakdowns 
# imports and exports by SITC 1 category
pyp_series_sitc1 <- pyp_series_disagg %>% ungroup() %>%
  mutate(sitc1 = substr(ProductCode,0,1)) %>%
  aggregate_pyp(value = "value",
                 filter_on = "ProductCode", 
                 filter_to = all_excl_erratic, 
                 group_vars = c("date", "country", "flowtype", "sitc1")) %>%
  rename("agg" = "sitc1") %>%
  mutate(partner = "World")

pyp_series_sitc1_eu <- pyp_series_disagg %>% ungroup() %>%
  mutate(sitc1 = substr(ProductCode,0,1)) %>%
  aggregate_pyp(value = "value",
                filter_on = "ProductCode", 
                filter_to = all_excl_erratic, 
                group_vars = c("date", "country", "partner", "flowtype", "sitc1")) %>%
  rename("agg" = "sitc1") 

  # combine dataframes
if(country_breakdown == TRUE){
pyp_series_all <- rbind(pyp_series_part_flow,
                        pyp_series, 
                        pyp_series_trade,
                        pyp_series_t_trade,
                        pyp_series_sitc1,
                        pyp_series_sitc1_eu)


  }


if(country_breakdown == FALSE){
  pyp_series_all <- rbind(pyp_series_part_flow,
                          pyp_series, 
                          pyp_series_trade,
                          pyp_series_t_trade,
                          pyp_series_sitc1,
                          pyp_series_sitc1_eu)
  
}

# remove dataframes to clean
rm(pyp_series_part_flow,
   pyp_series, 
   pyp_series_trade,
   pyp_series_t_trade,
   pyp_series_sitc1,
   pyp_series_sitc1_eu
)

if(exclude_drink == TRUE){
  pyp_series_all <- rbind(pyp_series_all, 
                          pyp_series_part_flow_excl_drink,
                          pyp_series_excl_drink, 
                          pyp_series_trade_excl_drink,
                          pyp_series_t_trade_excl_drink)

  rm(pyp_series_part_flow_excl_drink,
      pyp_series_excl_drink, 
      pyp_series_trade_excl_drink,
      pyp_series_t_trade_excl_drink)
}

 # basic (but pretty useless) check
country_agg_counts <- pyp_series_all  %>%
  ungroup() %>%
  group_by(date, country, agg) %>%
  summarise(n=n())

#-------------------------------------------------------------------------------#

###### D) Generate chained volume measure (CVM) series ######

# - Column names will default to those generated by previous function, but you can specify different column names if desired

cvm_series <- generate_cvm(pyp_series_all, 
                           group_vars = c("country", "flowtype", "partner", "agg"), 
                           value = "value", 
                           deflators = TRUE,
                           CVM_index_to = value) #Use this last argument to make the CVM measure appear in £ rather than an index relative to 100 in the base year. use 'value' to make it appear in £ and 100 otherwise

#Add year ending current quarter
quarter_want <- substr(data_date, 5,6)

quarters_to_advance <- case_when(
  quarter_want == "Q1" ~ list("Q2", "Q3", "Q4"),
  
  quarter_want == "Q2" ~ list("Q3", "Q4", ""),
  
  quarter_want == "Q3" ~ list("Q4", "", ""),
  
  quarter_want == "Q4" ~ list("", "", "")
)

cvm_series <- cvm_series %>%
  mutate(month = month(date),
         quarter = case_when(month == 1 ~ "Q1",
                             month == 4 ~ "Q2",
                             month == 7 ~ "Q3",
                             month == 10 ~ "Q4"),
         year = as.numeric(year))

cvm_series$ye_currq <- ifelse(cvm_series$quarter %in% quarters_to_advance, cvm_series$year + 1, cvm_series$year)

#Export
if(write_to_csv == TRUE){write_csv(cvm_series, output_file)}

if(copy_to_clipboard == TRUE){clipr::write_clip(cvm_series)}


#-------------------------------------------------------------------------------#