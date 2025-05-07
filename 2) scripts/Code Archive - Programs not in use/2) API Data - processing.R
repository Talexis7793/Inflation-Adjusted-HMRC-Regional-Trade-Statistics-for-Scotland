library(tidyverse)

# remove scientific notation 
options(scipen=999)

data_date <- "2024Q2"

start_time <- Sys.time()
##### Load in data #####
collapse_to_eu <- TRUE #FALSE = download granular data broken down for flows to each individual country, TRUE = EU/Non-EU only. 
RTS_data_frame <- read_csv(paste0("1) data/raw/RTS ", data_date, " - All data - API.csv"))

##### TEMPORARY FIX TO 81 - PREFAB BUILDINGS #####

# This holds prefab buildings exports to Ireland constant in Netmass in most recent period.
# Not a perfect fix but addresses severe error and shouldn't cause big problems.
#20/3/24 this outlier appears to still be there in the raw data, I did quickly inspect the data in a pivot table to see if there were any other sudden spikes
#16/7/24 this error appears to have been corrected in the raw data
RTS_data_frame <- RTS_data_frame %>% 
  group_by(value.CountryId, value.FlowTypeId, value.GovRegionId, value.CommoditySitc2Id) %>%
  mutate(value.NetMass = case_when(
    (value.MonthId == "202304") &
    (value.CountryId == 7) & 
    (value.CommoditySitc2Id == 81) &
    (value.FlowTypeId == 4) ~ lag(value.NetMass), TRUE ~ value.NetMass)) # Exclude outlier series

##### Load in country and product dictionaries #####

countryid_dictionary <- read_csv("1) data/raw/countryid_dictionary.csv")

sitc2_dictionary <- read_csv("1) data/raw/sitc2_dictionary.csv")

gov_region_dictionary <- read_csv("1) data/raw/gov_region_dictionary.csv")

##### Processing #####

eu_countries <- read_csv("1) data/raw/eu_countries.csv")
eu_codes <- countryid_dictionary %>% filter(CountryName %in% (eu_countries %>% pull(EU))) %>% pull(value.CountryId)

CP_df <- RTS_data_frame %>%
  rename(
    flowtype_eu = value.FlowTypeId, 
    partner = value.CountryId,
    country = value.GovRegionId,  # country is UK regions and countries
    ProductCode = value.CommoditySitc2Id,
    value = value.Value, 
    netmass = value.NetMass, 
    date = value.MonthId 
  ) %>%
  left_join(countryid_dictionary, by = c("partner" = "value.CountryId")) %>%
  left_join(sitc2_dictionary, by = c("ProductCode" = "ProductCode")) %>%
  left_join(gov_region_dictionary, by = join_by(country == id)) %>% 
mutate(
    year = as.character(substr(date,0,4)),
    month = as.character(substr(date, 5 ,6 )))

if(collapse_to_eu == TRUE) {    
  CP_df <- CP_df %>% 
mutate(partner = case_when(partner %in% eu_codes~ "EU",
                                        TRUE ~ "Non EU"))
}

CP_df <- CP_df %>% 
mutate(flowtype_eu = as.character(flowtype_eu),
    flowtype = case_when(flowtype_eu == 3 ~ "Imports",
                         TRUE ~ "Exports"),
    date = as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%m-%d"),
    country = region_name,
    ProductCode = as.character(ProductCode),
    ProductCode = ifelse(nchar(ProductCode) == 1, paste0("0",ProductCode), ProductCode)
  ) %>%
  group_by(date, country, flowtype, partner, ProductCode) %>%
  mutate(value = sum(value),
         netmass = sum(netmass)) %>%
  ungroup() %>%
  select(date, country, flowtype, partner, ProductCode, Sitc2Desc, value, netmass) %>%
  unique()



# RTS_Scotland data

CP_Scotland <- CP_df %>%
  filter(country == "Scotland")

# RTS UK data
#unknown or unallocated regions are not removed from the total. They are still part of UK trade, they could just not be allocated to a region. See HMRC methodology note. 
unknown_regions <- c(13,14) 

CP_UK <- CP_df %>%
  #filter(!country %in% unknown_regions) %>%
  group_by(date, partner,flowtype, ProductCode ) %>%
  mutate(value = sum(value),
         netmass = sum(netmass),
         country = "UK") %>%
  unique() %>% 
  ungroup

# Add UK to dataframe with other regions
CP_all <- rbind(CP_df, CP_UK) %>% 
  select(-c(Sitc2Desc))


CP_UK_Scotland <- rbind(CP_UK, CP_Scotland)

##### Write to CSV# ####

if(collapse_to_eu == TRUE) {  
write_csv(CP_all, paste0("1) data/raw/RTS regions and countries - ",data_date," - API.csv"))
write_csv(CP_UK_Scotland, paste0("1) data/raw/RTS UK and Scotland ",data_date," - API.csv"))
}

if(collapse_to_eu == FALSE) { 
  CP_all <- CP_all  %>% 
    left_join(countryid_dictionary, by = join_by(partner == value.CountryId)) %>% 
    select(-partner) %>% 
    rename(partner = CountryName)
  
  CP_UK_Scotland <- CP_UK_Scotland  %>% 
    left_join(countryid_dictionary, by = join_by(partner == value.CountryId)) %>% 
    select(-partner) %>% 
    rename(partner = CountryName)
  
  write_csv(CP_all, paste0("1) data/raw/RTS regions and countries - dest country breakdown - ",data_date," - API.csv"))
  write_csv(CP_UK_Scotland, paste0("1) data/raw/RTS UK and Scotland - dest country breakdown - ",data_date," - API.csv"))
}

end_time <- Sys.time()
run_time_processing <- end_time - start_time
