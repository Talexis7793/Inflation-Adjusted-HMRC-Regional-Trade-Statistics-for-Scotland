library(tidyverse)
library(janitor)
library(anytime)

start_time <- Sys.time()

##### Load in data #####
RTS_data_frame <- read_csv(paste0("1) data/raw/HMRC RTS ",year_want," ", quarter_want," - UK data SITC2 level - imports and exports.csv")) |> 
  clean_names()

##### Load in country and product dictionaries #####

countryid_dictionary <- read_csv("1) data/raw/countryid_dictionary.csv")

sitc2_dictionary <- read_csv("1) data/raw/sitc2_dictionary.csv")

gov_region_dictionary <- read_csv("1) data/raw/gov_region_dictionary.csv")

##### TEMPORARY FIX TO 81 - PREFAB BUILDINGS #####

# This holds prefab buildings exports to Ireland constant in Netmass in most recent period.
# Not a perfect fix but addresses severe error and shouldn't cause big problems.
#20/3/24 this outlier appears to still be there in the raw data, I did quickly inspect the data in a pivot table to see if there were any other sudden spikes
#16/7/24 this error appears to have been corrected in the raw data
CP_df <- RTS_data_frame %>% 
  group_by(partner_name, trade_flow_name, reporter_name, sitc_division) %>%
  mutate(summed_netmass = case_when(
    (year == "2023" & quarter == "Q2") &
      (partner_name == "IRISH REPUBLIC") & 
      (sitc_division == "81 - P/fab buildings;sanit.,plumbing,heating &lighting fixt.") &
      (trade_flow_name == "Gross Exp.") ~ lag(summed_netmass), 
    TRUE ~ summed_netmass)) |>  # Exclude outlier series
  ungroup() |>
  mutate(month = case_when(quarter == "Q1" ~ "01",
                           quarter == "Q2" ~ "04",
                           quarter == "Q3" ~ "07",
                           quarter == "Q4" ~ "10",),
         day = "01",
         date = anydate(paste(year, month, day, sep = "-")),
         ProductCode = as.numeric(str_sub(sitc_division, 1, 2)),
         partner_name = str_to_title(partner_name)) |> 
  rename(partner = partner_name,
         country = reporter_name,
         flowtype = trade_flow_name,
         value = summed_value,
         netmass = summed_netmass
         ) |> 
  select(-day, - month, -year, -quarter, -sitc_division, -sitc_section) |>
  left_join(sitc2_dictionary, by = c("ProductCode" = "ProductCode")) 

#Check to see if there are any countries with multiple world areas (like stores & provisions) - this dataset will be used later
check_world_area <- CP_df %>% distinct(partner, world_area)
check_world_area <- check_world_area[duplicated(check_world_area[,"partner"]), ] %>% 
  pull(partner)
print(paste("The following countries have multiple world areas:", check_world_area)) #Check the message this prints, stores & provisions is dealt with already

#Deal with countries that have multiple world areas (currently only stores and provisions).
#Do this by concatenating partner name with world area to make them unique and separate entries in the partner field.
#They have to be distinguished by the information in the partner field alone so that subsequent programs will treat them separately
CP_df <- CP_df %>% 
  mutate(partner = case_when(partner %in% check_world_area ~ paste(partner, world_area),
                             .default = partner))

##### Processing #####

#eu_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Irish Republic", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")
#eu_codes <- countryid_dictionary %>% filter(CountryName %in% eu_countries) %>% pull(value.CountryId)

if(collapse_to_eu == TRUE) {    
  #The EU/non-EU breakdown produced by this step will be used for every part of the analysis/presentation except the country level breakdown
  CP_df <- CP_df %>% 
    mutate(partner = case_when(world_area == "European Union" ~ "EU",
                               TRUE ~ "Non EU"))
}

if(collapse_to_world_area == TRUE) {    
  #The EU/non-EU breakdown produced by this step will be used for every part of the analysis/presentation except the country level breakdown
  CP_df <- CP_df %>% 
    mutate(partner = world_area) %>% 
    select(-world_area)
}

CP_df <- CP_df %>% 
  mutate(flowtype = case_when(flowtype == "Gross Exp." ~ "Exports",
                              flowtype == "Gross Imp." ~ "Imports"),
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
  group_by(date, partner, flowtype, ProductCode) %>%
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
  write_csv(CP_all, paste0("1) data/raw/RTS regions and countries - ",data_date,".csv"))
  write_csv(CP_UK_Scotland, paste0("1) data/raw/RTS UK and Scotland ",data_date,".csv"))
}

if(collapse_to_eu == FALSE) {
  
  if(country_product_breakdown == FALSE) {
    
        CP_all <- CP_all %>% 
          group_by(across(-c(ProductCode, value, netmass))) %>% 
          summarise(value = sum(value),
                    netmass = sum(netmass)) %>% 
          mutate(ProductCode = "All",
                 Sitc2Desc = "All")
          
        CP_UK_Scotland <- CP_UK_Scotland %>% 
          group_by(across(-c(ProductCode, Sitc2Desc, value, netmass))) %>% 
          summarise(value = sum(value),
                    netmass = sum(netmass)) %>% 
          mutate(ProductCode = "All",
                 Sitc2Desc = "All")
        
        
        write_csv(CP_all, paste0("1) data/raw/RTS regions and countries - dest country breakdown - ",data_date,".csv"))
        write_csv(CP_UK_Scotland, paste0("1) data/raw/RTS UK and Scotland - dest country breakdown - ",data_date,".csv"))
  }
  
  if(country_product_breakdown == TRUE) {
        write_csv(CP_all, paste0("1) data/raw/RTS regions and countries - dest country & product breakdown - ",data_date,".csv"))
        write_csv(CP_UK_Scotland, paste0("1) data/raw/RTS UK and Scotland - dest country & product breakdown - ",data_date,".csv"))
  }
}

end_time <- Sys.time()
run_time_processing <- end_time - start_time

# set product categories that you want to use to filter later (for aggregating CVM)
all_products <- CP_UK_Scotland %>% pull(ProductCode) %>% unique()
sitc3 <- all_products[which(substr(all_products, 0,1) == "3")] # MIGHT NEED TO ADD MORE
drink <- c("11") #24.9.24 there is interest in how exports are doing excluding whisky, we only have the data to SITC2 level so need to just exclude "Drink"
all_excl_79 <- all_products[!all_products %in% c("79")]
all_excl_erratic <- all_products[!all_products %in% c(erratic)]
all_excl_erratic_sitc3 <- all_products[!all_products %in% c(erratic, sitc3, "NAvalue")]
all_excl_erratic_sitc3_drink <- all_products[!all_products %in% c(erratic, sitc3, drink, "NAvalue")]

