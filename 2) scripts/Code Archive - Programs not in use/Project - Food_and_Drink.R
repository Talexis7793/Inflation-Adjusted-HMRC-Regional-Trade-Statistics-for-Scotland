source("2) scripts/1) Packages.R")
source("2) scripts/2) Data.R")


##### DATA: Food and Drink stats ####

 # filter to food and drink products
CP_fd <- CP_Scotland %>% 
  filter(str_sub(ProductCode, 0,1) == "0" | ProductCode == "11") %>%
  select(-c(country))

PYP_fd <- CP_fd %>% 
  generate_pyp(grouping_vars = c("partner", "ProductCode", "flowtype"))
 
 # get food and drink products
fd_prods <- CP_fd %>% pull(ProductCode) %>% unique()
food_prods = fd_prods[-which(fd_prods == "11")]
drink_prods = c("11") 

cvm_fd_partner <- PYP_fd %>% ungroup() %>%
  aggregate_pyp(value = "value",
                filter_on = "ProductCode",
                filter_to = fd_prods,
                group_vars = c("date", "flowtype", "partner")) %>%
  mutate(agg = "Food and drink") %>% 
  rbind(aggregate_pyp(PYP_fd, value = "value",
                  filter_on = "ProductCode",
                  filter_to = fd_prods,
                  group_vars = c("date", "flowtype")) %>%
        mutate(agg = "Food and drink",
             partner = "World")) %>% 
  generate_cvm(group_vars = c("agg", "partner", "flowtype"))

clipr::write_clip(cvm_fd_partner)

# cvm_food_partner <- PYP_fd %>% ungroup() %>%
#   aggregate_pyp(value = "value",
#                 filter_on = "ProductCode",
#                 filter_to = food_prods,
#                 group_vars = c("date", "flowtype", "partner")) %>%
#   mutate(agg = "0 - Food") %>%
#   generate_cvm(group_vars = c("agg", "partner", "flowtype"))
# 


cvm_sitc2_fd<- PYP_fd %>% ungroup() %>%
  aggregate_pyp(value = "value",
                filter_on = "ProductCode",
                filter_to = fd_prods,
                group_vars = c("date", "ProductCode", "flowtype", "partner")) %>%
  rbind(
  aggregate_pyp(PYP_fd, value = "value",
                filter_on = "ProductCode",
                filter_to = fd_prods,
                group_vars = c("date", "ProductCode", "flowtype"))  %>%
  mutate(partner = "World") 
  ) %>%
  generate_cvm(group_vars = c("ProductCode", "partner", "flowtype")) %>%
  select(
    date, ProductCode, flowtype, partner, value, netmass, value_index, CVM, implied_deflator) )

clipr::write_clip(cvm_sitc2_fd)

product_date_counts <- cvm_sitc2_fd  %>%
  ungroup() %>%
  group_by(ProductCode) %>%
  summarise(n=n()) %>% ungroup() 



# Trade-by-country work - for a later date

# CP_fooddrink<- CP_Scotland_raw %>%
#   rename(
#     flowtype = TradeFlowName, 
#     country = ReporterName, 
#     ProductCode = SITC_Division,
#     partner = PartnerName,
#     value = Value, 
#     netmass = Netmass, 
#     quarter = Quarter, 
#     year = Year ) %>%
#   mutate(
#     netmass = case_when(
#       (partner == "EGYPT") &
#         (flowtype == "Gross Exp.") &
#         (ProductCode == "05 - Vegetables & fruit") ~ 0 , TRUE ~ netmass)) %>%
#   filter(
#     country == "Scotland",
#     str_sub(ProductCode,0,1) == "0" | str_sub(ProductCode,0,2) == "11",
#     partner != "STORES & PROVIS.") %>%
#   mutate(
#     ProductCode = str_sub(ProductCode,0,2),
#     quarter = str_sub(quarter, 2,2),
#     month = case_when(
#       quarter == "1" ~ 1,
#       quarter == "2" ~ 4,
#       quarter == "3" ~ 7,
#       quarter == "4" ~ 10),
#     yearquarter = paste(year, quarter, sep=""),
#     partner = case_when(PartnerName == "UNITED STATES" ~ "USA",
#                         PartnerName == "IRISH REPUBLIC" ~ "Ireland",
#                         PartnerName == "FRANCE" ~ "France",
#                         TRUE ~ World_Area),
#     flowtype = case_when(flowtype == "Gross Imp." ~ "Imports",
#                          TRUE ~ "Exports"),
#     date = as.Date(paste(year,"-", month, "-01", sep=""), format = "%Y-%m-%d")) %>%
#   group_by(yearquarter, flowtype, ProductCode, partner) %>%
#   mutate(value = sum(value),
#          netmass = sum(netmass)) %>%
#   ungroup() %>%
#   select(date, country, flowtype, partner, ProductCode, value, netmass) %>%
#   unique()
# 

### Whisky and Salmon

# Filter to these two products
# Select only exports and aggregate to world exports
# PYP for each CN8 code
# Aggregate to total whisky and total salmon

# Can try EU-Non EU split later




### HS Level Data

hs_level= "Cn8Code"

CP_hs_raw <- read_csv(paste0("1) data/raw/Detailed food and drink/", hs_level, ".csv")) # sourced from Jessica's team

CP_hs0 <- CP_hs_raw %>%
  rename(flowtype_partner = FlowTypeDescription,
         ProductCode = {{hs_level}},
         value = Value,
         netmass = NetMass,) %>%
  filter(ProductCode != "NAvalue" ) %>%
  mutate(
    flowtype = str_sub(flowtype_partner, nchar(flowtype_partner) - 6, nchar(flowtype_partner)),
    partner = str_sub(flowtype_partner, 0, nchar(flowtype_partner)-7 )) %>%
  filter(flowtype == "Exports") %>%
  select(-c(1, flowtype_partner, partner)) %>%
  group_by(date, ProductCode) %>%
  mutate(
    value = sum(value),
    netmass = sum(netmass)
  ) %>%
  unique()

QA1 <- CP_hs0 %>% 
  ungroup() %>%
  group_by(date, ProductCode) %>%
  summarise(n=n()) %>% 
  spread(ProductCode,n) %>%
  ungroup() %>%
  select(-c(date)) %>% 
  select_if(~prod(!is.na(.)) > 0) # removes columns with one NA. Could use sum instead of prod to remove columns with all NAs.


CVM_hs <- CP_hs0%>%  
  select(flowtype, ProductCode, value, netmass, date) %>%
  generate_pyp(
               date = "date", 
               grouping_vars = c("flowtype", "ProductCode"), 
               value = "value", 
               mass = "netmass") %>%
  mutate(hs2code = substr(ProductCode, 0, 2)) %>% ungroup() %>%
  aggregate_pyp(filter_on = "hs2code",
                filter_to = c("03", "16"),
                group_vars = c("date", "hs2code")) %>%
  generate_cvm(group_vars = c("hs2code"))

clipr::write_clip(CVM_hs)



### Whisky and salmon only

whiskyandsalmon <- c("22083030",
                     "22083041",
                     "22083049",
                     "22083061",
                     "22083069",
                     "22083071",
                     "22083079",
                     "03021400" )

CP_whiskysalmon<- CP_hs_raw %>%
  rename(flowtype_partner = FlowTypeDescription,
         ProductCode = {{hs_level}},
         value = Value,
         netmass = NetMass,) %>%
  filter(ProductCode != "NAvalue" ) %>%
  mutate(
    flowtype = str_sub(flowtype_partner, nchar(flowtype_partner) - 6, nchar(flowtype_partner)),
    partner = str_sub(flowtype_partner, 0, nchar(flowtype_partner)-7 )) %>%
  filter(flowtype == "Exports") %>%
  select(-c(1, flowtype_partner)) %>%
  group_by(date, ProductCode, partner) %>%
  mutate(
    value = sum(value),
    netmass = sum(netmass)
  ) %>%
  unique()

PYP_whiskysalmon <- 
  CP_whiskysalmon %>%  
  select(flowtype, ProductCode, partner, value, netmass, date) %>%
  generate_pyp(
    date = "date", 
    grouping_vars = c("flowtype", "ProductCode", "partner"), 
    value = "value", 
    mass = "netmass") %>%
  mutate(hs2code = substr(ProductCode, 0, 2)) %>% ungroup() 

CVM_whiskysalmon <- 
  PYP_whiskysalmon %>%
  aggregate_pyp(filter_on = "ProductCode",
                filter_to = whiskyandsalmon,
                group_vars = c("date", "hs2code", "partner")) %>%
  rbind(PYP_whiskysalmon %>% 
          aggregate_pyp(filter_on = "ProductCode",
                        filter_to = whiskyandsalmon,
                        group_vars = c("date", "hs2code")) %>%
          mutate(partner = "World")
          ) %>%
  generate_cvm(group_vars = c("hs2code", "partner")) %>%

clipr::write_clip(CVM_whiskysalmon)

QA1 <- CVM_whiskysalmon %>% 
  ungroup() %>%
  filter(ProductCode %in% whiskyandsalmon,
         netmass != 0) %>%
  group_by(ProductCode, partner) %>%
  summarise(n=n())

##### PROCESSING #####

current_price_values <- current_price_uk_scotland %>% select(-c(country))

# Generate PYP series
pyp_series_disagg <- generate_pyp(current_price_values,
                                  date = "date", 
                                  grouping_vars = c("flowtype", "partner", "ProductCode"), 
                                  value = "value", 
                                  mass = "netmass") 
# Basic QA check
partner_flowtype_counts <- pyp_series_disagg  %>% 
  ungroup() %>%
  filter(PYP != 0) %>%
  group_by(partner, flowtype) %>%
  summarise(n=n()) %>% ungroup() %>%
  unique()

product_date_counts <- pyp_series_disagg  %>% 
  ungroup() %>%
  filter(PYP != 0) %>%
  group_by(date, ProductCode, flowtype) %>%
  summarise(n=n()) %>%
  unique()
#-------------------------------------------------------------------------------#

##### C) Aggregate how you like #####

# - key options: aggregate to total imports/exports, and total trade. Aggregate to SITC1 level.
# - (not 100% convinced it works perfectly in all scenarios, but here it seems to work)


# set product categories that you want to use to filter
all_products <- current_price_values %>% pull(ProductCode) %>% unique()
erratic <- c("66","68", "79", "93", "97")
sitc3 <- all_products[which(substr(all_products, 0,1) == "3")] # MIGHT NEED TO ADD MORE
all_excl_79 <- all_products[!all_products %in% c("79")]
all_excl_erratic_sitc3 <- all_products[!all_products %in% c(erratic, sitc3, "NAvalue")]

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

# imports and exports by SITC 1 category
pyp_series_sitc1 <- pyp_series_disagg %>% ungroup() %>%
  mutate(sitc1 = substr(ProductCode,0,1)) %>%
  aggregate_pyp(value = "value",
                filter_on = "ProductCode", 
                filter_to = all_excl_79, 
                group_vars = c("date", "country", "flowtype", "sitc1")) %>%
  rename("agg" = "sitc1") %>%
  mutate(partner = "World")

# combine dataframes
pyp_series_all <- rbind(pyp_series_part_flow,
                        pyp_series, 
                        pyp_series_trade,
                        pyp_series_t_trade,
                        pyp_series_sitc1)


# remove dataframes to clean
rm(pyp_series_part_flow,
   pyp_series, 
   pyp_series_trade,
   pyp_series_t_trade,
   pyp_series_sitc1)

# basic (but pretty useless) check
country_agg_counts <- pyp_series_all  %>%
  ungroup() %>%
  group_by(date, country, agg) %>%
  summarise(n=n())


#-------------------------------------------------------------------------------#

###### D) Generate chained value series ######

# - Column names will default to those generated by previous function, but you can specify different column names if desired

cvm_series <- generate_cvm(pyp_series_all, 
                           group_vars = c("country", "flowtype", "partner", "agg"), 
                           value = "value", 
                           deflators = TRUE)

if(write_to_csv == TRUE){write_csv(cvm_series, output_file)}

if(copy_to_clipboard == TRUE){clipr::write_clip(cvm_series)}

