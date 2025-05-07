

##### Merged UK Scotland data #####

### This brings in data for UK and Scotland trade by SITC2 level for EU and NON EU. 

CP_UK_raw <- read_csv(paste0("1) data/raw/OTS Quarterly ", data_date, ".csv")) # sourced from uktrade package
CP_Scotland_raw <- read_csv(paste0("1) data/raw/RTS ",data_date," Scotland.csv")) # sourced from Jessica's team

CP_UK <- CP_UK_raw %>% filter(SuppressionIndex == 0) %>%
  rename(country= FlowTypeDescription,
         ProductCode = Sitc2Code,
         value = TotalValue,
         netmass = TotalNetMass) %>%
  mutate(month = case_when(quarter == "1" ~ 1,
                           quarter == "2" ~ 4,
                           quarter == "3" ~ 7,
                           quarter == "4" ~ 10),
         yearquarter = paste(year, quarter, sep=""),
         date = as.Date(paste(year,"-", month, "-01", sep=""), format = "%Y-%m-%d"),
         flowtype = substr(country, nchar(country) - 6, nchar(country)),
         partner = str_replace_all(substr(country, 0, str_locate(country, "EU") + 1), "-", " " ),
         country = "UK"
         ) %>%
  select(date, country, flowtype, partner, ProductCode, value, netmass) %>%
  unique()


CP_Scotland <- CP_Scotland_raw %>%
  rename(
    flowtype = TradeFlowName, 
    country = ReporterName, 
    ProductCode = SITC_Division,
    value = Value, 
    netmass = Netmass, 
    quarter = Quarter, 
    year = Year ) %>%
  # mutate(
  #   netmass = case_when(
  #             (PartnerName == "EGYPT") &
  #             (flowtype == "Gross Exp.") &
  #             (ProductCode == "05 - Vegetables & fruit") ~ 0 , TRUE ~ netmass)) %>% # Exclude outlier series
  mutate(
    ProductCode = str_sub(ProductCode,0,2),
    quarter = str_sub(quarter, 2,2),
    month = case_when(
      quarter == "1" ~ 1,
      quarter == "2" ~ 4,
      quarter == "3" ~ 7,
      quarter == "4" ~ 10),
    yearquarter = paste(year, quarter, sep=""),
    partner = case_when(World_Area == "European Union" ~ "EU",
                        TRUE ~ "Non EU"),
    flowtype = case_when(flowtype == "Gross Imp." ~ "Imports",
                         TRUE ~ "Exports"),
    date = as.Date(paste(year,"-", month, "-01", sep=""), format = "%Y-%m-%d")) %>%
  group_by(yearquarter, flowtype, ProductCode, partner) %>%
  mutate(value = sum(value),
         netmass = sum(netmass)) %>%
  ungroup() %>%
  select(date, country, flowtype, partner, ProductCode, value, netmass) %>%
  unique()

current_price_values <- rbind(CP_Scotland, CP_UK)
rm(CP_Scotland, CP_UK)

