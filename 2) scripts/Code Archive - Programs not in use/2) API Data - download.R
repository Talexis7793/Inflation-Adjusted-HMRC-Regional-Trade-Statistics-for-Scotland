### API Data ###

# This may not work while in the office. But should work when running from home.

data_date <- "2024Q2"

##### Data Download #####

library(httr)
library(jsonlite)
library(tidyverse)
library(dplyr)

#time how long the program takes, this records the start time:
start_time <- Sys.time()

# remove scientific notation 
options(scipen=999)

# function to read API

readAPIresponse <- function(JSON_input) {
  fromJSON(rawToChar(JSON_input$content))
}

# function to read API and make into dataframe

APItoDF <- function(HTML){
  data_download <- GET(as.character(HTML))
  data_read <-readAPIresponse(data_download)
  data_frame <- data.frame(data_read)
}

# get first 30000 rows
RTS_data_frame <- APItoDF("https://api.uktradeinfo.com/RTS")

# set up loop

newrows = 30000
i = 1

while(newrows == 30000){
 
  print(i)
  
  skip = i * 30000
  
  RTS_data_frame_new <- APItoDF(paste0("https://api.uktradeinfo.com/RTS?$skip=", skip))
  
  newrows = nrow(RTS_data_frame_new)
  
  if(newrows == 30000){
    RTS_data_frame <- rbind(RTS_data_frame, RTS_data_frame_new)
  }else{
    # in the last loop the nextLink column no longer exists in API download
    RTS_data_frame <- rbind(RTS_data_frame %>% select(-c("X.odata.nextLink")), RTS_data_frame_new)
   }
 
  rm(RTS_data_frame_new)
  i = i + 1

} # this should run for around 141 loops before all RTS data is downloaded


# check no duplicated rows
duplicated_rows <- duplicated(RTS_data_frame)
prod(1-duplicated_rows) # if = 1 then all OK

write_csv(RTS_data_frame, paste0("1) data/raw/RTS ", data_date, " - All data - API.csv"))


### Read in dictionaries for country codes and sitc2 products
countryid_dictionary <- APItoDF("https://api.uktradeinfo.com/RTS?$apply=groupby((Country/CountryName, CountryId),%20aggregate(Value%20with%20sum%20as%20TotalValue))") %>% 
  tidyr::unnest(value.Country, names_repair = "unique") %>%
  select(value.CountryId, CountryName) %>%
  mutate(CountryName = ifelse(value.CountryId == "951", paste0(CountryName, " EU"), CountryName))

write_csv(countryid_dictionary, "1) data/raw/countryid_dictionary.csv")

sitc2_dictionary <- APItoDF("https://api.uktradeinfo.com/RTS?$apply=groupby((SITC/sITC2dESC, CommoditySitc2Id),%20aggregate(Value%20with%20sum%20as%20TotalValue))") %>% 
  tidyr::unnest(value.SITC, names_repair = "unique") %>%
  select(value.CommoditySitc2Id, Sitc2Desc) %>%
  rename("ProductCode" = value.CommoditySitc2Id)

write_csv(sitc2_dictionary, "1) data/raw/sitc2_dictionary.csv")

#TA 15/4/23 now getting dictionary for region IDs (meaning regions of the UK aka Scotland, wales England...etc.)

gov_region_dictionary <- APItoDF("https://api.uktradeinfo.com/Region") %>% 
  select(value.RegionId, value.RegionGroupName) %>% 
  rename(id = value.RegionId,
         region_name = value.RegionGroupName)
#  mutate(id = as.character(id))
write_csv(gov_region_dictionary, "1) data/raw/gov_region_dictionary.csv")

end_time <- Sys.time()
run_time_download <- end_time - start_time
