
##### Data Download #####

library(httr)
library(jsonlite)
library(tidyverse)
library(dplyr)

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



##### Processing #####

# RTS_Scotland data

RTS_Scotland <- RTS_data_frame %>% filter(value.GovRegionId == 11)

Scotland_aggregate <- RTS_Scotland %>% 
  group_by(value.MonthId) %>%
  mutate(Value = sum(value.Value),
         NetMass = sum(value.NetMass)) %>%
  select(value.MonthId, Value, NetMass) %>% unique()


# RTS UK data

unknown_regions <- c(13,14) 

RTS_UK <- RTS_data_frame %>%
  select(-c(X.odata.context, value.GovRegionId)) %>%
  #filter(!value.GovRegionId %in% unknown_regions) %>%
  group_by(value.MonthId, value.FlowTypeId, value.CountryId, value.CommoditySitc2Id) %>%
  mutate(Value = sum(value.Value),
         NetMass = sum(value.NetMass)) %>%
  select(-c(value.Value, value.NetMass)) %>% unique()

UK_aggregate <- RTS_UK %>% 
  ungroup() %>%
  group_by(value.MonthId) %>%
  mutate(Value = sum(Value),
         NetMass = sum(NetMass)) %>%
  select(value.MonthId, Value, NetMass) %>% unique()

clipr::write_clip(Scotland_aggregate)

# write to CSV
write_csv(RTS_data_frame, "1) data/raw/RTS 2023 Q2 - All data")
write_csv(RTS_Scotland, "1) data/raw/RTS Scotland 2023 Q2 - API")
write_csv(RTS_UK, "1) data/raw/RTS UK 2023 Q2 - API")

