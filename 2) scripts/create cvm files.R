
### Instructions ###
  #set the control script before running anything, then run each line of this script one by one (don't source the whole thing)
  #Each block of source statements will produce one output. Different blocks are for different outputs at different levels of granularity
  #Before each block are recommendations for settings on the control script. Note that if a setting is not specifically mentioned, you should be
  #able to just leave it as it is currently configured. Several of these options are left over from the development phase of this work, and can
  #likely be left alone, or removed from the code in later development. There are comments in the control script to explain what each
  #setting does, feel free to experiment with different values of defaults or explore what they do in the code!

  #Note if you are running the process for trade flows to individual world areas or partner countries, there are two kinds of exclusions:
  #whole countries/areas are excluded (e.g. if exports to Canada is found to be erratic, this is excluded, look for the text file in the outputs
  #folder called partner_exclusions for a list of these). Additionally the erratic products are excluded from country trade flows, look again for 
  #the text file in the outputs folder listing these.

#Intial results by EU/non-EU:
  #settings for control script (leave the rest as defaults, meaning how they are currently set):
    #update the data_date
    #collapse_to_eu = TRUE
    #collapse_to_world_area = FALSE
    #country_breakdown = FALSE
    #country_product_breakdown = FALSE
    #single_country = FALSE
    #file_date = today's date
  source("./2) scripts/control script.R")
  source("./2) scripts/1) Packages.R")
  source("./2) scripts/2) Alternative method to read in data - csv.R")
  source("./2) scripts/2a) determine volatility.R")
  source("./2) scripts/2b) seasonal adjustment.R")
  source("./2) scripts/2c) seasonal adjustment QA.R")
  source("./2) scripts/3) CVM Calculations.R")
  source("./2) scripts/3b) CVM calculations at SITC2 level.R")
  #Note the below is run just as a visual check of the results to help with QA, the actual functions to produce the charts in the
  #publication come from the RTS report automation project.
    source("./2) scripts/5) Visualisation.R")


#trading partner level results, these can either be by indidual country or aggregated into world areas
#This flow currently produced data by world area not individual partner country, since we only have the time to run it at this level 
#on the pre-release data and still get the publication out in time. The collapse_to_world_area = TRUE option controls this. 
#If you do want this flow by individual partner country, then set this option to FALSE.
#Note you don't run the SITC2 level CVM calculations for this (data is likely too volatile)
  #settings for control script (leave the rest as defaults, meaning how they are currently set):
    #update the data_date
    #collapse_to_eu = FALSE
    #collapse_to_world_area = TRUE 
    #country_breakdown = FALSE
    #country_product_breakdown = FALSE
    #single_country = FALSE
    #file_date = today's date
  source("./2) scripts/control script.R")
  #The control script overwrites the erratics vector with the products we know to be erratic for Scotland. This next step updates this 
  #vector with products we found in this project (in the EU/non-EU breakdown above)
  erratic <- c(erratic, new_erratics) %>% unique()
  source("./2) scripts/1) Packages.R")
  source("./2) scripts/2) Alternative method to read in data - csv.R")
  source("./2) scripts/2a) determine volatility.R")
  source("./2) scripts/2b) seasonal adjustment.R")
  source("./2) scripts/2c) seasonal adjustment QA.R")
  source("./2) scripts/3) CVM Calculations.R")

#Single country level analysis: this gives SITC2 level breakdown for a single country
  #settings for control script (leave the rest as defaults, meaning how they are currently set):
    #update the data_date
    #collapse_to_eu = FALSE
    #collapse_to_world_area = FALSE 
    #country_breakdown = TRUE
    #country_product_breakdown = TRUE
    #single_country = Enter the name of the country surrounded by "" exactly as it appears in the data, e.g. "United States"
    #file_date = today's date
  source("./2) scripts/control script.R")
  #The control script overwrites the erratics vector with the products we know to be erratic for Scotland. This next step updates this 
  #vector with products we found in this project (in the EU/non-EU breakdown above)
  erratic <- c(erratic, new_erratics) %>% unique()
  source("./2) scripts/1) Packages.R")
  source("./2) scripts/2) Alternative method to read in data - csv.R")
    single_country_only <- read_csv(paste0("./1) data/raw/RTS ", region_choice, " - dest country & product breakdown - ",data_date,".csv")) %>% 
      filter(partner == single_country)
    write_csv(single_country_only, paste0("1) data/raw/RTS ", region_choice, " - ", single_country, " product breakdown - ",data_date,".csv"))
  source("./2) scripts/2a) determine volatility.R")
  source("./2) scripts/2b) seasonal adjustment.R")
  source("./2) scripts/2c) seasonal adjustment QA.R")
  source("./2) scripts/3b) CVM Calculations at SITC2 level.R")

### Further development:
  #see how script 2c handles the input, this could be done across scripts to make integration easier and less error prone. It simply
  #makes the input file = output file from previous program. 
    