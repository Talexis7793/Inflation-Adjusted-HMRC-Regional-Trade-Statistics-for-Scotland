#Intial results by EU/non-EU
  source("./2) scripts/control script.R")
  source("./2) scripts/1) Packages.R")
  source("./2) scripts/2) Alternative method to read in data - csv.R")
  #Use the outputs from the below script to set the 'erratic' variable in the control script:
  source("./determine volatility.R")
  source("./2) scripts/2b) seasonal adjustment.R")
  source("./2) scripts/2c) seasonal adjustment QA.R")
  source("./2) scripts/3) CVM Calculations.R")
  source("./2) scripts/3b) CVM calculations at SITC2 level.R")


#trading partner level results, these can either be by indidual country or aggregated into world areas
#21.3.25 this is currently by world area
#Note you don't run the SITC2 level CVM calculations for this (data is likely too volatile)
  source("./2) scripts/control script.R")
  source("./2) scripts/1) Packages.R")
  source("./2) scripts/2) Alternative method to read in data - csv.R")
  #Use the outputs from the below script to set the 'erratic' variable in the control script:
  source("./determine volatility.R")
  source("./2) scripts/2b) seasonal adjustment.R")
  source("./2) scripts/2c) seasonal adjustment QA.R")
  source("./2) scripts/3) CVM Calculations.R")

#Single country level analysis: this gives SITC2 level breakdown for a single country
  source("./2) scripts/control script.R")
  source("./2) scripts/1) Packages.R")
  source("./2) scripts/2) Alternative method to read in data - csv.R")
  #Use the outputs from the below script to set the 'erratic' variable in the control script:
  source("./determine volatility.R")
  if(single_country != FALSE) {
    temp <- read_csv(paste0("./1) data/raw/RTS ", region_choice, " - dest country & product breakdown - ",data_date,".csv")) %>% 
      filter(partner == single_country)
    
    write_csv(temp, paste0("1) data/raw/RTS ", region_choice, " - dest country & product breakdown - ",data_date,".csv"))
  }
  source("./2) scripts/2b) seasonal adjustment.R")
  source("./2) scripts/2c) seasonal adjustment QA.R")
  source("./2) scripts/3b) CVM Calculations at SITC2 level.R")
