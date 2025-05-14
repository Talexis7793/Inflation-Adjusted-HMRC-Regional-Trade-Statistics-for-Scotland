# remove scientific notation 
options(scipen=999)
library(tidyverse)

#It is very important to check all of these options every time you run a process flow. The results may not be right or you might get an error if these options are not set 
#consistently with each other.

#Basic options:
    data_date <- "2024Q4"
      year_want <- str_sub(data_date, 1, 4)
      quarter_want <- str_sub(data_date, 5, 6)
    write_to_csv <- TRUE #always leave as TRUE
    copy_to_clipboard <- FALSE
    
    API_input = FALSE #Did the data come from the API?
      if(API_input == TRUE){
        api_suffix <- " - API" 
      }
      if(API_input == FALSE){
        api_suffix <- "" 
      }

#Raw (CP) data options:
    collapse_to_eu <- FALSE #FALSE = download granular data broken down for flows to each individual country, TRUE = EU/Non-EU only.
    collapse_to_world_area <- FALSE #Should the country level data be collapsed to world_area? This is not compatible with collapse_to_eu - you need to pick one
    #throw an error if user set the above 2 variables both to TRUE (you can't have it both ways)
    if(collapse_to_eu == TRUE & collapse_to_world_area == TRUE) {
      stop("Choose either collapse to EU or collapse to world area. Not both.")
    }
    
#define erratic products, erratic products will be excluded from seasonal adjustment and CVM analysis, note this will be updated automatically by the determine volatility script. 
#Files will also be produced in the outputs folder showing the list of newly discovered erratic series. If you are running the process for country or world area breakdown, and
#there were additional erratic series found in the EU/non-EU breakdown which has been run first, then make sure these are appended to this vector (this should be done automatically
#in determine volatility script).
#Note that for trade flows with individual countries that have been found to be volatile, these will be automatically excluded from CVM calculations, and you do not have to update anything.
#look for the partner_erratics vector, or the text file list of partner erratics in the outputs folder for a list of the countries/areas excluded.
    erratic <- c("66", "68", "79", "93", "97")

#CVM output options:
    #TRUE = every UK region (England, Scotland, Wales, NI and UK total) will be represented
    #FALSE = only UK overall and Scotland - use this if you want to see the results broken down by parter country:
    all_uk <- FALSE
      if(all_uk == FALSE){
        region_choice <- "UK and Scotland"
      }
      if(all_uk == TRUE){
        region_choice <- "regions and countries"
      }
    
    #Set this to true if you want to use seasonally adjusted values:
    use_sa <- TRUE
    #Do you also want to add a series with drink excluded (in addition to other exclusions)? This was added as there is interest in how trade is performing excluding whisky (put the best we can currently do is drink as the data isn't that granular)
    #this was in response to an ad-hoc request and probably generally is not required
    exclude_drink <- FALSE


#Destination/origin level options:
#only does anything if collapse_to_eu = FALSE, can still work with world_area as well as partner country
    #This option controls whether the trade flow breakdown for each individual country is present, if this is false, only breakdowns by EU, non-EU and international are shown, 
    #true = breakdown for every partner country. If using world_area rather than country, these should also be left as TRUE (just think of country as world_area)
    #NOTE: This will only work if all_uk is FALSE, otherwise the files are too unwieldy, and we only really need data for Scotland in this much detail anyway
    #If this option is TRUE only Scotland breakdown is produced (not overall UK as well)
    #This will produce csv output files with different names depending on whether TRUE/FALSE.
    country_breakdown <- TRUE
    #Is the country data also broken down into individual products (TRUE)? Or is it just overall trade to each country (FALSE)
    country_product_breakdown <- TRUE
    #Sometimes we want to look at SITC2 level results for a single destination, to do this, set up this script up so county_product_breakdown is TRUE and then enter that 
    #country name here, otherwise if you want all countries it is very important to leave this as FALSE! Note you have to spell the country name exactly as it occurs in the data
    single_country <- FALSE

#seasonal adjustment QA options:
    #this sets the threshold for differences (between the seasonally adjusted and original series) we think might be large enough to warrant investigation. If the difference between
    #the original and seasonally adjusted values for a series is greater than this threshold and is greater than 1.5*IQR for the original series, it will be flagged for investigation.
    absolute_threshold <- 50000

#Visualisation options:
    file_date <- "2025-05-13"  #Enter the date the file was generated (IMPORTANT FOR SELECTING THE RIGHT INPUT)
    #comparison_year <- 2018 # this will control where the horizontal line for the average in the comparison year appears - not currently used
    save_switch <- TRUE #save the outputs?
    #Reindex to year - this will make all CVM values relative to this base year (NOTE THIS IS YEAR ENDING IN THE CURRENT QUARTER, NOT CALENDAR YEAR). Set to FALSE if you do not want to reindex
    reindex_year <- 2018
    


