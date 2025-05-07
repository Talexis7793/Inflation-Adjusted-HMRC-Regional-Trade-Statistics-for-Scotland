##### Overview ######

# This script has 5 sections
# - 1. Loading packages used in other scripts
# - 2. Creating function for product-parnter level PYP series
# - 3. Creating function for aggregating PYP series
# - 4. Creating function that takes PYP series and generate CVM series
# - 5. Creating function that takes CVM results in long format and plots a chart



## To do for script:
# - Seasonal adjustment
# - Add option to set a base year (separate function)


#-----------------------------------------------------------------------------------------------#

##### 1. Load Functions #####

 # Package loading
library(tidyverse)
library(lubridate)
library(readxl)
library(reshape2)
library(dplyr)
library(data.table)
library(readxl)
library(zoo)
library(seasonal)
library(seasonalview)
library(xts)
library(patchwork)
#library(uktrade)


#-----------------------------------------------------------------------------------------------#

##### 2. Generate Previous Year Prices (PYP) function #####

 # Function takes current price data and creates a PYP series for each product

# all <- fruits %>% expand(date, {{ grouping_vars }})
# df_year_vars <- data %>% dplyr::right_join(all,  by = setNames(colnames(all), colnames(all)))


generate_pyp <- function(data, 
                         date = "date", 
                         grouping_vars = c("country", "product"),
                         value = "value",
                         netmass = "netmass"){
  
  # rename the variables, this avoids us using data masking throughout "{{}}", and allows names to be a bit more flexible.
  data <- data %>% rename("value" = {{value}},
                          "netmass"= {{netmass}},
                          "date" = {{date}})
  
  # check data is a dataframe
  print(colnames(data))
  datelist <- data %>% pull(date) %>% unique()
  data <- data %>% ungroup()
  
  df_all_groups <- as.data.frame(datelist)
  
  # create empty dataframe with all combinations of dates and grouping variables
  for(i in 1:length(grouping_vars)){
    df_temp <- data %>% pull({{ grouping_vars }}[i]) %>% unique() 
    df_all_groups <- merge(as.data.frame(df_all_groups), as.data.frame(df_temp), by = NULL)
    
    rm(df_temp)
  }
  colnames(df_all_groups) <- c("date", {{ grouping_vars }})
  
  # See methodology note, STEPS 1 AND 2:
  
  df_year_vars <-  as.data.frame(df_all_groups) %>%
    left_join(as.data.frame(data), by = setNames(colnames(df_all_groups), colnames(df_all_groups))) %>% 
     replace(., is.na(.), 0) %>%
     mutate(year = substr(as.character(date), 0,4)) %>%
     group_by(across({{grouping_vars}}), year) %>%
     mutate(valueY = sum(value), 
            quantY = sum(netmass),
            lagyear = as.character(as.numeric(year)-1),
            CYPlink = sum(value)/4) %>% #sum since potential issues with missing values and we always want to divide by 4
    ungroup()
   df_year_vars[is.na(df_year_vars)] <- 0
  
  minyear <- min(df_year_vars$year)
   
  df_pyp <- df_year_vars %>%
    left_join(df_year_vars %>%
                 select(all_of(grouping_vars), year, valueY, quantY, CYPlink) %>%
                 unique(),
               by = c("lagyear" = "year",  setNames({{grouping_vars}}, {{grouping_vars}})),
               suffix = c("", ".lag")) %>%
    mutate(PYP = netmass*(valueY.lag / quantY.lag))  %>%
    replace(., is.na(.), 0) %>%
    replace(sapply(., is.infinite), 0) %>%
    group_by(across({{grouping_vars}}), year)%>%
    filter(lagyear >= minyear) %>% ungroup()
  
  return(df_pyp)
}

 # This helps with above function diagnosis

   # current_price_values <- read_csv("rts_scotland_cvm_ready.csv")
   # grouping_vars = c("country","ProductCode")
   # value = "valuei"
   # mass = "netmass"
   # date = "date"
 

#-----------------------------------------------------------------------------------------------#

##### 3. Aggregate PYP and Current Year Price link (CYP) function #####

 # Function that takes PYP series, filters them based on product or country, then aggregates to a higher level

aggregate_pyp <- function(data, 
                           filter_on, 
                           filter_to, 
                           group_vars = c("country", "product"), 
                           value = "value", 
                           netmass = "netmass",
                           CYPlink = "CYPlink",
                           CYPlink.lag = "CYPlink.lag",
                           PYP = "PYP"
                          ){
  # rename the variables, this avoids us using data masking throughout "{{}}", and allows names to be a bit more flexible.
  # function will drop any grouping variables not used, since these will be aggregated over.
  data <- data %>% rename("value" = {{value}},
                          "netmass"= {{netmass}},
                          "CYPlink" = {{CYPlink}},
                          "CYPlink.lag" = {{CYPlink.lag}},
                          "PYP" = {{PYP}})
  
  # See methodology note, STEP 3:
  
  aggregated_pyp_df <- data %>%
    ungroup() %>% 
    filter(.data[[ filter_on ]] %in% filter_to) %>%
    group_by(across({{group_vars}})) %>%
    mutate( PYP = sum(PYP), 
            CYPlink = sum(CYPlink), 
            CYPlink.lag = sum(CYPlink.lag), 
            value = sum(value),
            netmass = sum(netmass)) %>%
    ungroup() %>%
    select(all_of({{ group_vars }}), year, PYP, CYPlink, lagyear, CYPlink.lag, value, netmass) %>%
    unique() 
  
  return(aggregated_pyp_df)
}


#-----------------------------------------------------------------------------------------------#

##### 2. Generate Chained Volume Measure (CVM) (and Implied Deflators, IDEF) function #####

 # Function that takes aggregated PYP series and creates chained volume measure series
 # Also has an option to create Implied Price Deflator series (IDEFs = I-mplied DEF-lators)

generate_cvm <- function(data,
                         date = "date",
                         group_vars = c("country", "product_aggregation"),
                         year = "year", #make optional,
                         CYPlink = "CYPlink",
                         lagyear = "lagyear",
                         PYP_var = "PYP",
                         CYPlink.lag = "CYPlink.lag",
                         value = "value",
                         deflators = TRUE, 
                         CVM_index_to = 100 #TA - added this argument, setting this to value will mean that the CVM series is calculated relative to the value (£) in the base period rather than using 100.
                         ){

  # rename the variables, this avoids us using data masking throughout "{{}}", and allows names to be a bit more flexible.
  data <- data %>% 
    rename(
      "date" = {{date}},
      "year" = {{year}},
      "value" = {{value}},
      "CYPlink" = {{CYPlink}},
      "CYPlink.lag" = {{CYPlink.lag}},
      "PYP" = {{PYP_var}})
  
  # generate some basic sets used in the calculations
  cvm_datelist <- data %>% pull(date) %>% unique() %>% sort()
  cvm_yearlist <- data %>% pull(year) %>% unique() %>% sort()
  base_year <- min(cvm_yearlist)
  
  ## See methodology note, STEP 4:
  # start with setting the CVM = 100 in all periods. this will be edited in the following loops
  
  cvm_df <- data %>%
    mutate(CVM = {{CVM_index_to}}) %>% ungroup() %>%
    arrange(across(c({{group_vars}}, date)))
  
  ## See methodology note, STEP 5:
  # calculate the CVM in the base year
  for(t in 2:4){
    date_temp = cvm_datelist[t]
    year_temp = substr(date_temp, 0 ,4)
    
    cvm_df <- cvm_df %>%
    mutate(CVM = case_when(date == date_temp ~ (PYP/lag(PYP))*lag(CVM), 
                           TRUE ~ CVM ))  
  }
  
  ## See methodology note, STEP 6:
  # calculate CVM in subsequent years, requiring the calculation of the average CVM of the previous year
  for(y in 2:length(cvm_yearlist)){
    year_temp <- cvm_yearlist[y]
    avgCVM_df = cvm_df %>% filter(.data[[year]] ==  cvm_yearlist[y-1]) %>% 
      group_by(across({{group_vars}})) %>%
      mutate(avgCVMlag = sum(CVM)/4) %>% 
      ungroup() %>%
      select(year, all_of({{group_vars}}), avgCVMlag)  %>%
      unique()
    
    cvm_df <- cvm_df %>% 
      group_by(across({{group_vars}})) %>% 
      left_join(avgCVM_df, by = setNames(c(year, {{group_vars}}),  c(lagyear, {{group_vars}}))) %>%
      ungroup() %>%
      mutate(CVM = case_when(year == year_temp ~ (PYP/CYPlink.lag)*avgCVMlag, 
                             TRUE ~ CVM )) %>%
      select(-c(avgCVMlag))
  }
  
  ## See methodology note, STEP 7:
  # if delfators == TRUE, creates implied price deflator series
  if(deflators == TRUE){
    CP_base_year <- cvm_df %>% ungroup() %>% 
      filter(date == min(cvm_datelist)) %>%
      rename("value_base" = {{value}}) %>%
      select(all_of({{group_vars}}), value_base)
    
    cvm_df <- cvm_df %>% 
      left_join(CP_base_year, 
                by = setNames({{group_vars}}, {{group_vars}})) %>%
      arrange(across(c({{group_vars}}, date))) %>%
      group_by(across({{group_vars}})) %>%
      mutate(
        value_index = (100*value/value_base),
        implied_deflator = 100*value_index/CVM)
  }
  
  return(cvm_df)
}

# # testing vars
# year = "year"
# data = pyp_series_all
# group_vars = c("country", "flowtype", "partner", "agg")
# date = "date"
# CYPlink = "CYPlink"
# lagyear = "lagyear"
# PYP_var = "PYP"
# CYPlink.lag = "CYPlink.lag"
# value = "value"
# deflators = TRUE


#-----------------------------------------------------------------------------------------------#


##### 4. Single plot function #####

# Function that takes long form data and plots a standardised line charts with Pandemic and TCA lines
# Able to choose country, flowtype, variable, partner and aggregation plotted. 
# If you want to plot multiple lines, include a vector of choices eg. partner = c("EU, Non EU) and set the legend_choice = "partner"

ggplot_line_chart <- function(data, 
                              country_pick, 
                              flowtype_pick, 
                              variable_pick, 
                              agg_pick, 
                              partner_pick, 
                              legend_choice, 
                              index_year,
                              min_year = 2017,
                              save = FALSE) {

  single_plot_data <- cvm_series %>% 
    filter(year >= min_year) |> 
    rename("IDEF" = "implied_deflator") %>%
    mutate(Value = 100*value / value_base) %>%
    select("date", "ye_currq", "country", "partner", "flowtype", "agg", "CVM", "Value", "IDEF", "year") %>%
    reshape2::melt(id.vars = c("date", "ye_currq", "country", "partner", "flowtype", "agg", "year")) %>%
    filter(country %in% country_pick) %>%
    filter(flowtype %in% flowtype_pick) %>%
    filter(variable %in% variable_pick) %>%
    filter(agg %in% agg_pick) %>%
    filter(partner %in% partner_pick) %>% 
    mutate(month = month(date),
           quarter = case_when(month == 1 ~ "Q1",
                               month == 4 ~ "Q2",
                               month == 7 ~ "Q3",
                               month == 10 ~ "Q4"),
           yearq = paste0(year, " ", quarter),
           q_labels = case_when(quarter == "Q4" ~ yearq, # add labels for 4th quarters only to avoid label overlap
                                TRUE ~ "")) %>% 
    select(-date, -month)

  single_plot_data_reindexed <- single_plot_data  %>%
    left_join(single_plot_data %>%
                filter(ye_currq==index_year) %>% #1.8.24 indexing to ye_currq to make consistent with tables (the CVM value in the base year should match the CP value in that period)
                group_by(country, agg, flowtype, variable, partner) %>%
                mutate(value_base = sum(value)/4) %>% 
                select(country, agg, flowtype, partner, variable, value_base) %>%
                unique(),
              by = c("agg" = "agg", "country" = "country", "flowtype" = "flowtype", "partner" = "partner", "variable" = "variable")) %>%
    mutate(value = 100*value / value_base) %>% 
    ungroup() %>%
    select(yearq, ye_currq, country, flowtype, agg, partner, variable, value, q_labels) 
  
  Legend <- single_plot_data_reindexed[,legend_choice] # which variable to have separate lines on the plot 
  
  title_country <- single_plot_data_reindexed %>% pull(country) %>% unique() %>% paste( collapse = " & ")
  title_flowtype <- single_plot_data_reindexed %>% pull(flowtype) %>% unique() %>% paste( collapse = " & ")
  title_partner <- single_plot_data_reindexed %>% pull(partner) %>% unique() %>% paste( collapse = ",  ")
  title_aggregation <- single_plot_data_reindexed %>% pull(agg) %>% unique()
  title_variable <- single_plot_data_reindexed %>% pull(variable) %>% unique() %>% paste( collapse = ",  ")
  
  if(title_variable == "CVM"){
    axis_name = paste0("Chained Volume Measure (", as.character(index_year), " = 100)")
  }else if(title_variable == "IDEF"){
    axis_name = paste0("Implied Price Deflator (", as.character(index_year), " = 100)")
  }else if(title_variable == "CVM,  Value"){
    axis_name = paste0("Chained Volume Measure & Current Prices (", as.character(index_year), " = 100)")
  }else{
    axis_name =  paste0("Current Prices (", as.character(index_year), " = 100)")
  } 
  if(title_partner == "World"){title_partner = ""}else{title_partner = paste0(title_partner, " ")}
  
  
  # mean_y <- single_plot_data_reindexed  %>%
  #   filter(year == comparison_year) %>%
  #   summarize(mean(value)) %>% 
  #   pull()
  
  #Define the time to plot the line for data collection changes following EU exit
if(flowtype_pick[1] == "Exports" & is.na(flowtype_pick[2])) {
  time_intrastat_change <- "2020 Q4"
  annotation_location <- "2020 Q4"
  annotation_text <- "Data \ncollection\nchanges\nfollowing\nBrexit"
  annotation_height <- 17
}

if(flowtype_pick[1] == "Imports" & is.na(flowtype_pick[2])) {
  time_intrastat_change <- "2021 Q4"
  annotation_location <- "2021 Q2"
  annotation_text <- "Data \ncollection\nchanges\nfollowing\nBrexit"
  annotation_height <- 17
}

  #This option is for total Trade, or when exports and imports are displayed on the same chart probably not used in formal publications but just in case
  #There would be too many lines to make interpretation possible if including the breaks in time series for both exports and imports, so just highlight the end of UK-EU transition period instead 
if(!is.na(flowtype_pick[2]) | flowtype_pick[1] == "Trade") { #If a vector containing 2 values is entered in flowtype_pick or trade then assume this is for both exports and imports
  time_intrastat_change <- "2020 Q4"
  annotation_location <- c("2020 Q4")
  annotation_text <- "End of \nUK-EU\nTransition\nPeriod"
  annotation_height <- 15
}
  
#Create plot
  trade_plot <- single_plot_data_reindexed %>% 
    ggplot(aes(x = yearq, 
               y = value,
               group = Legend,
               colour = Legend)) +
    coord_cartesian(ylim = c(0, 150)) +
    geom_line(linewidth = 1.5) +
    scale_colour_discrete_sg() + #use SG colours
    scale_x_discrete(breaks = single_plot_data_reindexed$q_labels,
                     labels = single_plot_data_reindexed$q_labels) +
    theme_sg() +
    
    #COVID line:
    geom_vline(xintercept = c("2020 Q1"), linetype = 'dashed') +
    
    #Brexit line:
    #geom_vline(xintercept = c("2020 Q4"), linetype = 'dashed') +
    geom_vline(xintercept = time_intrastat_change, linetype = 'dashed') +
    
    #War in Ukraine line:
    geom_vline(xintercept = c("2022 Q1"), linetype = 'dashed') +
    
    #Annotations:
    annotate("text", x = "2020 Q2", y = 3, label = "COVID-19\npandemic", size  = 6) +
    #annotate("text", x = "2021 Q1", y = 15, label = "End of \nUK-EU\nTransition\nPeriod", size  = 6) +
    annotate("text", x = annotation_location, y = annotation_height, label = annotation_text, size  = 6, hjust = 0.1) +
    annotate("text", x = "2022 Q2", y = 3, label = "Invasion of\nUkraine", size  = 6) +
    labs(x = NULL,
         y = NULL,
      #   caption = "HM Revenue and Customs (HMRC) data collection changes following Brexit have affected statistics on UK trade in goods with the EU, resulting in\na structural break from January 2021.",
         size = 10) +
         # title = paste0("Quarterly ", title_partner, title_flowtype, " - ", str_to_title(title_aggregation))) + 
  #         caption =
  #           "NOT OFFICIAL STATISTICS
  # Source: Scottish Government analysis based on HMRC OTS (UK) and RTS (Scotland)
  # Note: Excludes erratic products 66, 68, 79, 93, 97") +
    theme(
      text = element_text(size = 20),
      plot.title = element_text(size = 20),
      plot.caption = element_text(hjust=0),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 15),
      legend.title = element_blank()
    ) 
  
  # if( prod(1- duplicated(single_plot_data_reindexed$yearq)) == 1){
  #   trade_plot = trade_plot + 
  #     geom_hline(yintercept = mean_y,
  #                linetype = "dashed") +
  #     annotate("text", x = "2017 Q2", y = (mean_y+5), label = c(paste0(comparison_year, " Average"))) 
  # }


  if(save == TRUE){
    ggsave(
      paste0("3) outputs/",title_country, " ", title_variable, " ", title_partner, " ", title_flowtype, " - ", str_to_title(title_aggregation), ".png"),
      width = 15,
      height = 6,
      dpi = 1000
    )
    #save an svg version as well for uploading to bloomreach
    ggsave(
      paste0("3) outputs/",title_country, " ", title_variable, " ", title_partner, " ", title_flowtype, " - ", str_to_title(title_aggregation), ".svg"),
      width = 15,
      height = 6,
      dpi = 1000
    )
  }
  
  trade_plot
  
}
