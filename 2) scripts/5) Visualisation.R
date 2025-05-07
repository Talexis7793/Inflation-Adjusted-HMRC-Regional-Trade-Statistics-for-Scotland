##### Visualisation #####

# Basic graphs for use in Powerpoint

# Template
# remotes::install_github(
#   "DataScienceScotland/sgplot",
#   upgrade = "never"
# )

###########################################################################################
#User input required: visualisation:
use_sa <- TRUE #Use seasonally adjusted data?
all_uk <- FALSE #Use the breakdown for each UK nation or just UK v Scotland?
file_date <- "2025-03-17"  #Enter the date the file was generated (IMPORTANT FOR SELECTING THE RIGHT INPUT)
data_date = "2023Q4" #Enter the latest quarter you are working with (keep format to yyyyqq)
quarter_want = "Q4" #This is just the current quarter
#comparison_year <- 2018 # this will control where the horizontal line for the average in the comparison year appears - not currently used
save_switch = TRUE

#Reindex to year - this will make all CVM values relative to this base year (NOTE THIS IS YEAR ENDING IN THE CURRENT QUARTER, NOT CALENDAR YEAR). Set to FALSE if you do not want to reindex
reindex_year <- year_want

#Used in labels:
erratic <- c("66","68", "79", "93", "97")
#############################################################################################

library(tidyverse)
library(sgplot)
#############################################################################################

#Read in data           
if(use_sa == FALSE) {
  
  if(all_uk == FALSE){
    cvm_series <- read_csv(paste0("3) outputs/CVM - UK & Scotland ",data_date, " (Generated ", file_date,").csv"))
    cvm_series_sitc2 <- read_csv(paste0("3) outputs/CVM SITC2 - UK & Scotland ",data_date, " (Generated ", file_date,").csv"))
  }
  
  if(all_uk == TRUE) {
    cvm_series <- read_csv(paste0("3) outputs/CVM - all UK ",data_date, " (Generated ", file_date,").csv"))
    cvm_series_sitc2 <- read_csv(paste0("3) outputs/CVM SITC2 - all UK ",data_date, " (Generated ", file_date,").csv"))
  }
}

#Here we use the seasonally adjusted values, and simply rename them to be compatible with the rest of the code 
if(use_sa == TRUE) {
  
  if(all_uk == FALSE){
    cvm_series <- read_csv(paste0("3) outputs/CVM - SA - UK & Scotland ",data_date, " (Generated ", file_date,").csv"))
    cvm_series_sitc2 <- read_csv(paste0("3) outputs/CVM SITC2 - SA - UK & Scotland ",data_date, " (Generated ", file_date,").csv"))
  }
  
  if(all_uk == TRUE){
    cvm_series <- read_csv(paste0("3) outputs/CVM - SA - all UK ",data_date, " (Generated ", file_date,").csv"))
    cvm_series_sitc2 <- read_csv(paste0("3) outputs/CVM SITC2 - SA - all UK ",data_date, " (Generated ", file_date,").csv"))
  }
}

#Load chart functions
source("./2) scripts/1) Packages.R")

#############################################################################################

# -------------------- Plots
  
ggplot_line_chart(single_plot_data, 
                  country_pick = c("Scotland"),
                  flowtype_pick = "Imports",  #Enter "Exports", "Imports", "Trade" or c("Exports", "Imports")
                  variable_pick = "CVM",
                  agg_pick = "Total excl. sitc3 & erratic",
                  partner_pick = c("World"),
                  index_year = reindex_year,
                  legend_choice = "country",
                  save = save_switch)

ggplot_line_chart(single_plot_data, 
                  country_pick = c("Scotland"),
                  flowtype_pick = "Exports", 
                  variable_pick = "CVM",
                  agg_pick = "Total excl. sitc3 & erratic",
                  partner_pick = c("World"),
                  index_year = reindex_year,
                  legend_choice = "country",
                  save = save_switch)

ggplot_line_chart(single_plot_data, 
                  country_pick = c("Scotland"),
                  flowtype_pick = "Exports", 
                  variable_pick = "CVM",
                  agg_pick = "Total excl. sitc3, erratic & drink",
                  partner_pick = c("World"),
                  index_year = reindex_year,
                  legend_choice = "country",
                  save = save_switch)

ggplot_line_chart(single_plot_data, 
                  country_pick = c("Scotland"),
                  flowtype_pick = c("Exports", "Imports"), 
                  variable_pick = "IDEF",
                  agg_pick = "Total excl. sitc3 & erratic",
                  partner_pick = c("World"),
                  index_year = reindex_year,
                  legend_choice = "flowtype",
                  save = save_switch)


ggplot_line_chart(single_plot_data, 
                  country_pick = c("Scotland"),
                  flowtype_pick = "Trade", 
                  variable_pick = "CVM",
                  agg_pick = "Total excl. sitc3 & erratic",
                  partner_pick = c("World"),
                  index_year = reindex_year,
                  legend_choice = "country",
                  save = save_switch)

ggplot_line_chart(single_plot_data, 
                  country_pick = c("Scotland"),
                  flowtype_pick = "Imports", 
                  variable_pick = "CVM",
                  agg_pick = "Total excl. sitc3 & erratic",
                  partner_pick = c("EU", "Non EU"),
                  index_year = reindex_year,
                  legend_choice = "partner",
                  save = save_switch)

ggplot_line_chart(single_plot_data, 
                  country_pick = c("Scotland"),
                  flowtype_pick = "Exports", 
                  variable_pick = "CVM",
                  agg_pick = "Total excl. sitc3 & erratic",
                  partner_pick = c("EU", "Non EU"),
                  index_year = reindex_year,
                  legend_choice = "partner",
                  save = save_switch)

ggplot_line_chart(single_plot_data, 
                  country_pick = c("UK", "Scotland"),
                  flowtype_pick = "Exports", 
                  variable_pick = "Value",
                  agg_pick = "Total excl. sitc3 & erratic",
                  partner_pick = c("World"),
                  index_year = reindex_year,
                  legend_choice = "country",
                  save = save_switch)

ggplot_line_chart(single_plot_data, 
                  country_pick = c("UK", "Scotland"),
                  flowtype_pick = "Exports", 
                  variable_pick = "CVM",
                  agg_pick = "Total excl. sitc3, erratic & drink",
                  partner_pick = c("World"),
                  index_year = reindex_year,
                  legend_choice = "country",
                  save = save_switch)

ggplot_line_chart(single_plot_data, 
                  country_pick = c("UK", "Scotland"),
                  flowtype_pick = "Exports", 
                  variable_pick = "CVM",
                  agg_pick = "Total excl. sitc3 & erratic",
                  partner_pick = c("World"),
                  index_year = reindex_year,
                  legend_choice = "country",
                  save = save_switch)

ggplot_line_chart(single_plot_data, 
                  country_pick = c("Scotland"),
                  flowtype_pick = "Exports", 
                  variable_pick = c("CVM","Value"),
                  agg_pick = "Total excl. sitc3 & erratic",
                  partner_pick = c("World"),
                  index_year = reindex_year,
                  legend_choice = "variable",
                  save = save_switch)

 # tests
ggplot_line_chart(single_plot_data, 
                  country_pick = c("UK", "Scotland"),
                  flowtype_pick = "Exports", 
                  variable_pick = "Value",
                  agg_pick = "Total excl. sitc3 & erratic",
                  partner_pick = c("Non EU"),
                  index_year = reindex_year,
                  legend_choice = "country",
                  save = FALSE)

ggplot_line_chart(single_plot_data, 
                  country_pick = c("Scotland"),
                  flowtype_pick = c("Exports") ,
                  variable_pick = c("Value"),
                  agg_pick = c("1"),
                  partner_pick = c("EU", "Non EU"),
                  index_year = reindex_year,
                  legend_choice = "partner",
                  save = FALSE)

all_sitc_1 <- c("0","1","2","3","4","5","6","7","8","9")

# ggplot_line_chart(single_plot_data, 
#                   country_pick = c("UK"),
#                   flowtype_pick = "Exports", 
#                   variable_pick = "CVM",
#                   agg_pick = all_sitc_1,
#                   partner_pick = c("World"),
#                   index_year = reindex_year,
#                   legend_choice = "agg",
#                   save = FALSE)
#----------------------------------------------------------------------------#

#I haven't made these charts presentable as they probably won't go in the report
##### UK-Scotland SITC1 Charts #####

product_names <- read_csv("1) data/Product_Names.csv") %>% mutate(Product_Code = as.character(Product_Code))

sitc_chart_df <- cvm_series %>% 
  rename("IDEF" = "implied_deflator") %>%
  rename(Value = value_index) %>%
  select("date", "year", "country", "partner", "flowtype", "agg", "CVM", "Value", "IDEF") %>%
  reshape2::melt(id.vars = c("date", "year", "country", "partner", "flowtype", "agg")) %>%
  filter(
    variable == "IDEF",
    #country =="Scotland",
    partner == "World",
    flowtype == "Exports",
    agg != 9, 
    agg != "N",
    agg != "Total excl. sitc3 & erratic") %>%
  left_join(product_names, by = c("agg" = "Product_Code")) %>%
  mutate(Product = case_when(is.na(Product) ~ agg, TRUE ~ Product))


# Re-index to base year

sitc_chart_reindexed_df <- sitc_chart_df  %>%
  left_join(sitc_chart_df %>%
              filter(year==reindex_year) %>%
              group_by(country, agg, flowtype, partner, variable) %>%
              mutate(value_base = sum(value)/4) %>% 
              select(country, agg, flowtype, partner, variable, value_base) %>%
              unique(),
            by = c("agg" = "agg", "country" = "country", "flowtype" = "flowtype", "partner" = "partner", "variable" = "variable")) %>%
  mutate(value = 100*value / value_base,
         date = as.Date(date, "%d/%m/%Y")) %>% 
  ungroup() %>%
  select(date, country, flowtype, Product, partner, variable, value)

# Set up wording for plot


legend <- sitc_chart_reindexed_df$country # which variable to have separate lines on the plot 
wrap_var <- sitc_chart_reindexed_df$Product

title_flowtype_sitc <- sitc_chart_reindexed_df %>% pull(flowtype) %>% unique() %>% paste( collapse = " & ")
title_partner_sitc <- sitc_chart_reindexed_df %>% pull(partner) %>% unique() %>% paste( collapse = ",  ")
title_variable_sitc <- sitc_chart_reindexed_df %>% pull(variable) %>% unique() %>% paste( collapse = ",  ")

if(title_partner_sitc == "World"){title_partner_sitc = ""}else{title_partner_sitc = paste0(title_partner_sitc, " ")}

if(title_variable_sitc == "CVM"){
  axis_name_sitc = paste0("Chained Volume Measure (", as.character(reindex_year), " = 100)")
}else if(title_variable_sitc == "IDEF"){
  axis_name_sitc = paste0("Implied Price Deflator (", as.character(reindex_year), " = 100)")
}else{
  axis_name_sitc =  paste0("(", as.character(reindex_year), " = 100)")
} 


# Plot series

sitc_chart <- sitc_chart_reindexed_df %>%
  ggplot(aes(x = date, y = value, colour = legend) ) +
  geom_line() +
  facet_wrap(vars({{wrap_var}}), scales = "free_y") +
  #coord_cartesian(ylim = c(0, NA)) +
  labs( x = "Date",  y = axis_name_sitc,
        title = paste0("Quarterly ", axis_name_sitc, " ", title_flowtype_sitc), 
        caption =
          paste0("NOT OFFICIAL STATISTICS
Source: Scottish Government based on HMRC OTS (UK) and RTS (Scotland)
Note: Erratic goods are SITC2 products:", paste(erratic, collapse = ", "))) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 20),
    plot.caption = element_text(hjust=0),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 15))

sitc_chart

ggsave(
  paste0("3) outputs/sitc_chart.png"),
  width = 15,
  height = 6,
  dpi = 1000
)
#----------------------------------------------------------------------------#


##### Partner-Flowtype CVM vs CP#####

product_names <- read_csv("1) data/Product_Names.csv") %>% mutate(Product_Code = as.character(Product_Code))

cpcvm_chart_df <- cvm_series %>% 
  rename("IDEF" = "implied_deflator") %>%
  mutate(Value = 100*value / value_base) %>%
  select("date", "year", "country", "partner", "flowtype", "agg", "CVM", "Value", "IDEF") %>%
  reshape2::melt(id.vars = c("date", "year", "country", "partner", "flowtype", "agg")) %>%
  filter(
    variable != "IDEF",
    country =="Scotland",
    partner != "World",
    flowtype != "Trade",
    agg != 9, 
    agg != "N",
    agg == "Total excl. sitc3 & erratic") %>%
  left_join(product_names, by = c("agg" = "Product_Code")) %>%
  mutate(Product = case_when(is.na(Product) ~ agg, TRUE ~ Product))


# Re-index to base year

cpcvm_chart_reindexed_df <- cpcvm_chart_df  %>%
  left_join(cpcvm_chart_df %>%
              filter(year==reindex_year) %>%
              group_by(country, agg, flowtype, partner, variable) %>%
              mutate(value_base = sum(value)/4) %>% 
              select(country, agg, flowtype, partner, variable, value_base) %>%
              unique(),
            by = c("agg" = "agg", "country" = "country", "flowtype" = "flowtype", "partner" = "partner", "variable" = "variable")) %>%
  mutate(value = 100*value / value_base,
         date = as.Date(date, "%d/%m/%Y")) %>% 
  ungroup() %>%
  select(date, country, flowtype, Product, partner, variable, value)

# Set up wording for plot


legend <- cpcvm_chart_reindexed_df$variable # which variable to have separate lines on the plot 
wrap_var <- paste( cpcvm_chart_reindexed_df$partner, cpcvm_chart_reindexed_df$flowtype, sep = " ")

title_flowtype_cpcvm <- cpcvm_chart_reindexed_df %>% pull(flowtype) %>% unique() %>% paste( collapse = " & ")
title_partner_cpcvm <- cpcvm_chart_reindexed_df %>% pull(partner) %>% unique() %>% paste( collapse = ",  ")
title_variable_cpcvm <- cpcvm_chart_reindexed_df %>% pull(variable) %>% unique() %>% paste( collapse = ",  ")

if(title_partner_cpcvm == "World"){title_partner_cpcvm = ""}else{title_partner_cpcvm = paste0(title_partner_cpcvm, " ")}

if(title_variable_cpcvm == "CVM"){
  axis_name_cpcvm = paste0("Chained Volume Measure (", as.character(reindex_year), " = 100)")
}else if(title_variable_cpcvm == "IDEF"){
  axis_name_cpcvm = paste0("Implied Price Deflator (", as.character(reindex_year), " = 100)")
}else{
  axis_name_cpcvm =  paste0("(", as.character(reindex_year), " = 100)")
} 


# Plot series

cpcvm_chart <- cpcvm_chart_reindexed_df %>%
  ggplot(aes(x = date, y = value, colour = legend) ) +
  geom_line() +
  facet_wrap(vars({{wrap_var}})) +
  coord_cartesian(ylim = c(0, NA)) +
  labs( x = "Date",  y = axis_name_cpcvm,
        title = paste0("Quarterly ", axis_name_cpcvm, " ", title_flowtype_cpcvm), 
        caption =
          paste0("NOT OFFICIAL STATISTICS
Source: Scottish Government based on HMRC OTS (UK) and RTS (Scotland)
Note: Erratic goods are SITC2 products:", paste(erratic, collapse = ", "))) +
  theme_bw() +
  theme(plot.title = element_text(size = 20),
    plot.caption = element_text(hjust=0),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 15))

cpcvm_chart

ggsave(
  paste0("3) outputs/cp_cvm_charts_by_region_&_flow.png"),
  width = 15,
  height = 6,
  dpi = 1000
)

#----------------------------------------------------------------------------#


#### Scotland - UK share #### - not really used


scot_uk_share_df <- cvm_series %>% ungroup() %>%
  filter(agg =="Total excl. sitc3 & erratic",
         partner == "World") %>%
  select(date, year, country, flowtype, agg, value, CVM) %>%
  pivot_wider(
    id_cols = c(date, year, flowtype), 
    names_from = 'country', 
    values_from = c(value, CVM), 
    names_glue = '{country}.{.value}'
  )

base_scot_uk <- scot_uk_share_df %>%
  filter(substr(date, 0, 4) == reindex_year) %>%
  select(-date) %>%
  group_by(flowtype) %>%
  summarise_at(vars(Scotland.value:UK.CVM), mean, na.rm = TRUE)

scot_uk_share_df <- scot_uk_share_df %>%
  left_join(base_scot_uk,
            by = c("flowtype" = "flowtype"),
            suffix = c("", "_base")) %>%
  mutate(
    Scotland_cvm = Scotland.CVM*100 / Scotland.CVM_base,
    UK_cvm = UK.CVM*100 / UK.CVM_base,
    Scotland_cvm_gbp = Scotland.CVM * Scotland.value_base / 100,
    UK_cvm_gbp = UK.CVM * UK.value_base / 100,
    `Value Share` = 100*(Scotland.value / UK.value),
    `CVM Share` = 100 * (Scotland_cvm_gbp) / (UK_cvm_gbp)
  )


scot_uk_share_df %>% filter(flowtype == "Exports") %>% 
  mutate(#`Value Share` = stats::filter(`Value Share`, rep(1,3), sides = 1)/3,
    #`CVM Share` = stats::filter(`CVM Share`, rep(1,3), sides = 1)/3,
    Difference = `CVM Share` -  `Value Share`,
  ) %>%
  filter(!is.na(Difference)) %>%
  select(date, `Value Share`, `CVM Share`, Difference) %>%
  reshape2::melt( c("date")) %>%
  ggplot(aes(x = date, y = value, colour = variable)) +
  geom_line() +
  theme_bw() +
  labs( x = "Date", y = paste0("Share of UK (%), CVM in ", reindex_year, " Prices"),
        title ="Scotland Share of UK", 
        caption =
          "NOT OFFICIAL STATISTICS
Source: Scottish Government based on HMRC OTS (UK) and RTS (Scotland)
Note: Excludes SITC 3 (oil and gas) and erratic products 66, 68, 79, 93, 97") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 20),
    plot.caption = element_text(hjust=0),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 15))

ggsave(
  paste0("3) outputs/Scotland_share_of_uk_cvm_v_value.png"),
  width = 15,
  height = 6,
  dpi = 1000
)
