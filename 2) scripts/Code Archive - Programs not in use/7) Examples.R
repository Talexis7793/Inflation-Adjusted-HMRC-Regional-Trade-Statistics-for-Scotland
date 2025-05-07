
## This creates a total trade CVM series for UK and Scotland ##

source("2) scripts/1) Packages.R")
source("2) scripts/2) Data.R")

 # Create PYP values

pyp_series_disagg <- generate_pyp(
  current_price_uk_scotland,
  date = "date", 
  grouping_vars = c("country", "flowtype", "partner", "ProductCode"), 
  value = "value", 
  mass = "netmass") 


 # create product groups

all_products <- current_price_uk_scotland %>% pull(ProductCode) %>% unique()
erratic <- c("66","68", "79", "93", "97")
sitc3 <- all_products[which(substr(all_products, 0,1) == "3")]
all_excl_erratic_sitc3 <- all_products[!all_products %in% c(erratic, sitc3, "NAvalue")]

 # Aggregate to desired product group

pyp_series_t_trade <- pyp_series_disagg %>% 
  ungroup() %>%
  aggregate_pyp(
    value = "value",
    filter_on = "ProductCode",
    filter_to = all_excl_erratic_sitc3,
    group_vars = c("date", "country")) %>%
  mutate(
    agg = "Total excl. sitc3 & erratic",
    flowtype = "Trade",
    partner = "World") 

 # generate CVM series

cvm_series <- generate_cvm(
  pyp_series_t_trade, 
  group_vars = c("country", "flowtype", "partner", "agg"), 
  value = "value", 
  deflators = TRUE)

