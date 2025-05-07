##### None of this stuff is being used, but could be useful in the future #####

### Annotation without boundaries
annotate("text", single_plot_data$date[c(25, 29)], y= 140, label=c("Pandemic", "EU Exit")) +
  

#####  Synthetic control visualisations ######

## Create series for plots. Some basic synthetic controls
all_countries <- cvm00 %>% pull(country) %>% unique()
eu_countries <- all_countries[substr(all_countries, str_length(all_countries)-2, str_length(all_countries)) == "EUi"]
euro_area <- c("Latvia -EUi", "Lithuania -EUi", "France -EUi", "Germany -EUi", "Italy -EUi", "Spain -EUi", "Netherlands -EUi", "Belgium -EUi", "Austria -EUi", "Greece -EUi", "Portugal -EUi", "Finland -EUi", "Ireland -EUi", "Slovakia -EUi", "Slovenia -EUi", "Luxembourg -EUi", "Estonia -EUi", "Cyprus -EUi", "Malta -EUi")
G7 <-  c("Canada -EU", "France -EUi", "Germany -EUi", "Italy -EUi", "Japan -EU", "United States -EU")

other_advanced_econ <-  c( "Andorra -EU",
                           "Australia -EU",
                           "Czech Republic -EUi",
                           "Denmark -EUi",
                           "Hong Kong SAR China -EU",
                           "Iceland -EU",
                           "Israel -EU",
                           "Korea -EU",
                           "Macao SAR China -EU",
                           "New Zealand -EU",
                           "Norway -EU",
                           "Puerto Rico -EU",
                           "San Marino -EU",
                           "Singapore -EU",
                           "Sweden -EUi",
                           "Switzerland -EU",
                           "Taiwan -EU")

advanced_economies <- c(euro_area, G7, other_advanced_econ) %>% unique()
small_independents <- c("Ireland -EUi", "Switzerland -EU", "Norway -EU", "Denmark -EUi", "Netherlands -EUi", "Sweden -EUi", "Austria -EUi", "Iceland -EU", "Belgium -EUi", "Finland -EUi")

## Plot 
scotland_cvm <- cvm00 %>% ungroup() %>% filter(country =="EU", agg == "Total excl. erratic & SITC3") %>% select(date, agg, CVM, country) %>% mutate(country = "Scotland")

adv_avg_cvm <- cvm00 %>% ungroup() %>% filter(country %in% advanced_economies,!country %in% c("Malta -EUi", "Cyprus -EUi", "Netherlands -EUi", "Macao SAR China -EU"), agg == "Total excl. erratic & SITC3") %>%
  group_by(date) %>% mutate(CVM = mean(CVM)) %>%
  select(date, agg, CVM) %>% unique() %>% mutate(country = "Advanced Economies") 

g7_avg_cvm <- cvm00 %>% ungroup() %>% filter(country %in% G7, agg == "Total excl. erratic & SITC3") %>%
  group_by(date) %>% mutate(CVM = mean(CVM)) %>%
  select(date, agg, CVM) %>% unique() %>% mutate(country = "G7") 

euro_avg_cvm <- cvm00 %>% ungroup() %>% filter(country %in% euro_area, !country %in% c("Malta -EUi", "Cyprus -EUi", "Netherlands -EUi", "Macao SAR China -EU"), agg == "Total excl. erratic & SITC3") %>%
  group_by(date) %>% mutate(CVM = mean(CVM)) %>%
  select(date, agg, CVM) %>% unique() %>% mutate(country = "Euro Area") 

eu_avg_cvm <- cvm00 %>% ungroup() %>% filter(country %in% eu_countries,!country %in% c("Malta -EUi", "Cyprus -EUi", "Netherlands -EUi", "Macao SAR China -EU"), agg == "Total excl. erratic & SITC3") %>%
  group_by(date) %>% mutate(CVM = mean(CVM)) %>%
  select(date, agg, CVM) %>% unique() %>% mutate(country = "European Union") 

small_inde_avg_cvm <- cvm00 %>% ungroup() %>% filter(country %in% small_independents, agg == "Total excl. erratic & SITC3") %>%
  group_by(date) %>% mutate(CVM = mean(CVM)) %>%
  select(date, agg, CVM) %>% unique() %>% mutate(country = "Small Independent Countries") 

cvm_blocs <- rbind(scotland_cvm,adv_avg_cvm, g7_avg_cvm, euro_avg_cvm, eu_avg_cvm, small_inde_avg_cvm)
#View( cvm00 %>% ungroup() %>% filter(country %in% advanced_economies, agg == "Total excl. erratic & SITC3"))

cvm_blocs_2019base <- cvm_blocs %>% ungroup() %>% 
  filter(date == "2019-10-01") %>%
  group_by(country, agg) %>% rename(base = CVM) %>%
  right_join(cvm_blocs, by = c("agg" = "agg", "country" = "country"), suffix = c(".b", "")) %>%
  mutate(CVM = CVM*100 / base )


ggplot() + geom_line(data = cvm_blocs %>% mutate(date = zoo::as.yearqtr(date, format = "%YQ%q")) , 
                     aes(x = date, y = CVM, colour = country)) + theme_classic()

ggplot() + geom_line(data = cvm_blocs_2019base %>% mutate(date = zoo::as.yearqtr(date, format = "%YQ%q")) , 
                     aes(x = date, y = CVM, colour = country)) + theme_classic()


ggplot() + geom_line(data = cvm00 %>% filter(agg == "Total excl. erratic & SITC3", substr(country, str_length(country)-2, str_length(country)) == "EUi") %>% mutate(date = zoo::as.yearqtr(date, format = "%YQ%q")) , 
                     aes(x = date, y = CVM, colour = country))  +  ylim(50, 300) + theme_classic()

ggplot() + geom_line(data = cvm00 %>% filter(agg == "Total excl. erratic & SITC3") %>% mutate(date = zoo::as.yearqtr(date, format = "%YQ%q")) , 
                     aes(x = date, y = CVM, colour = country))  +  ylim(50, 150) + theme_classic() 

#-----------------------------------------------------------------------------------------#


###### A) First load in data. #####
# Data should be in a dataframe with the columns including:
#  - date, value, mass, grouping variables (eg. country, products, flow direction or anything else desired, in a c("a","b", "c", ...) format.
#  - you can specify the names of those columns when you call function, otherwise it will default to those above
#  - currently only works with quarterly data. 

pyp_disagg_ukscot<- generate_pyp(current_price_uk_scotland,
                                 date = "date", 
                                 grouping_vars = c("country","partner", "ProductCode", "flowtype"), 
                                 value = "value", 
                                 mass = "netmass") 


pyp_disagg_ukscot<- generate_pyp(current_price_uk_scotland %>% filter(date< "2022-10-01") %>%
                                   mutate(group = paste(country, partner, ProductCode, flowtype, sep = "_")), 
                                 date = "date", 
                                 grouping_vars = c("group"), #  c("country","partner", "ProductCode", "flowtype"), 
                                 value = "value", 
                                 mass = "netmass") %>% ungroup() %>% select(-group)

pyps_many <- pyp_disagg_ukscot %>% ungroup() %>% select(PYP) 
pyps_few <- pyp_test %>% select(PYP) 
pyps_many %>% filter(PYP %in% pyps_few)
pyps_few %>% filter(PYP %in% pyps_many)


##### B) Aggregate how you like (unfinished) #####
# - key options: aggregate to total imports/exports, and total trade. Aggregate to SITC1 level.
# - Should set up a loop with the different aggregations you want.



all_products <- current_price_uk_scotland %>% pull(ProductCode) %>% unique()
erratic <- c("66","68", "79", "97")
sitc3 <- all_products[which(substr(all_products, 0,1) == "3")] # MIGHT NEED TO ADD MORE
all_excl_79 <- all_products[!all_products %in% c("79")]
all_excl_erratic_sitc3 <- all_products[!all_products %in% c(erratic, sitc3, "NAvalue")]


agg_functest <- pyp_test %>%
  aggregate_pyps(filter_on = "ProductCode",
                 filter_to = all_excl_erratic_sitc3,
                 group_vars = c("date", "country", "partner", "flowtype")) %>%
  mutate(agg = "Total excl. sitc3 & erratic") 


agg_functest2 <- pyp_disagg_ukscot %>% 
  aggregate_pyps(filter_on = "ProductCode",
                 filter_to = all_excl_erratic_sitc3,
                 group_vars = c("date", "country", "partner", "flowtype")) %>% 
  mutate(agg = "Total excl. sitc3 & erratic")

View(
  agg_functest2 %>% ungroup() %>% filter(!CYPlink.lag %in% (agg_functest$CYPlink.lag))
)
View(
  agg_functest %>% ungroup() %>% filter(!CYPlink.lag %in% (agg_functest2$CYPlink.lag))
)

total_trade_functest <- pyp_test %>% 
  aggregate_pyps(filter_on = "ProductCode", filter_to = all_excl_erratic_sitc3, group_vars = c("date", "country")) %>%
  mutate(agg = "Total excl. sitc3 & erratic", partner = "World", flowtype = "Total")

flow_functest <- pyp_test %>%
  aggregate_pyps(filter_on = "ProductCode", 
                 filter_to = all_excl_erratic_sitc3, 
                 group_vars = c("date", "country", "flowtype")) %>%
                 mutate(partner = "World")

sitc1_aggregate <- pyp_test %>% mutate(sitc1 = substr(ProductCode,0,1)) %>%
  aggregate_pyps(filter_on = "ProductCode", 
                 filter_to = all_excl_79, 
                 group_vars = c("date", "country", "partner", "flowtype", "sitc1"))%>%
  rename(agg = sitc1)

sitc1_totals <- pyp_test %>% mutate(sitc1 = substr(ProductCode,0,1)) %>%
  aggregate_pyps(filter_on = "ProductCode", 
                 filter_to = all_excl_79, 
                 group_vars = c("date", "country", "flowtype", "sitc1")) %>%
  rename(agg = sitc1)

pyp_aggregate <- rbind(total_trade_functest, flow_functest, sitc1_aggregate, sitc1_totals)

pyp_aggregate <- pyp_disagg_ukscot %>%
  ungroup() %>% rename("agg" = "ProductCode") %>%
  filter(agg %in% all_excl_erratic_sitc3) %>%
  group_by(country, partner, flowtype, date) %>%
  mutate( PYP = sum(PYP), 
          CYPlink = sum(CYPlink), 
          CYPlink.lag = sum(CYPlink.lag), 
          value = sum(value),
          netmass = sum(netmass),
          agg = "Total excl. sitc3 & erratic") %>%
  select(date, country, partner, flowtype, year, PYP, CYPlink, lagyear, CYPlink.lag, agg, value, netmass) %>%
  unique() 

pyp_aggregate <- rbind(pyp_aggregate,
                       pyp_disagg_ukscot %>%
                         ungroup() %>% rename("agg" = "ProductCode") %>%
                         filter(agg %in% all_excl_erratic_sitc3) %>%
                         group_by(country, flowtype, date) %>%
                         mutate( PYP = sum(PYP), 
                                 CYPlink = sum(CYPlink), 
                                 CYPlink.lag = sum(CYPlink.lag), 
                                 value = sum(value),
                                 netmass = sum(netmass),
                                 partner = "World",
                                 agg = "Total excl. sitc3 & erratic") %>%
                         select(date, country, partner, flowtype, year, PYP, CYPlink, lagyear, CYPlink.lag, agg, value, netmass) %>%
                         unique() 
                       )
pyp_aggregate <- rbind(pyp_aggregate,
                       pyp_disagg_ukscot %>%
                         ungroup() %>% rename("agg" = "ProductCode") %>%
                         filter(agg %in% all_excl_erratic_sitc3) %>%
                         group_by(country, date) %>%
                         mutate( PYP = sum(PYP), 
                                 CYPlink = sum(CYPlink), 
                                 CYPlink.lag = sum(CYPlink.lag), 
                                 value = sum(value),
                                 netmass = sum(netmass),
                                 partner = "World",
                                 flowtype = "Total",
                                 agg = "Total excl. sitc3 & erratic") %>%
                         select(date, country, partner, flowtype, year, PYP, CYPlink, lagyear, CYPlink.lag, agg, value, netmass) %>%
                         unique() 
                       )



###### C) Generate chained value series ######
# - Column names will default to those generated by previous function, but you can specify different column names if desired


cvm_raw_series <- generate_cvm(pyp_aggregate, group_vars = c("country", "partner", "flowtype", "agg"), value = "value", deflators = TRUE)

write_csv(cvm_raw_series, "3) cvm_outputs/CVM_UKandScotland Q3 2022.csv")
