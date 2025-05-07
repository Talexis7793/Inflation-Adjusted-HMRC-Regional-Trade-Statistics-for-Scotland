##### CVM Calculations

# Calculate PYP Values
##Need:
#Annual Quantity and Value figures
#Quarterly quantity figures


#### CVM Calculations ####

pyp_datelist <- current_price_values %>% pull(date) %>% unique()
pyp_productlist <- current_price_values %>% pull(ProductCode) %>% unique()
pyp_countrylist <-  current_price_values %>% pull(country) %>% unique()
pyp_set <- merge(merge(as.data.frame(pyp_datelist), as.data.frame(pyp_productlist)), as.data.frame(pyp_countrylist)) 

####Calculate PYP values####
pyp0 <-  pyp_set %>%
  rename("date" = "pyp_datelist", "ProductCode" = "pyp_productlist","country" = "pyp_countrylist") %>%
  left_join(current_price_values, 
            by = c("date" = "date", "ProductCode" = "ProductCode", "country" = "country")) %>% 
  replace(., is.na(.), 0) %>%
  mutate(., year = substr(as.character(date), 0,4)) %>%
  group_by(country, ProductCode, year) %>%
  mutate(valueY = sum(valuei), quantY = sum(netmass), 
         lagyear = as.character(as.numeric(year)-1),
         CYPlink = sum(valuei)/4) #sum since potential issues with missing values and we always want to divide by 4
pyp0[is.na(pyp0)] <- 0

minyear <- min(pyp0$year)

#Calculate values in previous year's prices to create chains of Q1-Q2-Q3-Q4
pyp00 <- pyp0 %>%
  left_join(pyp0 %>% 
               select(country, ProductCode, year, valueY, quantY, CYPlink) %>%
               unique(),
             by = c("lagyear" = "year", "ProductCode" = "ProductCode", "country" = "country"), 
             suffix = c("", ".lag")) %>%
  mutate(PYP = netmass*(valueY.lag / quantY.lag)) %>%
  replace(., is.na(.), 0) %>%
  replace(sapply(., is.infinite), 0)%>%
  #filter(PYP != Inf) %>% ungroup() %>%
  group_by(country, ProductCode, year) %>%
  select(date, country, ProductCode, valuei, netmass, year, valueY, quantY, lagyear, CYPlink, valueY.lag, quantY.lag,  CYPlink.lag, PYP) %>%
  filter(lagyear >= minyear)


#At this point, before aggregation, can do some QA checks on whether
  #any country's products see huge swings in PYP values which will feed into the aggregations
pyp_qa <- pyp00 %>% group_by(country, ProductCode) %>%
  mutate(swing = (PYP / lag(PYP))- 1)


#filter product aggregation - here you could do anything you want, for now I am sticking to removing products 66, 68 and 79 which are erratic in ONS erratic series 
#May also want to consider removing SITC1 level 9 products since they have some weird behaviors and are often NAs, but are relatively small.

all_products <- current_price_values %>% pull(ProductCode) %>% unique()
erratic <- c("66","68", "79", "97")
sitc3 <- all_products[which(substr(all_products, 0,1) == "3")] # MIGHT NEED TO ADD MORE
all_excl_erratic_sitc3 <- all_products[!all_products %in% c(erratic, sitc3, "NAvalue")]


###### aggregate PYPs #####

## To remove EU and NON EU:
  
pyp00_country_tot <- pyp00 %>%
              ungroup() %>% rename("agg" = "ProductCode") %>%
              mutate(country = case_when(
                  grepl("Imp.", country) == TRUE ~ "Total Imports",
                    TRUE ~ "Total Exports")) %>%
              group_by(country, agg, date) %>%
              mutate( PYP = sum(PYP), 
                      CYPlink = sum(CYPlink), 
                      CYPlink.lag = sum(CYPlink.lag), 
                      valuei = sum(valuei),
                      netmass = sum(netmass)) %>%
              select(date, country, year, PYP, CYPlink, lagyear, CYPlink.lag, agg, valuei, netmass) %>%
              unique() 

#aggregate to sitc1

pyp000 <- pyp00_country_tot %>%
  ungroup() %>%
  filter(agg %in% all_excl_erratic_sitc3) %>%
  group_by(country, date) %>%
  mutate(valuei = sum(valuei), 
         netmass = sum(netmass), 
         PYP = sum(PYP), 
         CYPlink = sum(CYPlink),  
         CYPlink.lag = sum(CYPlink.lag), 
         agg = "Total excl. erratic & SITC3") %>%
  select(date, country, year, PYP, CYPlink, CYPlink.lag, lagyear, agg, valuei, netmass) %>%
  unique()
  

pyp000 <- rbind(pyp000, pyp00_country_tot %>% 
    ungroup() %>%
    filter(agg %in% all_excl_erratic_sitc3) %>%
    group_by(date) %>%
    mutate(valuei = sum(valuei), 
           netmass = sum(netmass), 
           PYP = sum(PYP), 
           CYPlink = sum(CYPlink),  
           CYPlink.lag = sum(CYPlink.lag), 
           agg = "Total excl. erratic & SITC3",
           country = "Total Trade") %>%
    select(date, country, year, PYP, CYPlink, CYPlink.lag, lagyear, agg, valuei, netmass) %>%
    unique())


pyp000 <- rbind(pyp000, pyp00_country_tot %>% ungroup() %>%
  filter(!agg %in% c("79")) %>%
  mutate(agg = substr(agg,0,1)) %>%   
  group_by(country, agg, date) %>%
  mutate(valuei = sum(valuei), 
         netmass = sum(netmass), 
         PYP = sum(PYP), 
         CYPlink = sum(CYPlink),  
         CYPlink.lag = sum(CYPlink.lag)) %>%
  select(date, country, year, PYP, CYPlink, CYPlink.lag, lagyear, agg, valuei, netmass) %>%
  unique())

pyp000 <- rbind(pyp000, pyp00 %>% ungroup() %>%
  rename("agg" = "ProductCode") %>%
  filter(agg %in% all_excl_erratic_sitc3) %>%
  group_by(date, country) %>%
  mutate(valuei = sum(valuei), 
         netmass = sum(netmass), 
         PYP = sum(PYP), 
         CYPlink = sum(CYPlink),  
         CYPlink.lag = sum(CYPlink.lag), 
         agg = "Total excl. erratic & SITC3") %>%
  select(date, country, year, PYP, CYPlink, CYPlink.lag, lagyear, agg, valuei, netmass) %>%
    unique()
  )
                
# pyp000 <- rbind(pyp000, (pyp00 %>%
#   ungroup() %>%
#   filter(ProductCode %in% sitc3) %>%
#   group_by(country, date) %>%
#   mutate(PYP = sum(PYP), CYPlink = round(sum(CYPlink),0), agg = "SITC3") %>%
#   select(date, country, year, PYP, CYPlink, lagyear, agg) %>%
#   unique()))

## quick qa

product_date_counts <- pyp00  %>%
  ungroup() %>%
  group_by(date, country) %>%
  summarise(n=n())


####Calculate CVMs####

cvm_datelist <- pyp000 %>% pull(date) %>% unique() %>% sort()
cvm_yearlist <- pyp000 %>% pull(year) %>% unique()%>% sort()
base_year <- min(cvm_yearlist)

# base quarter = 100
cvm00 <- pyp000 %>%
  mutate(CVM = 100) %>% 
  ungroup() %>% 
  arrange(country, agg, date)

# base year CVM
for(t in 2:4){
  date_temp = cvm_datelist[t]
  year_temp = substr(date_temp, 0 ,4)

  cvm00 <- cvm00 %>%
  mutate(CVM = case_when(date == date_temp ~ (PYP/lag(PYP))*lag(CVM), TRUE ~CVM )) #
}

#subsequent year CVM
for(y in 2:length(cvm_yearlist)){
  year_temp <- cvm_yearlist[y]
  avgCVM_df = cvm00 %>% filter(year ==  cvm_yearlist[y-1]) %>% 
    group_by(country, agg) %>%
    mutate(avgCVMlag = sum(CVM)/4) %>% ungroup() %>%
    select(year, country, agg, avgCVMlag)  %>%
    unique()
  print(avgCVM_df %>% filter(country == "EU Gross Exp.", agg == "Total excl. erratic & SITC3") %>% pull(avgCVMlag))
  
  cvm00 <- cvm00 %>% 
    group_by(year, country, agg) %>% 
    left_join(avgCVM_df, by = c("lagyear" = "year", "agg" = "agg", "country" = "country")) %>%
    mutate(CVM = case_when(year == year_temp ~ (PYP/CYPlink.lag)*avgCVMlag, TRUE ~ CVM )) %>%
    select(-c(avgCVMlag))
}

# construct price deflators

CP_base_year <- cvm00 %>% ungroup() %>% filter(date == min(cvm_datelist))  %>%
  select(country, agg, valuei)

cvm00_deflators <- cvm00 %>% 
  left_join(CP_base_year, 
            by = c("country" = "country", "agg"= "agg"),
            suffix = c("", ".base")) %>%
  arrange(country, agg, date) %>% group_by(country, agg) %>%
  mutate(implied_deflator = 100*(100*valuei/valuei.base)/CVM)

clipr::write_clip(
  cvm00_deflators %>%  filter(agg == "Total excl. erratic & SITC3") %>%
    ungroup() %>%
    select(date, country, CYPlink) %>%
    pivot_wider(names_from = country, values_from = CYPlink))

clipr::write_clip(
  pyp00 %>% filter(country == "Non EU Gross Exp.") %>% ungroup() %>%
    select(date, ProductCode, PYP) %>%
    pivot_wider(names_from = date, values_from = PYP))


write_csv(cvm00_deflators, "3) cvm_outputs/CVM_rts_totals_sitc1.csv")

# 
# clipr::write_clip(spread(cvm00_deflators %>%
#                            mutate(flow_product = paste(country, agg, sep= " ")) %>%
#                            ungroup() %>%
#                            select(date, flow_product, implied_deflator),
#                          flow_product, implied_deflator))
