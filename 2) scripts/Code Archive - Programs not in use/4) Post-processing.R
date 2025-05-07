##### 4) Post-Processing #####

##### Seasonality ######

#charts show clear signs of seasonality
ggplot(data = cvm_raw_series %>% filter(agg == "Total excl. erratic & SITC3")) +
  geom_line(mapping = aes(x = date, y = CVM, colour = country)) +
  theme_classic()

ggplot(data = cvm_raw_series %>% filter(agg == "Total excl. erratic & SITC3", grepl("Imp", country))) +
  geom_line(mapping = aes(x = date, y = CVM, colour = country)) +
  theme_classic()

ggplot(data = cvm_raw_series %>% filter(agg == "Total excl. erratic & SITC3", grepl("Exp", country))) +
  geom_line(mapping = aes(x = date, y = CVM, colour = country)) +
  theme_classic()

#Use functions from the "seasons" package to adjust series

#a. choose one series
cvm_deseasoned_test_ready <- cvm_raw_series %>%
  filter(country == "Total Exports") %>%
  select(date, CVM) %>%
  # mutate(year = substr(date, 0,4),
  # quarter = ceiling(as.numeric(substr(date, 6,7))/3)) %>%
   #select(-date) %>% 
  read.zoo() %>% ts(frequency = 4, start = c(2014,1))


   #spread(quarter, CVM) %>% 
  
cvm_deseasoned <- seas(x = cvm_deseasoned_test_ready)
quarterplot(cvm_deseasoned)
view(cvm_deseasoned)  

cvm_deseasoned <- seas(x = cvm_deseasoned_test_ready,
      arima.model = "(1 1 1)(0 1 1)",
      regression.aictest = NULL,
      outlier = NULL,
      transform.function = "none")



seas(
  x = cvm_deseasoned_test_ready,
  regression.aictest = "td",
  x11 = "",
  outlier.critical = 3
)
  