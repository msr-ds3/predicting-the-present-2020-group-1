library(scales)
library(tidyverse)
library(lubridate)
library(broom)
library(modelr)
library(readxl)
library(caret)
library(zoo)

#Trends data
suvs_trends <- read_csv("trucks_suvs_2004_2020.csv")
insurance_trends <- read_csv("auto_insurance_2004_2020.csv")
# rename
names1<- names(suvs_trends)
names2 <- names(insurance_trends)
suvs_trends <- suvs_trends %>% 
  rename(suvs = names1[2],
         Period = Month)
insurance_trends <- insurance_trends %>% 
  rename(insurance = names2[2], 
         Period = Month)

# join trends data
trends_full <- left_join(insurance_trends, suvs_trends, by = "Period") %>% 
  mutate(Period = as.Date(as.yearmon(Period, "%Y-%m"))) 

# load the sales data
sales <- read_excel("sales.xls")
# Period has type char in the data, convert that to yearmonth 
ym <- as.yearmon(sales$Period, "%b-%Y")
# use as.Date() to convert the type of ym to date
sales$Period <- as.Date(ym)



with_trends_full <- sales %>% 
  left_join(trends_full, by = "Period") %>% 
  rename(sales = Value) %>% 
  mutate(sales = as.numeric(sales)) %>% 
  filter(!(is.na(sales)))

save(with_trends_full, file = "sales_trends.RData")