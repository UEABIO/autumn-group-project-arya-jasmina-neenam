
# PACKAGES ----
library(tidyverse)
library(janitor) 
library(lubridate) 
library(scales)
library(patchwork)


#__________________________----
# IMPORT DATA ----
covid <- read_csv ("data/owid-covid-data.csv")
head(covid)

#__________________________----
# CHECK DATA----
# check the data
colnames(covid)
covid <- janitor::clean_names(covid) 
colnames(covid) # quickly check the new variable names
glimpse(covid) # displays column names in console
colnames(covid)

# check for duplicates---
duplicated(covid)# produces a list of TRUE/FALSE statements for duplicated or not
sum() # sums all the TRUE statements                                                                                                                                
covid %>% 
  is.na() %>% #checks for N/A values in data
  sum()
summary(covid) # summaries data
filter(.data = covid, continent %in% c("north america", "south america"))

