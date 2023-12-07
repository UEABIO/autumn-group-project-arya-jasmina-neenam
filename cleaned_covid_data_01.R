#LOAD PACKAGES
library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)


# IMPORT DATA ----
covid <- read_csv("data/owid-covid-data.csv")
head(covid)

#CLEAN DATA----
covid <- janitor::clean_names(covid) # clean the column names

colnames(covid) # quickly check the new variable names

covid <- rename(covid,
                   "AFR"="OWID_AFR")
                  
glimpse(covid)

# check for duplicate rows in the data
covid %>% 
  duplicated() %>% # produces a list of TRUE/FALSE statements for duplicated or not
  sum() # sums all the TRUE statements                                                                                                                                

summary(covid) # summarises data
