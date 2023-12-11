
# PACKAGES ----
library(tidyverse) # tidy data packages
library(janitor) # cleans variable names
library(lubridate) # make sure dates are processed properly

#__________________________----
# IMPORT DATA ----
covid <- read_csv ("data/owid-covid-data.csv")
head(covid) # check the data has loaded, prints first 10 rows of dataframe


#__________________________----
# CHECK DATA----
# check the data
colnames(covid)
covid <- janitor::clean_names(covid)




