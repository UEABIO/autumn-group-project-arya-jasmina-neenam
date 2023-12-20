
# PACKAGES ----
library(tidyverse) # tidy data packages
library(janitor) # cleans variable names
library(lubridate) # make sure dates are processed properly
library(scales)
library(patchwork)


#__________________________----
# IMPORT DATA ----
covid <- read_csv ("data/owid-covid-data.csv")
head(covid) # check the data has loaded, prints first 10 rows of dataframe


#__________________________----
# CHECK DATA----
# check the data
colnames(covid)


covid <- janitor::clean_names(covid) # clean the column names

colnames(covid) # quickly check the new variable names

#glimpse(covid) # displays column names in console

colnames(covid)
# check for duplicate rows in the data


duplicated(covid)# produces a list of TRUE/FALSE statements for duplicated or not
sum() # sums all the TRUE statements                                                                                                                                

covid %>% 
  is.na() %>% #checks for N/A values in data
  sum()

summary(covid) # summaries data

filter(.data = covid, continent %in% c("north america", "south america"))

